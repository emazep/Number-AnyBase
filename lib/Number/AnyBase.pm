# ABSTRACT: Converts decimals to and from any alphabet of any size (for shortening IDs, URLs etc.)

# no critic
package Number::AnyBase;
## use critic

use strict;
use warnings;

use base 'Class::Accessor::Faster';

use Carp qw(croak);

Number::AnyBase->mk_ro_accessors( qw/
    alphabet
    _inverted_alphabet
/);

sub new {
    my ($class, @in_alphabet) = @_;

    croak 'No alphabet passed to Number::AnyBase->new()'
        unless @in_alphabet;

    my $type = ref $in_alphabet[0];

    my $tmp_alphabet
        = $type eq 'ARRAY'
        ? $in_alphabet[0]
        : scalar @in_alphabet == 1
        ? [ split '', $in_alphabet[0] ]
        : \@in_alphabet;

    my %seen;
    my @normalized_alphabet;
    for ( @$tmp_alphabet ) {
        push @normalized_alphabet, $_ unless $seen{$_}++
    }

    croak 'The given alphabet must have at least two symbols'
        if @normalized_alphabet < 2;

    for (@normalized_alphabet) {
        croak 'Symbols in the given alphabet cannot be more than one character long'
            if length > 1
    }

    $class->fastnew(\@normalized_alphabet)
}

sub fastnew {
    my ($class, $alphabet) = @_;

    my %inverted_alphabet;
    @inverted_alphabet{ @$alphabet } = 0 .. $#{ $alphabet };

    $class->SUPER::new({
        alphabet           => $alphabet,
        _inverted_alphabet => \%inverted_alphabet
    })
}

sub new_bin {
    shift->fastnew( ['0', '1'] )
}

sub new_oct {
    shift->fastnew( ['0'..'7'] )
}

sub new_hex {
    shift->fastnew( ['0'..'9', 'A'..'F'] )
}

sub new_base36 {
    shift->fastnew( ['0'..'9', 'A'..'Z'] )
}

sub new_base62 {
    shift->fastnew( ['0'..'9', 'A'..'Z', 'a'..'z'] )
}

sub new_base64 {
    shift->fastnew( ['A'..'Z', 'a'..'z', '0'..'9', '+', '/'] )
}

sub new_base64url {
    shift->fastnew( ['A'..'Z', 'a'..'z', '0'..'9', '-', '_'] )
}

sub new_urisafe {
    shift->fastnew( ['-', '.', '0'..'9', 'A'..'Z', '_', 'a'..'z', '~'] )
}

sub new_ascii {
    shift->fastnew([
        '!', '"' , '#', '$', '%', '&', "'", '(', ')', '*', '+', '-', '.', '/',
        '0'..'9' , ':', ';', '<', '=', '>', '?', '@', 'A'..'Z',
        '[', '\\', ']', '^', '_', '`', 'a'..'z', '{', '|', '}', '~'
    ])
}

sub to_base {
    my ($self, $dec_num) = @_;

    my $alphabet      = $self->alphabet;
    my $alphabet_size = @{ $alphabet };

    my $base_num = '';
    use integer;
    do { $base_num .= $alphabet->[ $dec_num % $alphabet_size ] }
        while $dec_num /= $alphabet_size;

    return scalar reverse $base_num
}

sub to_dec {
    my $self = shift;
    my $reversed_base_num = reverse shift;

    my $inverted_alphabet = $self->_inverted_alphabet;
    my $alphabet_size     = @{ $self->alphabet };

    # Make $dec_num a bignum upon request.
    my $dec_num = defined $_[0] ? $_[0] * 0 : 0;

    #$base_num = reverse $base_num;
    $dec_num
        = $dec_num * $alphabet_size
        + $inverted_alphabet->{ chop $reversed_base_num }
            while length $reversed_base_num;

    return $dec_num
}

sub next {
    my ($self, $curr) = @_;

    my $alphabet          = $self->alphabet;
    my $alphabet_size     = @{ $alphabet };
    my $inverted_alphabet = $self->_inverted_alphabet;

    my $next_char_value;
    my $next_num = '';
    while ( length $curr ) {
        if (
            ( $next_char_value = $inverted_alphabet->{chop $curr} + 1 )
                < $alphabet_size
        ) {
            $next_num .= $alphabet->[$next_char_value];
            last
        } else {
            $next_num .= $alphabet->[0]
        }
    }

    $next_num .= $alphabet->[1] if $next_char_value >= $alphabet_size;

    return $curr . reverse $next_num
}

sub prev {
    my ($self, $curr) = @_;

    my $alphabet = $self->alphabet;
    return if $curr eq $alphabet->[0];

    my $inverted_alphabet = $self->_inverted_alphabet;

    my $prev_char_value;
    my $prev_num = '';
    while ( length $curr ) {
        if (
            ( $prev_char_value = $inverted_alphabet->{chop $curr} ) == 0
        ) {
            $prev_num .= $alphabet->[-1]
        } else {
            $prev_num .= $alphabet->[$prev_char_value - 1];
            last
        }
    }

    chop $prev_num if $prev_char_value == 1 && !length($curr);

    return $curr . reverse $prev_num
}

1;

__END__

=head1 SYNOPSIS

    use strict;
    use warnings;
    
    use Number::AnyBase;
    
    # 62 symbols alphabet
    my @alphabet = (0..9, 'A'..'Z', 'a'..'z');
    my $conv = Number::AnyBase->new(\@alphabet);
    my $base62_num = $conv->to_base(123456);     # W7E
    my $dec_num    = $conv->to_dec($base62_num); # back to 123456
    
    use feature 'say';
    
    # URI unreserved characters alphabet
    my $uri_conv = Number::AnyBase->new_urisafe;
    say $uri_conv->to_base(1234567890); # ~2Bn4
    say $uri_conv->to_dec( '~2Bn4' );   # 1234567890
    
    # ASCII printable characters alphabet
    my $ascii_conv = Number::AnyBase->new_ascii;
    say $ascii_conv->to_base(199_000_000_000); # >Z8X<8
    say $ascii_conv->to_dec( '>Z8X<8' );       # 199000000000
    
    # Hexadecimal base
    my $hex_conv = Number::AnyBase->new( 0..9, 'A'..'F' );
    say $hex_conv->to_base(2047);   # 7FF
    say $hex_conv->to_dec( '7FF' ); # 2047
    
    # Morse-like alphabet :-)
    my $morse_conv = Number::AnyBase->new( '_.' );
    say $morse_conv->to_base(99);         # ..___..
    say $morse_conv->to_dec( '..___..' ); # 99
    
    {
        # Unicode alphabet (webdings font);
        use utf8;
        binmode STDOUT, ':utf8';
        my $webdings_conv = Number::AnyBase->new(
            '♣♤♥♦☭☹☺☻✈✪✫✭✰✵✶✻❖♩♧♪♫♬⚓⚒⛔✼✾❁❂❄❅❊☿⚡⚢⚣⚤⚥⚦⛀⛁⛦⛨'
        );
        say $webdings_conv->to_base(1000000000); # ☺⚢♬♬⚥⛦
        say $webdings_conv->to_dec( '☺⚢♬♬⚥⛦' ); # 1000000000
    }
    
    # Fast native unary increment/decrement
    my $sequence = Number::AnyBase->fastnew(['A'..'Z']);
    say $sequence->next('ZZZ');  # BAAA
    say $sequence->prev('BAAA'); # ZZZ

=head1 DESCRIPTION

First the intended usage scenario: this module has been conceived to shorten 
ids, URLs etc., like the most popular URL shortening services do (think of 
TinyURL and similar).

Then a bit of theory: an id is (or can anyway be mapped to) just a number,
therefore it can be represented in any base. The longer is the alphabet of the
base, the shorter the number representation will be (in terms of symbols of the
said alphabet). This module converts any non-negative decimal integer (including
L<Math::BigInt>-compatible objects) to any given base/alphabet and vice versa,
thus giving the shortest possible representation for the original number/id (at
least for a I<collision-free> transformation).

The suggested workflow to shorten your ids is therefore the following:

=for :list
1. when storing an item in your data store, generate a decimal id for it (for example through the SEQUENCE field type offered by many DBMSs);
2. shorten the said decimal id through the L</to_base> method explained below;
3. publish the shortened id rather than the (longer) original decimal id.

When receiving a request for a certain item through its corresponding shortened
id you've published:

=for :list
1. obtain the corresponding original decimal id through the L</to_dec> method explained below;
2. retrieve the requested item in your data store through its original decimal id you've obtained at the previous step;
3. serve the requested item.

Of course one can also save the shortened id along with the item in the data
store, thus saving the C<to_dec> conversion at the step 1 above (using the
shortened id rather than the decimal one in the subsequent step 2).

Through the fast native unary increment/decrement offered by the L</next> and
L</prev> methods, it is even possible to skip the decimal ids generation and the
conversion steps altogether.

A couple of similar modules were already present on CPAN, but for one reason or
another I did not find them completely satisfactory: for a detailed explanation,
please see the L</COMPARISON> section below.

=head1 METHODS

=head2 Constructors

=head3 C<new>

=for :list
* C<< Number::AnyBase->new( @alphabet ) >>
* C<< Number::AnyBase->new( \@alphabet ) >>
* C<< Number::AnyBase->new( $alphabet ) >>

This is the constructor method, which initializes and returns the I<converter>
object. It requires an I<alphabet>, that is the set of symbols to represent the
converted numbers (the size of the base is the number of symbols of the provided
alphabet).

An exception is thrown if no alphabet is passed to C<new>.

The alphabet can be passed as a list or as a listref of characters, or packed
into a string (in which case the alphabet is obtained by splitting the string
into its individual characters).

For example the following three invocations return exactly the same object:

    $conv = Number::AnyBase->new( '0'..'9', 'a'..'z' );
    
    # Same as above
    $conv = Number::AnyBase->new( ['0'..'9', 'a'..'z'] );
    
    # The same through a string
    $conv = Number::AnyBase->new( '0123456789abcdefghijklmnopqrstuvwxyz' );

An alphabet must have at least two symbols (that is, at least two distinct
characters), otherwise an excpetion is thrown. Any duplicate character is
automatically removed, so for example:

    $conv = Number::AnyBase->new( 'a'..'z', '0'..'9' );
    
    # Exactly the same as above
    $conv = Number::AnyBase->new( 'a'..'z', '0'..'9', qw/a b c d z z z/ );
    
    # Error: an alphabet with a single symbol has been passed
    $conv = Number::AnyBase->new( 'aaaaaaaaaaaaaaaa' );

As a single symbol alphabet is not admissible, when C<new> is called with a
single (string) parameter, it is interpreted as a string containing the whole
alphabet and not as a list containing a single (multichar) symbol. In other
words, if you want to pass the alphabet as a list, it must contain at least two
elements.

The alphabet can't contain symbols longer than one character, otherwise an
exception is thrown. Note that this can happen only when the alphabet is passed
as a list or a listref, since when a (single) string is given to C<new>, the
alphabet is obtained by splitting the string into its individual characters (and
the possible duplicate characters are removed), so no multichar symbols are ever
created in this case:

    # Error: the last symbol in the provided alphabet (as a list) is two characters long
    Number::AnyBase->new( qw/z z z aa/ );
    
    # This is instead correct since the alphabet will be: 'z', 'a'
    Number::AnyBase->new( 'zzzaa' );

=head3 C<fastnew>

=for :list
* C<< Number::AnyBase->fastnew( \@alphabet ) >>

This is an alternative, faster constructor, which skips all of the checks
performed by C<new> (if an illegal alphabet is passed, the behavior is
currently indeterminate).

It only accepts a listref.

=head3 Specialized Constructors

Several constructors with ready-made alphabets are offered as well.

=head4 C<new_urisafe>

It builds and returns a converter to/from an alphabet made by the I<unreserved
URI characters>, as per the L<RFC3986|http://www.ietf.org/rfc/rfc3986.txt>.
More precisely, it is the same as:

    Number::AnyBase->fastnew( ['-', '.', '0'..'9', 'A'..'Z', '_', 'a'..'z', '~'] );

=head4 C<new_base36>

The same as:

    Number::AnyBase->fastnew( ['0'..'9', 'A'..'Z'] );

=head4 C<new_base62>

The same as:

    Number::AnyBase->fastnew( ['0'..'9', 'A'..'Z', 'a'..'z'] );

=head4 C<new_base64>

The same as:

    Number::AnyBase->fastnew( ['A'..'Z', 'a'..'z', '0'..'9', '+', '/'] );

=head4 C<new_base64url>

The same as:

    Number::AnyBase->fastnew( ['A'..'Z', 'a'..'z', '0'..'9', '-', '_'] );

=head4 C<new_bin>

It builds a binary converter. The same as:

    Number::AnyBase->fastnew( ['0', '1'] );

=head4 C<new_oct>

It builds an octal converter. The same as:

    Number::AnyBase->fastnew( ['0'..'7'] )

=head4 C<new_hex>

It builds an hexadecimal converter. The same as:

    Number::AnyBase->fastnew( ['0'..'9', 'A'..'F'] );

=head4 C<new_ascii>

It builds and returns a converter to/from an alphabet composed of all the
printable ASCII characters except the space. More precisely, it is the same as:

    Number::AnyBase->fastnew([
        '!', '"' , '#', '$', '%', '&', "'", '(', ')', '*', '+', '-', '.', '/',
        '0'..'9' , ':', ';', '<', '=', '>', '?', '@', 'A'..'Z',
        '[', '\\', ']', '^', '_', '`', 'a'..'z', '{', '|', '}', '~'
    ]);

=head2 C<to_base>

=for :list
* C<< $string = $converter->to_base( $decimal ) >>

This is the method which transforms the given decimal number into its
representation in the new base, as shown in the L</SYNOPSIS> above.

It works only on decimal non-negative integers (including C<0>). For speed
reasons, no check is performed on the given number: in case it is illegal, the
behavior is currently indeterminate.

It works transparently also on L<Math::BigInt>-compatible objects (that is, any
object which overloads the arithmetic operators like L<Math::BigInt> does): just
pass any such I<big number> and you will get the correct result:

    use Math::BigInt; # Or use Math::GMP;
    Math::BigInt->accuracy(60); # For example
    
    my $bignum = Math::BigInt->new( '123456789012345678901234567890123456789012345678901234567890' ); # Or Math::GMP->new(...)
    
    my $conv = Number::AnyBase->new_base62;
    
    my $base_num = $conv->to_base( $bignum ); # sK0FUywPQsEhMwNhdPBZJcA9KumP0WpD0

This permits to freely choose any L<Math::BigInt> I<option> (the
I<accuracy>, as shown above, or the I<backend library> etc.), or to use any
other compatible class, such as, for example, L<Math::GMP>.

=head2 C<to_dec>

=for :list
* C<< $decimal_number = $converter->to_base( $base_num ) >>
* C<< $decimal_bignumber = $converter->to_base( $base_num, $bigint_obj ) >>

This is the method which converts the transformed I<number> (or rather
I<string>) back to its decimal representation, as exemplified in the
L</SYNOPSIS> above.

For speed reasons, no check is performed on the given string, which could be
inconsistent (for example because it contains characters not present in the
current alphabet): in this case the behavior is currently indeterminate.

It accepts a second optional parameter, which should be a
L<Math::BigInt>-compatible object (it does not matter if it is initialized or
not), which tells C<to_base> that a I<bignum> result is requested. It is
necessary only when the result is too large to be held by a native perl
integer (though, other than slowing down the conversion, it does not harm
anyway).

The passed bignum object is used for the internal calculations so, though
unusual, this interface permits to have the maximum flexibility, as it
completely decouples the I<bignum> library, permitting the user to freely choose
any L<Math::BigInt> I<option> as well as any (faster) L<Math::BigInt>-compatible
alternative (such as L<Math::GMP>):

    use Math::BigInt; # Or use Math::GMP;
    Math::BigInt->accuracy(60); # For example
    
    my $conv = Number::AnyBase->new_base62;
    
    my $big_dec_num = $conv->to_dec( 'sK0FUywPQsEhMwNhdPBZJcA9KumP0WpD0', Math::BigInt->new ); # Or Math::GMP->new
    # $big_dec_num is now a Math::BigInt object which stringifies to:
    # 123456789012345678901234567890123456789012345678901234567890

=head2 C<next>

=for :list
* C<< $string = $converter->next( $base_num ) >>

This method performs an optimized I<native> unary increment on the given
converted number/string, returning the next number/string in the current base
(see also the L</SYNOPSYS> above):

    $next_base_num = $converter->next($base_num);

It is over 2x faster than the conversion roundtrip:

    $next_base_num = $converter->to_base( $converter->to_dec($base_num) + 1 );

(see the F<benchmark/native_sequence.pl> benchmark included in the
distribution). It therefore offers an efficient way to get the next id from the
last (converted) id stored in a db, for example.

=head2 C<prev>

=for :list
* C<< $string = $converter->prev($base_num) >>

This method performs an optimized I<native> unary decrement on the given
converted number/string, returning the previous number/string in the current
base (see also the L</SYNOPSYS> above):

    $prev_base_num = $converter->prev($base_num);

It is over 2x faster than the conversion roundtrip:

    $prev_base_num = $converter->to_base( $converter->to_dec($base_num) - 1 );

When called on the I<zero> of the base, it returns C<undef>.

=head2 C<alphabet>

=for :list
* C<< $listref = $converter->alphabet >>

Read-only method which returns the alphabet of the current I<target> base, as a
listref.

=head1 COOKBOOK

=head2 Security

This module focuses only on converting numbers from decimals to any 
base/alphabet and vice versa, therefore it has nothing to do with security, 
that is, given a number/string and the alphabet it is represented on, the 
next (through an unary increment) number/string is guessable. If you want 
your (converted) id sequence not to be guessable, the solution is however 
simple: just randomize your decimal numbers upfront, leaving large random gaps 
in the set. Then feed the randomized decimals to this module to have them 
shortened.

=head2 Sorting

Characters ordering in the given alphabet does matter: if it is desidered that
converting a sorted sequence of decimals produces a sorted sequence of strings
(when properly padded of course), the characters in the provided alphabet must
be sorted as well.

An alphabet with unsorted characters can be used to make the converted
numbers somewhat harder to guess.

Note that the predefined constructors always use sorted alphabets.

=head2 Speed

For maximum speed, as a constructor use C<fastnew> or any of the predefined
constructors, resorting to C<new> only when it is necessary to perform the extra
checks.

Conversion speed maximization does not require any trick: as long as
I<big numbers> are not used, the calculations are performed at the full perl
native integers speed.

Big numbers of course slow down the conversions but, as shown above,
performances can be fine-tuned, for example by properly setting the
L<Math::BigInt> precision and accuracy, by choosing a faster back-end library,
or by using L<Math::GMP> directly in place of L<Math::BigInt> (advised).

As already said, the optimized native unary increment [decrement] provided by
C<next> [C<prev>] is over 2x faster than the C<to_dec>/C<to_base> conversion
rountrip. However, if a sequence of converted numbers must be generated, and
such sequence is large enough so that the first C<to_dec()> call can be
amortized, using C<to_base()> (only) is marginally faster than using C<next>:

    use Number::AnyBase;
    
    use constant SEQ_LENGTH => 10_000;
    
    my $conv = Number::AnyBase->new( 0..9, 'A'..'Z', 'a'..'z' );
    my (@seq1, @seq2);
    my $base_num = 'zzzzzz';
    
    # @seq1 construction through native increment:
    my $next = $base_num;
    push @seq1, $next = $conv->next($next) for 1..SEQ_LENGTH;
    
    # @seq2 construction through to_base is marginally faster than @seq1:
    my $dec_num = $conv->to_dec($base_num);
    push @seq2, $conv->to_base( $dec_num + $_ ) for 1..SEQ_LENGTH;

See the F<benchmark/native_sequence.pl> benchmark script included in the
distribution.

=head1 COMPARISON

Here is a brief and B<completely biased> comparison with L<Math::BaseCalc>,
L<Math::BaseConvert> and L<Math::Base::Convert>, which are similar CPAN modules.

For the performance claims, please see the
F<benchmark/other_cpan_modules.pl> benchmark script included in the
distribution. Also note that the coversion speed gaps tend to increase with the
numbers size.

=begin :list
 
* vs C<Math::BaseCalc>
 
=begin :list

* Pros

=begin :list
 
* C<Number::AnyBase> is faster: S<< decimal->base >> conversion is about 2x (100%) faster, S<< base->decimal >> conversion is about on par, C<fastnew> is about 20% faster than C<Math::BaseCalc::new>.
* S<< Base->decimal >> conversion in C<Number::AnyBase> can return C<Math::BigInt> (or I<similar>) objects upon request, while C<Math::BaseCalc> only returns native perl integers, thus producing wrong results when the decimal number is too large.
* C<Math::BaseCalc> lacks the fast native unary increment/decrement offered by C<Number::Anybase>, which permits an additional 2x speedup.
 
=end :list

* Cons

=begin :list

* C<Math::BaseCalc::new> converts also negative integers, while C<Number::AnyBase> only converts non-negative integers (this feature has been considered not particularly important and therefore traded for speed in C<Number::AnyBase>).

=end :list

=end :list

* vs C<Math::BaseConvert>

=begin :list

* Pros

=begin :list

* With native perl integers, C<Number::AnyBase> is hugely faster: something like 200x faster in S<< decimal->base >> conversion and 130x faster in S<< base->decimal >> conversion (using C<Math::BaseConvert::cnv>).
* With big integers (60 digits), C<Number::AnyBase> (using C<Math::GMP>) is still faster: over 13x faster in both S<< decimal->base >> conversion and S<< base->decimal >> conversion; though much less, it's faster even using C<Math::BigInt> with its pure-perl backend.
* C<Math::BaseConvert> has a weird API: first it has a functional interface, which is not ideal for code which has to maintain its internal state. Then, though a custom alphabet can be set (through a state-changing function called C<dig>), every time C<cnv> is called, the I<target> alphabet size must be passed anyway.
* C<Math::BaseConvert> doesn't permit to use a bignum library other than C<Math::BigInt>, nor it permits to set any C<Math::BigInt> option.
* C<Math::BaseConvert> lacks the fast native unary increment/decrement offered by C<Number::Anybase>, which permits an additional 2x speedup.

=end :list

* Cons

=begin :list

* C<Math::BaseConvert> manages big numbers transparently (but this makes it extremely slow and does not permit to use a library other than C<Math::BigInt>, as already said).
* C<Math::BaseConvert> can convert numbers between two arbitrary bases with a single call.
* C<Math::BaseConvert> converts also negative integers.

=end :list

=end :list

* vs C<Math::Base::Convert>

=begin :list

* Pros

=begin :list

* With native perl integers, C<Number::AnyBase> is largely faster: something like over 15x faster in S<< decimal->base >> conversion and over 22x faster in S<< base->decimal >> conversion (using the C<Math::Base::Convert> object API, which is the recommended one for speed); C<fastnew> is over 70% faster than C<Math::Base::Convert::new>.
* With big integers (60 digits), C<Number::AnyBase> (using C<Math::GMP>) is still faster: about 15% faster in S<< decimal->base >> conversion and about 100% faster in S<< base->decimal >> conversion.
* Though generally better, C<Math::Base::Convert> preserves some of the C<Math::BaseConvert> API shortcomings: to convert numbers bidirectionally between base 10 to/from another given base, two different objects must be istantiated (or the bases must be passed each time through the functional API).
* C<Math::Base::Convert> lacks the fast native unary increment/decrement offered by C<Number::Anybase>, which permits an additional 2x speedup.
* Possible minor glitch: some of the predefined alphabets offered by C<Math::Base::Convert> are not sorted.

=end :list

* Cons

=begin :list

* C<Math::Base::Convert> manages big numbers transparently and natively, i.e. without resorting to C<Math::BigInt> or similar modules (but, though not as pathologically slow as C<Math::BaseConvert>, this makes C<Math::Base::Convert> massively slow as well, when native perl integers can be used).
* On big integers, if C<Number::AnyBase> uses C<Math::BigInt> with its pure-perl engine, C<Math::Base::Convert> is faster: about 11x in S<< decimal->base >> conversion and about 6x in in S<< base->decimal >> conversion (as already said, C<Number::AnyBase> can however use C<Math::GMP> and be faster even with big numbers).
* C<Math::Base::Convert> can convert numbers between two arbitrary bases with a single call.
* C<Math::Base::Convert> converts also negative integers.

=end :list

=end :list

=end :list

All of the reviewed modules are I<pure-perled>, though the C<Math::GMP> module
that C<Number::AnyBase> can (optionally) use to maximize its speed with big
numbers it's not.
Note however that the C<Number::AnyBase> fast native unary increment/decrement
work even on big numbers without any external module.

=head1 SEE ALSO

=for :list
* L<Math::BaseCalc>
* L<Math::BaseConvert>
* L<Math::Base::Convert>

=head1 BUGS

No known bugs.

Please report any bugs or feature requests to C<bug-number-AnyBase at
rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Number-AnyBase>. I will be
notified, and then you'll automatically be notified of progress on your bug as I
make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Number::AnyBase

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Number-AnyBase>

=item * GitHub issues (you can also report bugs here)

L<https://github.com/emazep/Number-AnyBase/issues>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Number-AnyBase>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Number-AnyBase>

=item * Search CPAN

L<http://search.cpan.org/dist/Number-AnyBase/>

=back

=head1 ACKNOWLEDGEMENTS

Many thanks to the IPW (Italian Perl Workshop) organizers and sponsors: they
run a fascinating an inspiring event.

=cut
