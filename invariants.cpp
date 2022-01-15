                                                                                                                              /*
 Copyright Mykola Rabchevskiy 2022.
 Distributed under the Boost Software License, Version 1.0.
 (See http://www.boost.org/LICENSE_1_0.txt)

________________________________________________________________________________________________________________________________
                                                                                                                              */
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cmath>

#include <random>
#include <string>
#include <map>
#include <unordered_set>

#include "heapsort.h"
#include "range.h"
#include "stack.h"


constexpr unsigned RPN_CAPACITY{ 128 }; // :max capacity of posufix expression

std::random_device randomDevice;
std::mt19937 RANDOM( randomDevice() );

std::uniform_real_distribution< double > uniformRadius(    2.0,  20.0 );
std::uniform_real_distribution< double > uniformCoord ( -200.0, 200.0 );


char* append( char* s, const char& symbol ){
                                                                                                                              /*
  Append symbol to zero-terminated C-string:
                                                                                                                              */
  auto len = strlen( s );
  s[ len ] = symbol;
  s[ len + 1 ] = '\0';
  return s;
}
                                                                                                                              /*
Set of available semantic types (type denoted by single character):
                                                                                                                              */
enum class Sem: char {
  o = 'o',    // :undefned
  B = 'B',    // :boolean
  R = 'R',    // :real dimensionless number
  r = 'r',    // :ratio
  X = 'X',    // :X coordinate
  Y = 'Y',    // :Y coordinate
  T = 'T',    // :time point
  D = 'D',    // :duration
  x = 'x',    // :X distance
  y = 'y',    // :Y distance
  E = 'E',    // :Euclidean norm
  L = 'L',    // :distances
  U = 'U',    // :first  velocity coordinate
  V = 'V',    // :second velocity coordinate
};
                                                                                                                              /*
Textual description of semantic types:
                                                                                                                              */
const char* LEX( const Sem& semanticType ){
  switch( semanticType ){
    case Sem::o: return "undefined";
    case Sem::B: return "boolean";
    case Sem::R: return "real number";
    case Sem::r: return "ratio";
    case Sem::X: return "X coordinate";
    case Sem::Y: return "Y coordinate";
    case Sem::T: return "time point";
    case Sem::D: return "duration";
    case Sem::x: return "X distance";
    case Sem::y: return "Y distance";
    case Sem::E: return "Euclidean norm";
    case Sem::L: return "distance";
    case Sem::U: return "X velocity";
    case Sem::V: return "Y velocity";
  }
  return "?";
};

void printSemanticTypes(){
  printf( "\n\n SEMANTIC TYPES:\n" );
  for( auto& t: std::string( "BRrXYTDxyELUV") ) printf( "\n   %c | %s", t, LEX( (Sem)t ) );
  printf( "\n" );
}
                                                                                                                              /*
Set of available operators (operator denoted by single character):
                                                                                                                              */
const std::unordered_set< char > OPERATORS{ '*', '/', '-', '+', '^', '=', '<', '|', '&' };
                                                                                                                              /*
Textual description of operators:
                                                                                                                              */
const char* LEX( const char& func ){
  switch( func ){
    case '*': return "multiplication";
    case '/': return "division";
    case '-': return "substraction";
    case '+': return "addition";
    case '^': return "hypot(*,*)";
    case '=': return "equality";
    case '<': return "less than";
    case '|': return "or";
    case '&': return "and";
  }
  return "?";
};

void printOperators(){
  printf( "\n\n OPERATORS:\n" );
  for( auto& f: std::string( "*/-+^=<|&") ) printf( "\n   %c | %s", f, LEX( f ) );
  printf( "\n" );
}

double calc( const double* arg, const char* postfix ){
                                                                                                                              /*
  Calculate value using provided numerical arguments and postfix expression:
                                                                                                                              */
  CoreAGI::Stack< double, 32 > STACK;

  constexpr double EPS{ 1.0e-3 };

  auto equal = [&]( const double& L, const double& R )->double{ return fabs( L - R ) < EPS ? 1.0 : -1.0; };

  for( const char* symbol = postfix; *symbol; symbol++ ){
    if( isdigit( *symbol ) ){
      const int i = int(*symbol) - 48; // :convert digit symbol into int index
      if( i < 0 or i >= 6 ){
        printf( "\n\n [calc] RPN `%s` index `%c` out of range:\n", postfix, *symbol );
        printf( "\n\n ABEND\n" );
        exit( 15 );
      }
      STACK.push( arg[i] );
      continue;
    }
    double L;
    double R;
    try{ R = STACK.pop(); } catch(...){ printf( "\n\n Empty stack (R) `%s` symbol %c\n", postfix, *symbol ); exit(13); }
    try{ L = STACK.pop(); } catch(...){ printf( "\n\n Empty stack (L) `%s` symbol %c\n", postfix, *symbol ); exit(14); }
    switch( *symbol ){
      case '*' : STACK.push(             L * R                  ); break;
      case '/' : STACK.push(             L / R                  ); break;
      case '-' : STACK.push(             L - R                  ); break;
      case '+' : STACK.push(             L + R                  ); break;
      case '^' : STACK.push( std::hypot( L,  R )                ); break;
      case '=' : STACK.push( equal     ( L,  R )                ); break;
      case '<' : STACK.push(   L < R               ? 1.0 : -1.0 ); break;
      case '|' : STACK.push( ( L > 0.0 or  R > 0 ) ? 1.0 : -1.0 ); break;
      case '&' : STACK.push( ( L > 0.0 and R > 0 ) ? 1.0 : -1.0 ); break;
      default  : assert( false );
    }
  }
  const auto result{ STACK.pop() }; assert( STACK.empty() );
  return result;
}//calc


std::string infix( const char* postfix ){
                                                                                                                              /*
  Convert RPN (postfix) into infix expression:
                                                                                                                              */
  using Str = std::string;
  CoreAGI::Stack< std::string, 64 > STACK;
  auto br = [&]( Str S )->Str{
    if( S.size() == 2   ) return S;
    if( S.back() == ')' ) return S;
    return Str("(") + S + ')';
  };
  Str L;
  Str R;

  auto popRL = [&](){ R = STACK.pop(); L = STACK.pop(); };

  auto lex = [&]( char symbol )->Str{
    switch( symbol ){
      case '0': return "X1";
      case '1': return "Y1";
      case '2': return "R1";
      case '3': return "X2";
      case '4': return "Y2";
      case '5': return "R2";
      default : return "?";
    }
  };

  for( const char* symbol = postfix; *symbol; symbol++ ){
    switch( *symbol ){
      case '*' : popRL(); STACK.push( br( L ) + '*'  + br( R )         ); break;
      case '/' : popRL(); STACK.push( br( L ) + '/'  + br( R )         ); break;
      case '-' : popRL(); STACK.push( br( L ) + '-'  + br( R )         ); break;
      case '+' : popRL(); STACK.push( br( L ) + '+'  + br( R )         ); break;
      case '^' : popRL(); STACK.push( Str("hypot") + br( L + ',' + R ) ); break;
      case '=' : popRL(); STACK.push( br( L ) + "==" + br( R )         ); break;
      case '<' : popRL(); STACK.push( L + "<"  + R                     ); break;
      case '|' : popRL(); STACK.push( L + '|'  + R                     ); break;
      case '&' : popRL(); STACK.push( L + '&'  + R                     ); break;
      default  : STACK.push( lex( *symbol ) );
    }
  }
  const auto result{ STACK.pop() }; assert( STACK.empty() );
  return result;
}

bool equivalent( const char* RPN1, const char* RPN2 ){
                                                                                                                              /*
  Check if expressions are equivalent:
                                                                                                                              */
  constexpr unsigned ATTEMPT_NUMBER{ 1024 };

  for( [[maybe_unused]]auto attempt: RANGE{ ATTEMPT_NUMBER } ){
                                                                                                                              /*
    Assign random values:
                                                                                                                              */
    const auto X = uniformCoord ( RANDOM );
    const auto Y = uniformCoord ( RANDOM );
    const auto R = uniformRadius( RANDOM );

    const auto x = uniformCoord ( RANDOM );
    const auto y = uniformCoord ( RANDOM );
    const auto r = uniformRadius( RANDOM );

    double arg[6]{ X,Y,R, x,y,r };
                                                                                                                              /*
    Calculate values using both RPN and compare results:
                                                                                                                              */
    const double v1{ calc( arg, RPN1 ) };
    const double v2{ calc( arg, RPN2 ) };

    if( v1 != v2 ) return false;
  }
  return true;
}//equivalent


bool normalize( char* postfix ){
                                                                                                                              /*
  Normalization of postfix expression that simplifies detection of
  expressions with identic meaning:
                                                                                                                              */
  bool ok{ true };

  std::unordered_set< char > M;

  auto arrangeOperands = [&]( int io, int it ){
                                                                                                                              /*
    Check for multiple operand occurences:
                                                                                                                              */
    M.clear();
    for( auto i: RANGE{ io, it+1 } ) M.insert( postfix[i] );
    if( int( M.size() ) < it - io + 1 ) ok = false;
    char* co{ &postfix[io] };
    CoreAGI::heapSort<char>( co, it - io + 1, []( const char& L, const char& R )->int{ return int(L)-int(R); } );
  };

  enum Event{
    NONE,
    OPERAND_2_OPERATOR,
    OPERATOR_2_DIFF_OPERATOR,
    OPERATOR_2_OPERAND
  };

  const int L{ int( strlen( postfix ) ) };
  int  Jo  { -1 };  // :index of first group element
  int  Jt  { -1 };  // :index of last  group element
  char pred{ '~' }; // :previous symbol initialized by dummy operator (ant non-alpha is OK)
  for( int i: RANGE{ L } ){
    char succ{ postfix[i] };
                                                                                                                              /*
    Detect event:
                                                                                                                              */
    Event event{ NONE };
    if(     isalpha( pred ) and not isalpha( succ )                  ) event = OPERAND_2_OPERATOR;
    if( not isalpha( pred ) and not isalpha( succ ) and pred != succ ) event = OPERATOR_2_DIFF_OPERATOR;
    if( not isalpha( pred ) and     isalpha( succ )                  ) event = OPERATOR_2_OPERAND;
                                                                                                                              /*
    Process event:
                                                                                                                              */
    switch( event ){
      case NONE:
        Jt++;
        break;
      case OPERATOR_2_DIFF_OPERATOR:
      case OPERATOR_2_OPERAND:
        if( pred == '+' or pred == '*' ) if( Jo >= 0 and Jt > Jo ) arrangeOperands( Jo, Jt );
        Jo = Jt = i;
        break;
      case OPERAND_2_OPERATOR:
        Jo = Jt = i;
        break;
    }//switch
    pred = succ;
  }//for i
  if( pred == '+' or pred == '*' ) if( Jo >= 0 and Jt > Jo ) arrangeOperands( Jo, Jt );
  return ok;
}//normalize


struct Func{
                                                                                                                              /*
  Semantic description of operator:
                                                                                                                              */
  char symbol;      // :operator  symbol
  Sem  type;        // :result    semantic type
  Sem  left;        // :left  arg semantic type
  Sem  right;       // :right arg aemantic type
  bool commutative; // :not depends o argument order if commutative == true
                                                                                                                              /*
  bool( Func f ) retuen `false` if it represents constant (i.e. value, not a function):
                                                                                                                              */
  explicit operator bool() const { return left != Sem::o and right != Sem::o; }

  Func(): symbol{'\0'}, type{ Sem::o }, left{ Sem::o }, right{ Sem::o }, commutative{ false }{}
                                                                                                                              /*
  Constructor parses operator description string that consists of 4 symbols:

     "LRFO"

     L - left  arg semantic type
     R - right arg semantic type
     F - operator/function symbol
     O - semantic type of result/output

  If right arg type symbol equals `,` it means that:

     [a] allowed more than 2 arguments
     [b] all arguments has the same semantic type
     [c] operator indifferent to order off arguments (communicative)
                                                                                                                              */
  Func( const char* def ): symbol{}, type{}, left{}, right{}, commutative{}{
    symbol      =   def[2];
    commutative = ( def[1] == ',' );
    left        = Sem( def[0] );
    right       = commutative ? left : Sem( def[1] );
    type        = Sem( def[3] );
  }
                                                                                                                              /*
  Constant as pseudo-operator:
                                                                                                                              */
  Func( char name, Sem type ): symbol{ name }, type{ type }, left{ Sem::o }, right{ Sem::o }, commutative{ false }{}

  Func( const Func& F ):
    symbol     { F.symbol      },
    type       { F.type        },
    left       { F.left        },
    right      { F.right       },
    commutative{ F.commutative }
  {}

  Func& operator= ( const Func& F ){
    symbol      = F.symbol;
    type        = F.type;
    left        = F.left;
    right       = F.right;
    commutative = F.commutative;
    return *this;
  }

};//struct Func

constexpr unsigned NUMBER_OF_OPERATORS{ 18 };

const Func FUNC[ NUMBER_OF_OPERATORS ] = {
                                                                                                                              /*
  Atomic operator/function definition:

  [ left argument type ][ right argument type ][ operator symbol ][ result type ]
                                                                                                                              */
  { "XX-x" },
  { "YY-y" },
  { "TT-D" },
  { "xy^E" },
  { "xD/U" },
  { "yD/V" },
  { "EE/r" },
  { "LL/R" },
  { "E,+L" },
  { "EE<B" },
  { "LL<B" },
  { "LE<B" },
  { "EL<B" },
  { "rr<B" },
  { "Rr<B" },
  { "rR<B" },
  { "B,|B" },
  { "B,&B" },
};

void printAtomicFunctions(){
  printf( "\n\n ATOMIC INVARIANT FUNCTIONS\n"                                          );
  printf( "\n     Note: If right arg type symbol equals `,` it means that:\n"          );
  printf( "\n       [a] allowed more than 2 arguments"                                 );
  printf( "\n       [b] all arguments has the same semantic type"                      );
  printf( "\n       [c] operator indifferent to order off arguments (communicative)\n" );
  printf( "\n   %2s  %c  %-16s %-16s %-16s %-16s\n", "#", ' ', "Operator", "Left arg type", "Right arg type", "Result type");
  for( auto i: RANGE{ NUMBER_OF_OPERATORS } ){
    auto& F{ FUNC[i] };
    printf( "\n   %2u  %c  %-16s %-16s %-16s %-16s", i, F.symbol, LEX(F.symbol), LEX(F.left), LEX(F.right), LEX(F.type) );
  }
  printf( "\n" );
}


struct Expr {
                                                                                                                              /*
  Expression represents value if `left` and `right` are nulls:
                                                                                                                              */
  Sem      type;   // :function/operator
  unsigned depth;
  char     RPN[ RPN_CAPACITY ]; // :postfix expression == Reverse Polish Notation aka `RPN`

  Expr(): type{}, depth{ 0 }, RPN{}{ memset( RPN, 0, RPN_CAPACITY ); };
                                                                                                                              /*
  Constant value attributed by Type, Type::Semantic; RPM is just it name:
                                                                                                                              */
  Expr( char name, const Sem& type ): type{ type }, depth{ 0 }, RPN{}{ memset( RPN, 0, RPN_CAPACITY ); RPN[0] = name; };

  Expr( const Expr& E ): type{ E.type }, depth{ E.depth }, RPN{}{ memcpy( RPN, E.RPN, RPN_CAPACITY ); }

  Expr( const unsigned d, const Func& f, std::initializer_list< Expr > E ): type{ f.type }, depth{ d }, RPN{}{
    std::vector< std::string > arg;
    std::unordered_set< std::string > S;
    for( auto Ei: E ){
      arg.push_back( std::string{ Ei.RPN } );
      S.insert( Ei.RPN );
    }
    assert( not arg.empty() );
                                                                                                                              /*
    Clear RPN:
                                                                                                                              */
    memset( RPN, 0, RPN_CAPACITY );
                                                                                                                              /*
    Check if all args are different: using a few identical args is meaningless in our context;
    just let RPN empty is such case:
                                                                                                                              */
    if( S.size() < arg.size() ) return;
                                                                                                                              /*
    Sort arg`s RPN in case when function is commutative (it simplifies detection of identical expressions):
                                                                                                                              */
    if( f.commutative ) std::sort( arg.begin(), arg.end() );
                                                                                                                              /*
    Compose RPN - concatenate arg`s RPN one by one:
                                                                                                                              */
    unsigned pos{ 0 };
    for( auto Ai: arg ) for( auto& symbol: Ai ){ RPN[ pos++ ] = symbol; assert( pos <= RPN_CAPACITY ); }
                                                                                                                              /*
    Append function symbol (repeated once of more times dependently on arg num):
                                                                                                                              */
    for( [[maybe_unused]]auto& i: RANGE{ arg.size() - 1 } ){ RPN[ pos++ ] = f.symbol; assert( pos <= RPN_CAPACITY ); }
  }

  explicit operator bool() const { return RPN[0] == '\0'; }

  bool acceptable( /*out*/char* expl ) const {
                                                                                                                              /*
    Check for constant value using set of random arguments:
                                                                                                                              */
    constexpr unsigned ATTEMPT_NUMBER{ 1024 };
    std::unordered_set< double > S;
    for( [[maybe_unused]]auto attempt: RANGE{ ATTEMPT_NUMBER } ){
                                                                                                                              /*
      Assign random values:
                                                                                                                              */
      double arg[6]{
        uniformCoord ( RANDOM ), // :X1
        uniformCoord ( RANDOM ), // :Y1
        uniformRadius( RANDOM ), // :R1
        uniformCoord ( RANDOM ), // :X2
        uniformCoord ( RANDOM ), // :Y2
        uniformRadius( RANDOM )  // :R2
      };

      const double v{ calc( arg, RPN ) };
      S.insert( v );
    }
    const auto N{ S.size() };
    if( N > 1 ){ strcpy( expl, "var"      ); return true;  }
    else       { strcpy( expl, "constant" ); return false; }
  }//acceptable

};//struct Expr


int main(){

  printf( "\n CONSTRUCTION OF INVARIANTS USING SEMANTIC TYPES\n" );
  printf( "\n  For more details see https://gnosiseng.com/agi.html\n" );

  constexpr unsigned CAPACITY{ 16*1024 };
  constexpr unsigned LIMIT   { 200 };

  Expr expr[ CAPACITY ];

  printSemanticTypes();
  printOperators();
  printAtomicFunctions();

  printf( "\n\n _____________________________________________ \n" );

  std::unordered_set< std::string > F;

  unsigned n{ 0 }; // :total number of criterial functions
                                                                                                                              /*
  Define variables as (atomic) expressions:
                                                                                                                              */
  expr[n++] = Expr{ '0', Sem::X }; // :X1
  expr[n++] = Expr{ '1', Sem::Y }; // :Y1
  expr[n++] = Expr{ '2', Sem::E }; // :R1

  expr[n++] = Expr{ '3', Sem::X }; // :X2
  expr[n++] = Expr{ '4', Sem::Y }; // :Y2
  expr[n++] = Expr{ '5', Sem::E }; // :R2

  auto expl = []( unsigned i )->const char*{
                                                                                                                              /*
    Description of the arguments:
                                                                                                                              */
    switch( i ){
      case 0 : return "X1 ~ Object1, x coordinate";
      case 1 : return "Y1 ~ Object1, y coordinate";
      case 2 : return "R1 ~ Object1, radius";
      case 3 : return "X2 ~ Object2, x coordinate";
      case 4 : return "Y2 ~ Object2, y coordinate";
      case 5 : return "R2 ~ Object2, radius";
    }
    return "";
  };
                                                                                                                              /*
  Source variables:
                                                                                                                              */
  printf( "\n ARGUMENTS\n" );
  printf( "\n   Arguments are numerical values and can be used as invariants." );
  printf( "\n   In the postfix expression index `%u`..`%u` refers argument:\n", 0, n );
  printf( "\n   index  semantic type    description\n" );
  for( auto i: RANGE{ n } ) printf( "\n     %1u    %-16s %s", i, LEX( expr[i].type ), expl( i ) );

  constexpr unsigned MAX_DEPTH{ 5 };
  printf( "\n _____________________________________________\n" );

  printf( "\n\n POTENTIAL INVARIANT FUNCTIONS" );
  printf( "\n\n   Maximal depth of function nesting: %u", MAX_DEPTH );

  printf( "\n _____________________________________________\n" );

  unsigned No{ 0 };  // :total tested
  unsigned Na{ 0 };  // :total `not acceptable`
  unsigned Nu{ 0 };  // :total `not unique`
  unsigned Np{ n };  // :total `presented already`

  std::map< std::string, unsigned > statistics; // :explication statistics

  const char* PRESENTED{ "usable" };

  auto upd = [&]( const char* expl ){
    std::string s{ expl };
    if( statistics.contains( s ) ) statistics[s]++; else statistics[s]=1;
  };

  printf( "\n   %3s  %-30s Semantic type %s", "#", "Postfix expression", "Infix expression\n" );
  for( auto i: RANGE{ n } ){
    auto& E{ expr[i] };
    printf( "\n %u %3u  %-40s [%c] %s", E.depth, i, E.RPN, (char)E.type, infix( E.RPN ).c_str() );
  }

  printf( "\n\n Dept         %6u", 0  );
  printf(   "\n Total tested %6u", No );
  printf(   "\n Total usable %6u", n  );
  printf(   "\n __________________________________________________________________________________________________\n" );

  char SRC[ RPN_CAPACITY ];

  auto process = [&]( Expr E ){
                                                                                                                              /*
    Check if already presented:
                                                                                                                              */
    std::string RPN{ E.RPN };
    if( F.contains( RPN ) ){ upd( PRESENTED ), Np++; return; }
                                                                                                                              /*
    Normalize RPN:
                                                                                                                              */
    memcpy( SRC, E.RPN, RPN_CAPACITY );             // :prerserved original RPN
    if( strcmp( SRC, E.RPN ) == 0 ) SRC[0] = '\0';  // :kill original RPN if it the same as normalized
                                                                                                                              /*
    Check if is acceptable:
                                                                                                                              */
    char expl[ 32 ];
    bool ok = E.acceptable( expl );
    if( not ok ){ upd( expl ), Na++; return; }
                                                                                                                              /*
    Check if is unique:
                                                                                                                              */
    bool unique{ true };
    for( auto k: RANGE{ n } ){
      if( E.type != expr[k].type ) continue; // :expression with different semantic types can't be identical
      if( equivalent( E.RPN, expr[k].RPN ) ){ unique = false; break; }
    }
    if( not unique ){ upd( "non-unique" ), Nu++; return; }
                                                                                                                              /*
    Add to set of usable ones:
                                                                                                                              */
    expr[ n++ ] = E;
    F.insert( RPN );
    printf( "\n %u %3u  %-40s [%c] %s", E.depth, n, E.RPN, (char)E.type,infix(E.RPN).c_str() );
  };
                                                                                                                              /*
  Function composition:
                                                                                                                              */
  for( auto& depth: RANGE{ 1u, MAX_DEPTH } ){
    printf( "\n\n   %3s  %-30s Semantic type %s", "#", "Postfix expression", "Infix expression\n" );
    const unsigned N{ n };
    for( auto i: RANGE{ N } ){
      auto& Ei{ expr[i] };
      for( auto j: RANGE{ N } ){
        if( i == j ) continue;
        auto& Ej{ expr[j] };
                                                                                                                              /*
        Two arguments functions:
                                                                                                                              */
        for( auto f: FUNC ){
          if( f.commutative ){
            if( Ei.type == f.left and Ej.type == f.right ) process( Expr{ depth, f, { Ei, Ej } } );
            No++;
          } else {
            if( Ei.type == f.left and Ej.type == f.right ) process( Expr{ depth, f, { Ei,Ej } } );
            if( Ej.type == f.left and Ei.type == f.right ) process( Expr{ depth, f, { Ej,Ei } } );
            No++;
            No++;
          }
          if( n >= LIMIT ) goto FINISH;
        }//for f
                                                                                                                              /*
        Functions with 3 arguments:

        for( auto k: RANGE{ N } ){
          if( k == i or k == j ) continue;
          for( auto f: FUNC ){
            if( not f.commutative ) continue;
            auto& Ek{ expr[k] };
            if( Ei.type == f.left and Ej.type == f.left and Ek.type == f.left ) process( Expr{ depth, f, { Ei,Ej,Ek } } );
            No++;
            if( n>= LIMIT ) goto FINISH;
          }//for f
        }//for k
                                                                                                                              */
      }//for j
    }//for i

    printf( "\n\n Dept         %6u", depth );
    printf(   "\n Total tested %6u", No    );
    printf(   "\n Total usable %6u", n  );
    printf(   "\n __________________________________________________________________________________________________" );

  }//for depth
  FINISH:
                                                                                                                              /*
  printf( "\n\n Statistics:\n" );
  for( const auto& [ expl, num ]: statistics ) printf( "\n   %-12s %6u", expl.c_str(), num );
                                                                                                                              */
  printf( "\n\n Finish\n" );
  return EXIT_SUCCESS;
}
