# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 2.t'


use Test::More;
use Test::Smoke::Database::Parsing;
use Data::Dumper;
use File::Copy;
use File::Basename qw(basename dirname);

use strict;

#chdir(dirname($0));
my ($rep,$tt);

if ($ARGV[0]) {
  foreach (glob("t/rpt/*.normal.rpt.*")) { s!^t/!!; $$rep{$_}=1; }
  $tt=1;
} else {
  # Count how many test
  my $d = "cat t/result_normal";
  $rep = eval `$d`;
  $tt = 0;
  foreach my $f (keys %$rep) {
    $tt+= $#{$$rep{$f}}+1;
  }
}

plan tests => $tt+3;

copy("t/rpt/response.good","t/rpt/response.rpt");

# no file return undef
ok(!Test::Smoke::Database::Parsing::parse_rpt, 
   'Test::Smoke::Database::Parsing::parse_rpt without file');
# no existent file return undef
ok(!Test::Smoke::Database::Parsing::parse_rpt('t/mlkmlkmlk'),
    'Test::Smoke::Database::Parsing::parse_rpt with non existent file');
# file not in report format (reply)
cmp_ok(Test::Smoke::Database::Parsing::parse_rpt('t/rpt/response.rpt'), 
   '==', -2, 'Test::Smoke::Database::Parsing::parse_rpt without file');

my %res;
foreach my $f (keys %$rep) {
  my @lr = Test::Smoke::Database::Parsing::parse_rpt('t/'.$f);
  if ($ARGV[0]) { $res{$f}=\@lr; next; }
  else {
    my $nb=0;
    foreach (@lr) { 
       ok(eq_hash($_, $$rep{$f}->[$nb]), "Parse normal report as model") 
          or diag("Find ". Data::Dumper->Dump([ $_  ]).
                  " and want ".Data::Dumper->Dump([ $$rep{$f}->[$nb] ]));
       $nb++;
    }
  }
}
print Data::Dumper->Dump([ \%res], ['rep']),"\n" if ($ARGV[0]);
 