use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'Test::Smoke::Database',
    'VERSION_FROM'	=> 'lib/Test/Smoke/Database.pm', # finds $VERSION
    'PREREQ_PM'		=> {'DBI' => 0,
			    'CGI' => 0,
			    'News::NNTPClient' =>0},
    'EXE_FILES'         => [ 'bin/admin_smokedb' ],
    ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
      (ABSTRACT => 'Add / parse / view perl reports smoke database', 
      AUTHOR     => 'alian <alian@cpan.org>') : ()),
);
