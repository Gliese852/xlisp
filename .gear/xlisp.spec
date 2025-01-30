%define _unpackaged_files_terminate_build 1

Name: xlisp
Version: 3.0.0
Release: alt1

Summary: An object-oriented LISP
License: MIT
Group: Development/Lisp
Url: https://github.com/dbetz/xlisp

Source: %name-%version.tar

BuildRequires(pre): rpm-macros-cmake
BuildRequires: gcc cmake

%description
XLISP is a dialect of the Lisp programming language with extensions to support
object-oriented programming.

%prep
%setup

%build
%cmake
%cmake_build

%install
%cmake_install

%files
%_bindir/xlisp
%_libdir/libxlisp-ext.so
%_prefix/libexec/xlisp/*.lsp
%_docdir/xlisp/*.md
%_includedir/xlisp/xlisp.h

%changelog
* Mon Jan 27 2025 Anton Golubev <golubevan@altlinux.org> 3.0.0-alt1
- initial build
