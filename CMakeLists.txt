cmake_minimum_required(VERSION 3.14)
project(xlisp
    VERSION 3.3
    DESCRIPTION "XLISP - an object-oriented LISP"
    LANGUAGES C)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(GNUInstallDirs)

set(XLISP_LIBS_PATH ${CMAKE_INSTALL_FULL_LIBEXECDIR}/${PROJECT_NAME})
add_compile_definitions(XLISP_LIBS_PATH="${XLISP_LIBS_PATH}/")

add_library(xlisp-lib "")
add_subdirectory(src)

target_link_libraries(xlisp-lib m)
set_property(TARGET xlisp-lib PROPERTY POSITION_INDEPENDENT_CODE ON)

add_library(xlisp-ext SHARED "")
add_subdirectory(ext)

add_subdirectory(xlisp)

include(CheckIPOSupported)
check_ipo_supported(RESULT can_link_time_optimize)
if(can_link_time_optimize)
    set_target_properties(xlisp-lib PROPERTIES INTERPROCEDURAL_OPTIMIZATION TRUE)
    set_target_properties(xlisp-ext PROPERTIES INTERPROCEDURAL_OPTIMIZATION TRUE)
    set_target_properties(xlisp PROPERTIES INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()

# message(FATAL_ERROR ${CMAKE_INSTALL_FULL_LIBEXECDIR})

#---------------------------------installation---------------------------------#
# if(NOT WIN32 AND CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
#     set(CMAKE_INSTALL_PREFIX "/opt/${PROJECT_NAME}")
# endif()

# The '.' ends up being relative to CMAKE_INSTALL_PREFIX.
install(TARGETS
    xlisp xlisp-ext
    COMPONENT xlisp_component
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME})

install(FILES
    doc/api.md
    doc/crec.md
    doc/xlisp.md
    COMPONENT xlisp_component
    DESTINATION ${CMAKE_INSTALL_FULL_DOCDIR})

install(FILES
    clisp.lsp
    compile.lsp
    crec.lsp
    env.lsp
    fact.lsp
    fasl.lsp
    macros.lsp
    math.lsp
    obj.lsp
    objects.lsp
    pp.lsp
    qquote.lsp
    xlinit.lsp
    xlisp.lsp
    COMPONENT xlisp_component
    DESTINATION ${XLISP_LIBS_PATH})

#----------------------------------packaging-----------------------------------#
set(CPACK_VERBATIM_VARIABLES YES)
set(CPACK_RESOURCE_FILE_LICENSE ${CMAKE_CURRENT_LIST_DIR}/LICENSE.txt)
set(CPACK_PACKAGE_FILE_README ${CMAKE_CURRENT_LIST_DIR}/README.txt)
set(CPACK_SOURCE_IGNORE_FILES
      /\\.git/
      \\.swp
      \\.orig
      /CMakeLists\\.txt\\.user
      /privateDir/)

if(WIN32)
    set(CPACK_GENERATOR ZIP WIX)
elseif(APPLE)
    set(CPACK_GENERATOR TGZ productbuild)
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(CPACK_GENERATOR TGZ RPM)
else()
    set(CPACK_GENERATOR TGZ)
endif()

# `make package`
include(CPack)
