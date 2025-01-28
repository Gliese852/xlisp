add_executable(xlisp)

target_sources(xlisp PRIVATE
    xlisp.c
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/xlisp.h")

target_include_directories(xlisp PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/../include)
target_link_libraries(xlisp xlisp-lib)
set_target_properties(xlisp PROPERTIES
    PUBLIC_HEADER ${CMAKE_CURRENT_SOURCE_DIR}/../include/xlisp.h)
