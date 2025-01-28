target_sources(xlisp-ext PRIVATE
    ext.c
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/xlisp.h")

target_include_directories(xlisp-ext PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/../include)
target_link_libraries(xlisp-ext xlisp-lib)
