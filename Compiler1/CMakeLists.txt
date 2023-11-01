cmake_minimum_required(VERSION 3.5)

project(GLSLCompiler)

set(CMAKE_CXX_STANDARD 17)
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
add_compile_options(-fno-rtti)
llvm_map_components_to_libnames(llvm_libs core orcjit support nativecodegen)

add_executable(GLSLCompiler main.cpp)
target_link_libraries(GLSLCompiler PUBLIC ${llvm_libs})