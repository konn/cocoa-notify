#import <Foundation/Foundation.h>
#import <objc/runtime.h>

void snatchingBundle(NSString *name, void(^blk)()) {
  IMP imp_newBlock = imp_implementationWithBlock(^NSString *() {
    return name;
  });
  IMP imp_oldBlock = method_getImplementation(class_getInstanceMethod(objc_getClass("NSBundle"), @selector(bundleIdentifier)));
  method_setImplementation(class_getInstanceMethod(objc_getClass("NSBundle"), @selector(bundleIdentifier)),
                           imp_newBlock);
  blk();
  method_setImplementation(class_getInstanceMethod(objc_getClass("NSBundle"), @selector(bundleIdentifier)),
                           imp_oldBlock);
}
