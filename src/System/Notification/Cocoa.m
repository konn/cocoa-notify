
#import <Foundation/Foundation.h>

#import "BundleSnatcher.h"

NSUserNotificationCenter * inline_c_System_Notification_Cocoa_0_1372577b584c30ce3627c4a7dedfec27ca068c60() {
return (
                         [NSUserNotificationCenter defaultUserNotificationCenter]
                        );
}


NSString * inline_c_System_Notification_Cocoa_1_4d49a7bf918b0d45f9ef6565fd96773943ccad17() {
return ( nil );
}


NSUserNotification * inline_c_System_Notification_Cocoa_2_7ef372a21e3564fc0098b711964efe9fed7a25e5(NSString * notifTitle_inline_c_0, NSString * subt_inline_c_1, NSString * notifInformativeText_inline_c_2, NSString * ident_inline_c_3, BOOL hasAct_inline_c_4, NSString * actBName_inline_c_5, NSString * othBName_inline_c_6, BOOL hasRepl_inline_c_7, NSString * sound_inline_c_8) {

                 NSUserNotification *notif = [NSUserNotification new];
                 notif.title = notifTitle_inline_c_0;
                 notif.subtitle  = subt_inline_c_1;
                 notif.informativeText = notifInformativeText_inline_c_2;
                 notif.identifier = ident_inline_c_3;
                 notif.hasActionButton = hasAct_inline_c_4;
                 notif.actionButtonTitle = actBName_inline_c_5;
                 notif.otherButtonTitle = othBName_inline_c_6;
                 notif.hasReplyButton = hasRepl_inline_c_7;
                 notif.soundName = sound_inline_c_8;
                 return notif;
               
}


void inline_c_System_Notification_Cocoa_3_934447d7ea3cff821d33e46098edab39fdb93ac2(NSUserNotification * notif_inline_c_0, double secs_inline_c_1, NSInteger zone_inline_c_2) {

              NSUserNotification *notif = notif_inline_c_0;
              notif.deliveryDate = [NSDate dateWithTimeIntervalSince1970: secs_inline_c_1];
              NSLog(@"deliv Date set");
              notif.deliveryTimeZone = [NSTimeZone timeZoneForSecondsFromGMT: zone_inline_c_2];
              NSLog(@"deliv Zone set");
            
}


void inline_c_System_Notification_Cocoa_4_51df0313d192e642c664b96bdf023522eeab36e1(NSUserNotification * obj_inline_c_0) {
 [[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification: obj_inline_c_0] ;
}


void inline_c_System_Notification_Cocoa_5_707c0af0faa1856ed12a2c4a50f55f20d95c0110(NSString * name_inline_c_0, NSUserNotification * obj_inline_c_1) {

              snatchingBundle(name_inline_c_0,
                              ^void {[[NSUserNotificationCenter defaultUserNotificationCenter] scheduleNotification: obj_inline_c_1];}) ;
}

