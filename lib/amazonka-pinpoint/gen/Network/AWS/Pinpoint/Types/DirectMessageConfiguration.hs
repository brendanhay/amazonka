{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DirectMessageConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.DirectMessageConfiguration
  ( DirectMessageConfiguration (..)
  -- * Smart constructor
  , mkDirectMessageConfiguration
  -- * Lenses
  , dmcADMMessage
  , dmcAPNSMessage
  , dmcBaiduMessage
  , dmcDefaultMessage
  , dmcDefaultPushNotificationMessage
  , dmcEmailMessage
  , dmcGCMMessage
  , dmcSMSMessage
  , dmcVoiceMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ADMMessage as Types
import qualified Network.AWS.Pinpoint.Types.APNSMessage as Types
import qualified Network.AWS.Pinpoint.Types.BaiduMessage as Types
import qualified Network.AWS.Pinpoint.Types.DefaultMessage as Types
import qualified Network.AWS.Pinpoint.Types.DefaultPushNotificationMessage as Types
import qualified Network.AWS.Pinpoint.Types.EmailMessage as Types
import qualified Network.AWS.Pinpoint.Types.GCMMessage as Types
import qualified Network.AWS.Pinpoint.Types.SMSMessage as Types
import qualified Network.AWS.Pinpoint.Types.VoiceMessage as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings and content for the default message and any default messages that you tailored for specific channels.
--
-- /See:/ 'mkDirectMessageConfiguration' smart constructor.
data DirectMessageConfiguration = DirectMessageConfiguration'
  { aDMMessage :: Core.Maybe Types.ADMMessage
    -- ^ The default push notification message for the ADM (Amazon Device Messaging) channel. This message overrides the default push notification message (DefaultPushNotificationMessage).
  , aPNSMessage :: Core.Maybe Types.APNSMessage
    -- ^ The default push notification message for the APNs (Apple Push Notification service) channel. This message overrides the default push notification message (DefaultPushNotificationMessage).
  , baiduMessage :: Core.Maybe Types.BaiduMessage
    -- ^ The default push notification message for the Baidu (Baidu Cloud Push) channel. This message overrides the default push notification message (DefaultPushNotificationMessage).
  , defaultMessage :: Core.Maybe Types.DefaultMessage
    -- ^ The default message for all channels.
  , defaultPushNotificationMessage :: Core.Maybe Types.DefaultPushNotificationMessage
    -- ^ The default push notification message for all push notification channels.
  , emailMessage :: Core.Maybe Types.EmailMessage
    -- ^ The default message for the email channel. This message overrides the default message (DefaultMessage).
  , gCMMessage :: Core.Maybe Types.GCMMessage
    -- ^ The default push notification message for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message overrides the default push notification message (DefaultPushNotificationMessage).
  , sMSMessage :: Core.Maybe Types.SMSMessage
    -- ^ The default message for the SMS channel. This message overrides the default message (DefaultMessage).
  , voiceMessage :: Core.Maybe Types.VoiceMessage
    -- ^ The default message for the voice channel. This message overrides the default message (DefaultMessage).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DirectMessageConfiguration' value with any optional fields omitted.
mkDirectMessageConfiguration
    :: DirectMessageConfiguration
mkDirectMessageConfiguration
  = DirectMessageConfiguration'{aDMMessage = Core.Nothing,
                                aPNSMessage = Core.Nothing, baiduMessage = Core.Nothing,
                                defaultMessage = Core.Nothing,
                                defaultPushNotificationMessage = Core.Nothing,
                                emailMessage = Core.Nothing, gCMMessage = Core.Nothing,
                                sMSMessage = Core.Nothing, voiceMessage = Core.Nothing}

-- | The default push notification message for the ADM (Amazon Device Messaging) channel. This message overrides the default push notification message (DefaultPushNotificationMessage).
--
-- /Note:/ Consider using 'aDMMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcADMMessage :: Lens.Lens' DirectMessageConfiguration (Core.Maybe Types.ADMMessage)
dmcADMMessage = Lens.field @"aDMMessage"
{-# INLINEABLE dmcADMMessage #-}
{-# DEPRECATED aDMMessage "Use generic-lens or generic-optics with 'aDMMessage' instead"  #-}

-- | The default push notification message for the APNs (Apple Push Notification service) channel. This message overrides the default push notification message (DefaultPushNotificationMessage).
--
-- /Note:/ Consider using 'aPNSMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcAPNSMessage :: Lens.Lens' DirectMessageConfiguration (Core.Maybe Types.APNSMessage)
dmcAPNSMessage = Lens.field @"aPNSMessage"
{-# INLINEABLE dmcAPNSMessage #-}
{-# DEPRECATED aPNSMessage "Use generic-lens or generic-optics with 'aPNSMessage' instead"  #-}

-- | The default push notification message for the Baidu (Baidu Cloud Push) channel. This message overrides the default push notification message (DefaultPushNotificationMessage).
--
-- /Note:/ Consider using 'baiduMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcBaiduMessage :: Lens.Lens' DirectMessageConfiguration (Core.Maybe Types.BaiduMessage)
dmcBaiduMessage = Lens.field @"baiduMessage"
{-# INLINEABLE dmcBaiduMessage #-}
{-# DEPRECATED baiduMessage "Use generic-lens or generic-optics with 'baiduMessage' instead"  #-}

-- | The default message for all channels.
--
-- /Note:/ Consider using 'defaultMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcDefaultMessage :: Lens.Lens' DirectMessageConfiguration (Core.Maybe Types.DefaultMessage)
dmcDefaultMessage = Lens.field @"defaultMessage"
{-# INLINEABLE dmcDefaultMessage #-}
{-# DEPRECATED defaultMessage "Use generic-lens or generic-optics with 'defaultMessage' instead"  #-}

-- | The default push notification message for all push notification channels.
--
-- /Note:/ Consider using 'defaultPushNotificationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcDefaultPushNotificationMessage :: Lens.Lens' DirectMessageConfiguration (Core.Maybe Types.DefaultPushNotificationMessage)
dmcDefaultPushNotificationMessage = Lens.field @"defaultPushNotificationMessage"
{-# INLINEABLE dmcDefaultPushNotificationMessage #-}
{-# DEPRECATED defaultPushNotificationMessage "Use generic-lens or generic-optics with 'defaultPushNotificationMessage' instead"  #-}

-- | The default message for the email channel. This message overrides the default message (DefaultMessage).
--
-- /Note:/ Consider using 'emailMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcEmailMessage :: Lens.Lens' DirectMessageConfiguration (Core.Maybe Types.EmailMessage)
dmcEmailMessage = Lens.field @"emailMessage"
{-# INLINEABLE dmcEmailMessage #-}
{-# DEPRECATED emailMessage "Use generic-lens or generic-optics with 'emailMessage' instead"  #-}

-- | The default push notification message for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message overrides the default push notification message (DefaultPushNotificationMessage).
--
-- /Note:/ Consider using 'gCMMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcGCMMessage :: Lens.Lens' DirectMessageConfiguration (Core.Maybe Types.GCMMessage)
dmcGCMMessage = Lens.field @"gCMMessage"
{-# INLINEABLE dmcGCMMessage #-}
{-# DEPRECATED gCMMessage "Use generic-lens or generic-optics with 'gCMMessage' instead"  #-}

-- | The default message for the SMS channel. This message overrides the default message (DefaultMessage).
--
-- /Note:/ Consider using 'sMSMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcSMSMessage :: Lens.Lens' DirectMessageConfiguration (Core.Maybe Types.SMSMessage)
dmcSMSMessage = Lens.field @"sMSMessage"
{-# INLINEABLE dmcSMSMessage #-}
{-# DEPRECATED sMSMessage "Use generic-lens or generic-optics with 'sMSMessage' instead"  #-}

-- | The default message for the voice channel. This message overrides the default message (DefaultMessage).
--
-- /Note:/ Consider using 'voiceMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcVoiceMessage :: Lens.Lens' DirectMessageConfiguration (Core.Maybe Types.VoiceMessage)
dmcVoiceMessage = Lens.field @"voiceMessage"
{-# INLINEABLE dmcVoiceMessage #-}
{-# DEPRECATED voiceMessage "Use generic-lens or generic-optics with 'voiceMessage' instead"  #-}

instance Core.FromJSON DirectMessageConfiguration where
        toJSON DirectMessageConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("ADMMessage" Core..=) Core.<$> aDMMessage,
                  ("APNSMessage" Core..=) Core.<$> aPNSMessage,
                  ("BaiduMessage" Core..=) Core.<$> baiduMessage,
                  ("DefaultMessage" Core..=) Core.<$> defaultMessage,
                  ("DefaultPushNotificationMessage" Core..=) Core.<$>
                    defaultPushNotificationMessage,
                  ("EmailMessage" Core..=) Core.<$> emailMessage,
                  ("GCMMessage" Core..=) Core.<$> gCMMessage,
                  ("SMSMessage" Core..=) Core.<$> sMSMessage,
                  ("VoiceMessage" Core..=) Core.<$> voiceMessage])
