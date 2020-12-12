{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DirectMessageConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.DirectMessageConfiguration
  ( DirectMessageConfiguration (..),

    -- * Smart constructor
    mkDirectMessageConfiguration,

    -- * Lenses
    dmcAPNSMessage,
    dmcGCMMessage,
    dmcDefaultMessage,
    dmcADMMessage,
    dmcSMSMessage,
    dmcEmailMessage,
    dmcVoiceMessage,
    dmcBaiduMessage,
    dmcDefaultPushNotificationMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ADMMessage
import Network.AWS.Pinpoint.Types.APNSMessage
import Network.AWS.Pinpoint.Types.BaiduMessage
import Network.AWS.Pinpoint.Types.DefaultMessage
import Network.AWS.Pinpoint.Types.DefaultPushNotificationMessage
import Network.AWS.Pinpoint.Types.EmailMessage
import Network.AWS.Pinpoint.Types.GCMMessage
import Network.AWS.Pinpoint.Types.SMSMessage
import Network.AWS.Pinpoint.Types.VoiceMessage
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings and content for the default message and any default messages that you tailored for specific channels.
--
-- /See:/ 'mkDirectMessageConfiguration' smart constructor.
data DirectMessageConfiguration = DirectMessageConfiguration'
  { apnsMessage ::
      Lude.Maybe APNSMessage,
    gcmMessage :: Lude.Maybe GCMMessage,
    defaultMessage ::
      Lude.Maybe DefaultMessage,
    aDMMessage :: Lude.Maybe ADMMessage,
    sMSMessage :: Lude.Maybe SMSMessage,
    emailMessage ::
      Lude.Maybe EmailMessage,
    voiceMessage ::
      Lude.Maybe VoiceMessage,
    baiduMessage ::
      Lude.Maybe BaiduMessage,
    defaultPushNotificationMessage ::
      Lude.Maybe
        DefaultPushNotificationMessage
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DirectMessageConfiguration' with the minimum fields required to make a request.
--
-- * 'aDMMessage' - The default push notification message for the ADM (Amazon Device Messaging) channel. This message overrides the default push notification message (DefaultPushNotificationMessage).
-- * 'apnsMessage' - The default push notification message for the APNs (Apple Push Notification service) channel. This message overrides the default push notification message (DefaultPushNotificationMessage).
-- * 'baiduMessage' - The default push notification message for the Baidu (Baidu Cloud Push) channel. This message overrides the default push notification message (DefaultPushNotificationMessage).
-- * 'defaultMessage' - The default message for all channels.
-- * 'defaultPushNotificationMessage' - The default push notification message for all push notification channels.
-- * 'emailMessage' - The default message for the email channel. This message overrides the default message (DefaultMessage).
-- * 'gcmMessage' - The default push notification message for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message overrides the default push notification message (DefaultPushNotificationMessage).
-- * 'sMSMessage' - The default message for the SMS channel. This message overrides the default message (DefaultMessage).
-- * 'voiceMessage' - The default message for the voice channel. This message overrides the default message (DefaultMessage).
mkDirectMessageConfiguration ::
  DirectMessageConfiguration
mkDirectMessageConfiguration =
  DirectMessageConfiguration'
    { apnsMessage = Lude.Nothing,
      gcmMessage = Lude.Nothing,
      defaultMessage = Lude.Nothing,
      aDMMessage = Lude.Nothing,
      sMSMessage = Lude.Nothing,
      emailMessage = Lude.Nothing,
      voiceMessage = Lude.Nothing,
      baiduMessage = Lude.Nothing,
      defaultPushNotificationMessage = Lude.Nothing
    }

-- | The default push notification message for the APNs (Apple Push Notification service) channel. This message overrides the default push notification message (DefaultPushNotificationMessage).
--
-- /Note:/ Consider using 'apnsMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcAPNSMessage :: Lens.Lens' DirectMessageConfiguration (Lude.Maybe APNSMessage)
dmcAPNSMessage = Lens.lens (apnsMessage :: DirectMessageConfiguration -> Lude.Maybe APNSMessage) (\s a -> s {apnsMessage = a} :: DirectMessageConfiguration)
{-# DEPRECATED dmcAPNSMessage "Use generic-lens or generic-optics with 'apnsMessage' instead." #-}

-- | The default push notification message for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message overrides the default push notification message (DefaultPushNotificationMessage).
--
-- /Note:/ Consider using 'gcmMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcGCMMessage :: Lens.Lens' DirectMessageConfiguration (Lude.Maybe GCMMessage)
dmcGCMMessage = Lens.lens (gcmMessage :: DirectMessageConfiguration -> Lude.Maybe GCMMessage) (\s a -> s {gcmMessage = a} :: DirectMessageConfiguration)
{-# DEPRECATED dmcGCMMessage "Use generic-lens or generic-optics with 'gcmMessage' instead." #-}

-- | The default message for all channels.
--
-- /Note:/ Consider using 'defaultMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcDefaultMessage :: Lens.Lens' DirectMessageConfiguration (Lude.Maybe DefaultMessage)
dmcDefaultMessage = Lens.lens (defaultMessage :: DirectMessageConfiguration -> Lude.Maybe DefaultMessage) (\s a -> s {defaultMessage = a} :: DirectMessageConfiguration)
{-# DEPRECATED dmcDefaultMessage "Use generic-lens or generic-optics with 'defaultMessage' instead." #-}

-- | The default push notification message for the ADM (Amazon Device Messaging) channel. This message overrides the default push notification message (DefaultPushNotificationMessage).
--
-- /Note:/ Consider using 'aDMMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcADMMessage :: Lens.Lens' DirectMessageConfiguration (Lude.Maybe ADMMessage)
dmcADMMessage = Lens.lens (aDMMessage :: DirectMessageConfiguration -> Lude.Maybe ADMMessage) (\s a -> s {aDMMessage = a} :: DirectMessageConfiguration)
{-# DEPRECATED dmcADMMessage "Use generic-lens or generic-optics with 'aDMMessage' instead." #-}

-- | The default message for the SMS channel. This message overrides the default message (DefaultMessage).
--
-- /Note:/ Consider using 'sMSMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcSMSMessage :: Lens.Lens' DirectMessageConfiguration (Lude.Maybe SMSMessage)
dmcSMSMessage = Lens.lens (sMSMessage :: DirectMessageConfiguration -> Lude.Maybe SMSMessage) (\s a -> s {sMSMessage = a} :: DirectMessageConfiguration)
{-# DEPRECATED dmcSMSMessage "Use generic-lens or generic-optics with 'sMSMessage' instead." #-}

-- | The default message for the email channel. This message overrides the default message (DefaultMessage).
--
-- /Note:/ Consider using 'emailMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcEmailMessage :: Lens.Lens' DirectMessageConfiguration (Lude.Maybe EmailMessage)
dmcEmailMessage = Lens.lens (emailMessage :: DirectMessageConfiguration -> Lude.Maybe EmailMessage) (\s a -> s {emailMessage = a} :: DirectMessageConfiguration)
{-# DEPRECATED dmcEmailMessage "Use generic-lens or generic-optics with 'emailMessage' instead." #-}

-- | The default message for the voice channel. This message overrides the default message (DefaultMessage).
--
-- /Note:/ Consider using 'voiceMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcVoiceMessage :: Lens.Lens' DirectMessageConfiguration (Lude.Maybe VoiceMessage)
dmcVoiceMessage = Lens.lens (voiceMessage :: DirectMessageConfiguration -> Lude.Maybe VoiceMessage) (\s a -> s {voiceMessage = a} :: DirectMessageConfiguration)
{-# DEPRECATED dmcVoiceMessage "Use generic-lens or generic-optics with 'voiceMessage' instead." #-}

-- | The default push notification message for the Baidu (Baidu Cloud Push) channel. This message overrides the default push notification message (DefaultPushNotificationMessage).
--
-- /Note:/ Consider using 'baiduMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcBaiduMessage :: Lens.Lens' DirectMessageConfiguration (Lude.Maybe BaiduMessage)
dmcBaiduMessage = Lens.lens (baiduMessage :: DirectMessageConfiguration -> Lude.Maybe BaiduMessage) (\s a -> s {baiduMessage = a} :: DirectMessageConfiguration)
{-# DEPRECATED dmcBaiduMessage "Use generic-lens or generic-optics with 'baiduMessage' instead." #-}

-- | The default push notification message for all push notification channels.
--
-- /Note:/ Consider using 'defaultPushNotificationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcDefaultPushNotificationMessage :: Lens.Lens' DirectMessageConfiguration (Lude.Maybe DefaultPushNotificationMessage)
dmcDefaultPushNotificationMessage = Lens.lens (defaultPushNotificationMessage :: DirectMessageConfiguration -> Lude.Maybe DefaultPushNotificationMessage) (\s a -> s {defaultPushNotificationMessage = a} :: DirectMessageConfiguration)
{-# DEPRECATED dmcDefaultPushNotificationMessage "Use generic-lens or generic-optics with 'defaultPushNotificationMessage' instead." #-}

instance Lude.ToJSON DirectMessageConfiguration where
  toJSON DirectMessageConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("APNSMessage" Lude..=) Lude.<$> apnsMessage,
            ("GCMMessage" Lude..=) Lude.<$> gcmMessage,
            ("DefaultMessage" Lude..=) Lude.<$> defaultMessage,
            ("ADMMessage" Lude..=) Lude.<$> aDMMessage,
            ("SMSMessage" Lude..=) Lude.<$> sMSMessage,
            ("EmailMessage" Lude..=) Lude.<$> emailMessage,
            ("VoiceMessage" Lude..=) Lude.<$> voiceMessage,
            ("BaiduMessage" Lude..=) Lude.<$> baiduMessage,
            ("DefaultPushNotificationMessage" Lude..=)
              Lude.<$> defaultPushNotificationMessage
          ]
      )
