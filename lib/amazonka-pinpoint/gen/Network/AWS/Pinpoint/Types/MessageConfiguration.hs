{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageConfiguration
  ( MessageConfiguration (..),

    -- * Smart constructor
    mkMessageConfiguration,

    -- * Lenses
    mcAPNSMessage,
    mcGCMMessage,
    mcDefaultMessage,
    mcCustomMessage,
    mcADMMessage,
    mcSMSMessage,
    mcEmailMessage,
    mcBaiduMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignCustomMessage
import Network.AWS.Pinpoint.Types.CampaignEmailMessage
import Network.AWS.Pinpoint.Types.CampaignSmsMessage
import Network.AWS.Pinpoint.Types.Message
import qualified Network.AWS.Prelude as Lude

-- | Specifies the message configuration settings for a campaign.
--
-- /See:/ 'mkMessageConfiguration' smart constructor.
data MessageConfiguration = MessageConfiguration'
  { -- | The message that the campaign sends through the APNs (Apple Push Notification service) channel. If specified, this message overrides the default message.
    apnsMessage :: Lude.Maybe Message,
    -- | The message that the campaign sends through the GCM channel, which enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. If specified, this message overrides the default message.
    gcmMessage :: Lude.Maybe Message,
    -- | The default message that the campaign sends through all the channels that are configured for the campaign.
    defaultMessage :: Lude.Maybe Message,
    -- | The message that the campaign sends through a custom channel, as specified by the delivery configuration (CustomDeliveryConfiguration) settings for the campaign. If specified, this message overrides the default message.
    customMessage :: Lude.Maybe CampaignCustomMessage,
    -- | The message that the campaign sends through the ADM (Amazon Device Messaging) channel. If specified, this message overrides the default message.
    aDMMessage :: Lude.Maybe Message,
    -- | The message that the campaign sends through the SMS channel. If specified, this message overrides the default message.
    sMSMessage :: Lude.Maybe CampaignSmsMessage,
    -- | The message that the campaign sends through the email channel. If specified, this message overrides the default message.
    emailMessage :: Lude.Maybe CampaignEmailMessage,
    -- | The message that the campaign sends through the Baidu (Baidu Cloud Push) channel. If specified, this message overrides the default message.
    baiduMessage :: Lude.Maybe Message
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MessageConfiguration' with the minimum fields required to make a request.
--
-- * 'apnsMessage' - The message that the campaign sends through the APNs (Apple Push Notification service) channel. If specified, this message overrides the default message.
-- * 'gcmMessage' - The message that the campaign sends through the GCM channel, which enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. If specified, this message overrides the default message.
-- * 'defaultMessage' - The default message that the campaign sends through all the channels that are configured for the campaign.
-- * 'customMessage' - The message that the campaign sends through a custom channel, as specified by the delivery configuration (CustomDeliveryConfiguration) settings for the campaign. If specified, this message overrides the default message.
-- * 'aDMMessage' - The message that the campaign sends through the ADM (Amazon Device Messaging) channel. If specified, this message overrides the default message.
-- * 'sMSMessage' - The message that the campaign sends through the SMS channel. If specified, this message overrides the default message.
-- * 'emailMessage' - The message that the campaign sends through the email channel. If specified, this message overrides the default message.
-- * 'baiduMessage' - The message that the campaign sends through the Baidu (Baidu Cloud Push) channel. If specified, this message overrides the default message.
mkMessageConfiguration ::
  MessageConfiguration
mkMessageConfiguration =
  MessageConfiguration'
    { apnsMessage = Lude.Nothing,
      gcmMessage = Lude.Nothing,
      defaultMessage = Lude.Nothing,
      customMessage = Lude.Nothing,
      aDMMessage = Lude.Nothing,
      sMSMessage = Lude.Nothing,
      emailMessage = Lude.Nothing,
      baiduMessage = Lude.Nothing
    }

-- | The message that the campaign sends through the APNs (Apple Push Notification service) channel. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'apnsMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcAPNSMessage :: Lens.Lens' MessageConfiguration (Lude.Maybe Message)
mcAPNSMessage = Lens.lens (apnsMessage :: MessageConfiguration -> Lude.Maybe Message) (\s a -> s {apnsMessage = a} :: MessageConfiguration)
{-# DEPRECATED mcAPNSMessage "Use generic-lens or generic-optics with 'apnsMessage' instead." #-}

-- | The message that the campaign sends through the GCM channel, which enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'gcmMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcGCMMessage :: Lens.Lens' MessageConfiguration (Lude.Maybe Message)
mcGCMMessage = Lens.lens (gcmMessage :: MessageConfiguration -> Lude.Maybe Message) (\s a -> s {gcmMessage = a} :: MessageConfiguration)
{-# DEPRECATED mcGCMMessage "Use generic-lens or generic-optics with 'gcmMessage' instead." #-}

-- | The default message that the campaign sends through all the channels that are configured for the campaign.
--
-- /Note:/ Consider using 'defaultMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcDefaultMessage :: Lens.Lens' MessageConfiguration (Lude.Maybe Message)
mcDefaultMessage = Lens.lens (defaultMessage :: MessageConfiguration -> Lude.Maybe Message) (\s a -> s {defaultMessage = a} :: MessageConfiguration)
{-# DEPRECATED mcDefaultMessage "Use generic-lens or generic-optics with 'defaultMessage' instead." #-}

-- | The message that the campaign sends through a custom channel, as specified by the delivery configuration (CustomDeliveryConfiguration) settings for the campaign. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'customMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcCustomMessage :: Lens.Lens' MessageConfiguration (Lude.Maybe CampaignCustomMessage)
mcCustomMessage = Lens.lens (customMessage :: MessageConfiguration -> Lude.Maybe CampaignCustomMessage) (\s a -> s {customMessage = a} :: MessageConfiguration)
{-# DEPRECATED mcCustomMessage "Use generic-lens or generic-optics with 'customMessage' instead." #-}

-- | The message that the campaign sends through the ADM (Amazon Device Messaging) channel. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'aDMMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcADMMessage :: Lens.Lens' MessageConfiguration (Lude.Maybe Message)
mcADMMessage = Lens.lens (aDMMessage :: MessageConfiguration -> Lude.Maybe Message) (\s a -> s {aDMMessage = a} :: MessageConfiguration)
{-# DEPRECATED mcADMMessage "Use generic-lens or generic-optics with 'aDMMessage' instead." #-}

-- | The message that the campaign sends through the SMS channel. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'sMSMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcSMSMessage :: Lens.Lens' MessageConfiguration (Lude.Maybe CampaignSmsMessage)
mcSMSMessage = Lens.lens (sMSMessage :: MessageConfiguration -> Lude.Maybe CampaignSmsMessage) (\s a -> s {sMSMessage = a} :: MessageConfiguration)
{-# DEPRECATED mcSMSMessage "Use generic-lens or generic-optics with 'sMSMessage' instead." #-}

-- | The message that the campaign sends through the email channel. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'emailMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcEmailMessage :: Lens.Lens' MessageConfiguration (Lude.Maybe CampaignEmailMessage)
mcEmailMessage = Lens.lens (emailMessage :: MessageConfiguration -> Lude.Maybe CampaignEmailMessage) (\s a -> s {emailMessage = a} :: MessageConfiguration)
{-# DEPRECATED mcEmailMessage "Use generic-lens or generic-optics with 'emailMessage' instead." #-}

-- | The message that the campaign sends through the Baidu (Baidu Cloud Push) channel. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'baiduMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcBaiduMessage :: Lens.Lens' MessageConfiguration (Lude.Maybe Message)
mcBaiduMessage = Lens.lens (baiduMessage :: MessageConfiguration -> Lude.Maybe Message) (\s a -> s {baiduMessage = a} :: MessageConfiguration)
{-# DEPRECATED mcBaiduMessage "Use generic-lens or generic-optics with 'baiduMessage' instead." #-}

instance Lude.FromJSON MessageConfiguration where
  parseJSON =
    Lude.withObject
      "MessageConfiguration"
      ( \x ->
          MessageConfiguration'
            Lude.<$> (x Lude..:? "APNSMessage")
            Lude.<*> (x Lude..:? "GCMMessage")
            Lude.<*> (x Lude..:? "DefaultMessage")
            Lude.<*> (x Lude..:? "CustomMessage")
            Lude.<*> (x Lude..:? "ADMMessage")
            Lude.<*> (x Lude..:? "SMSMessage")
            Lude.<*> (x Lude..:? "EmailMessage")
            Lude.<*> (x Lude..:? "BaiduMessage")
      )

instance Lude.ToJSON MessageConfiguration where
  toJSON MessageConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("APNSMessage" Lude..=) Lude.<$> apnsMessage,
            ("GCMMessage" Lude..=) Lude.<$> gcmMessage,
            ("DefaultMessage" Lude..=) Lude.<$> defaultMessage,
            ("CustomMessage" Lude..=) Lude.<$> customMessage,
            ("ADMMessage" Lude..=) Lude.<$> aDMMessage,
            ("SMSMessage" Lude..=) Lude.<$> sMSMessage,
            ("EmailMessage" Lude..=) Lude.<$> emailMessage,
            ("BaiduMessage" Lude..=) Lude.<$> baiduMessage
          ]
      )
