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
    mcADMMessage,
    mcAPNSMessage,
    mcBaiduMessage,
    mcCustomMessage,
    mcDefaultMessage,
    mcEmailMessage,
    mcGCMMessage,
    mcSMSMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.CampaignCustomMessage as Types
import qualified Network.AWS.Pinpoint.Types.CampaignEmailMessage as Types
import qualified Network.AWS.Pinpoint.Types.CampaignSmsMessage as Types
import qualified Network.AWS.Pinpoint.Types.Message as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the message configuration settings for a campaign.
--
-- /See:/ 'mkMessageConfiguration' smart constructor.
data MessageConfiguration = MessageConfiguration'
  { -- | The message that the campaign sends through the ADM (Amazon Device Messaging) channel. If specified, this message overrides the default message.
    aDMMessage :: Core.Maybe Types.Message,
    -- | The message that the campaign sends through the APNs (Apple Push Notification service) channel. If specified, this message overrides the default message.
    aPNSMessage :: Core.Maybe Types.Message,
    -- | The message that the campaign sends through the Baidu (Baidu Cloud Push) channel. If specified, this message overrides the default message.
    baiduMessage :: Core.Maybe Types.Message,
    -- | The message that the campaign sends through a custom channel, as specified by the delivery configuration (CustomDeliveryConfiguration) settings for the campaign. If specified, this message overrides the default message.
    customMessage :: Core.Maybe Types.CampaignCustomMessage,
    -- | The default message that the campaign sends through all the channels that are configured for the campaign.
    defaultMessage :: Core.Maybe Types.Message,
    -- | The message that the campaign sends through the email channel. If specified, this message overrides the default message.
    emailMessage :: Core.Maybe Types.CampaignEmailMessage,
    -- | The message that the campaign sends through the GCM channel, which enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. If specified, this message overrides the default message.
    gCMMessage :: Core.Maybe Types.Message,
    -- | The message that the campaign sends through the SMS channel. If specified, this message overrides the default message.
    sMSMessage :: Core.Maybe Types.CampaignSmsMessage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MessageConfiguration' value with any optional fields omitted.
mkMessageConfiguration ::
  MessageConfiguration
mkMessageConfiguration =
  MessageConfiguration'
    { aDMMessage = Core.Nothing,
      aPNSMessage = Core.Nothing,
      baiduMessage = Core.Nothing,
      customMessage = Core.Nothing,
      defaultMessage = Core.Nothing,
      emailMessage = Core.Nothing,
      gCMMessage = Core.Nothing,
      sMSMessage = Core.Nothing
    }

-- | The message that the campaign sends through the ADM (Amazon Device Messaging) channel. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'aDMMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcADMMessage :: Lens.Lens' MessageConfiguration (Core.Maybe Types.Message)
mcADMMessage = Lens.field @"aDMMessage"
{-# DEPRECATED mcADMMessage "Use generic-lens or generic-optics with 'aDMMessage' instead." #-}

-- | The message that the campaign sends through the APNs (Apple Push Notification service) channel. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'aPNSMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcAPNSMessage :: Lens.Lens' MessageConfiguration (Core.Maybe Types.Message)
mcAPNSMessage = Lens.field @"aPNSMessage"
{-# DEPRECATED mcAPNSMessage "Use generic-lens or generic-optics with 'aPNSMessage' instead." #-}

-- | The message that the campaign sends through the Baidu (Baidu Cloud Push) channel. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'baiduMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcBaiduMessage :: Lens.Lens' MessageConfiguration (Core.Maybe Types.Message)
mcBaiduMessage = Lens.field @"baiduMessage"
{-# DEPRECATED mcBaiduMessage "Use generic-lens or generic-optics with 'baiduMessage' instead." #-}

-- | The message that the campaign sends through a custom channel, as specified by the delivery configuration (CustomDeliveryConfiguration) settings for the campaign. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'customMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcCustomMessage :: Lens.Lens' MessageConfiguration (Core.Maybe Types.CampaignCustomMessage)
mcCustomMessage = Lens.field @"customMessage"
{-# DEPRECATED mcCustomMessage "Use generic-lens or generic-optics with 'customMessage' instead." #-}

-- | The default message that the campaign sends through all the channels that are configured for the campaign.
--
-- /Note:/ Consider using 'defaultMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcDefaultMessage :: Lens.Lens' MessageConfiguration (Core.Maybe Types.Message)
mcDefaultMessage = Lens.field @"defaultMessage"
{-# DEPRECATED mcDefaultMessage "Use generic-lens or generic-optics with 'defaultMessage' instead." #-}

-- | The message that the campaign sends through the email channel. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'emailMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcEmailMessage :: Lens.Lens' MessageConfiguration (Core.Maybe Types.CampaignEmailMessage)
mcEmailMessage = Lens.field @"emailMessage"
{-# DEPRECATED mcEmailMessage "Use generic-lens or generic-optics with 'emailMessage' instead." #-}

-- | The message that the campaign sends through the GCM channel, which enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'gCMMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcGCMMessage :: Lens.Lens' MessageConfiguration (Core.Maybe Types.Message)
mcGCMMessage = Lens.field @"gCMMessage"
{-# DEPRECATED mcGCMMessage "Use generic-lens or generic-optics with 'gCMMessage' instead." #-}

-- | The message that the campaign sends through the SMS channel. If specified, this message overrides the default message.
--
-- /Note:/ Consider using 'sMSMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcSMSMessage :: Lens.Lens' MessageConfiguration (Core.Maybe Types.CampaignSmsMessage)
mcSMSMessage = Lens.field @"sMSMessage"
{-# DEPRECATED mcSMSMessage "Use generic-lens or generic-optics with 'sMSMessage' instead." #-}

instance Core.FromJSON MessageConfiguration where
  toJSON MessageConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("ADMMessage" Core..=) Core.<$> aDMMessage,
            ("APNSMessage" Core..=) Core.<$> aPNSMessage,
            ("BaiduMessage" Core..=) Core.<$> baiduMessage,
            ("CustomMessage" Core..=) Core.<$> customMessage,
            ("DefaultMessage" Core..=) Core.<$> defaultMessage,
            ("EmailMessage" Core..=) Core.<$> emailMessage,
            ("GCMMessage" Core..=) Core.<$> gCMMessage,
            ("SMSMessage" Core..=) Core.<$> sMSMessage
          ]
      )

instance Core.FromJSON MessageConfiguration where
  parseJSON =
    Core.withObject "MessageConfiguration" Core.$
      \x ->
        MessageConfiguration'
          Core.<$> (x Core..:? "ADMMessage")
          Core.<*> (x Core..:? "APNSMessage")
          Core.<*> (x Core..:? "BaiduMessage")
          Core.<*> (x Core..:? "CustomMessage")
          Core.<*> (x Core..:? "DefaultMessage")
          Core.<*> (x Core..:? "EmailMessage")
          Core.<*> (x Core..:? "GCMMessage")
          Core.<*> (x Core..:? "SMSMessage")
