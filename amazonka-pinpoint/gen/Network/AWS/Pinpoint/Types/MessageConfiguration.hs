{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignCustomMessage
import Network.AWS.Pinpoint.Types.CampaignEmailMessage
import Network.AWS.Pinpoint.Types.CampaignSmsMessage
import Network.AWS.Pinpoint.Types.Message

-- | Specifies the message configuration settings for a campaign.
--
-- /See:/ 'newMessageConfiguration' smart constructor.
data MessageConfiguration = MessageConfiguration'
  { -- | The message that the campaign sends through the ADM (Amazon Device
    -- Messaging) channel. If specified, this message overrides the default
    -- message.
    aDMMessage :: Core.Maybe Message,
    -- | The default message that the campaign sends through all the channels
    -- that are configured for the campaign.
    defaultMessage :: Core.Maybe Message,
    -- | The message that the campaign sends through the GCM channel, which
    -- enables Amazon Pinpoint to send push notifications through the Firebase
    -- Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
    -- If specified, this message overrides the default message.
    gCMMessage :: Core.Maybe Message,
    -- | The message that the campaign sends through the APNs (Apple Push
    -- Notification service) channel. If specified, this message overrides the
    -- default message.
    aPNSMessage :: Core.Maybe Message,
    -- | The message that the campaign sends through the email channel. If
    -- specified, this message overrides the default message.
    emailMessage :: Core.Maybe CampaignEmailMessage,
    -- | The message that the campaign sends through the SMS channel. If
    -- specified, this message overrides the default message.
    sMSMessage :: Core.Maybe CampaignSmsMessage,
    -- | The message that the campaign sends through the Baidu (Baidu Cloud Push)
    -- channel. If specified, this message overrides the default message.
    baiduMessage :: Core.Maybe Message,
    -- | The message that the campaign sends through a custom channel, as
    -- specified by the delivery configuration (CustomDeliveryConfiguration)
    -- settings for the campaign. If specified, this message overrides the
    -- default message.
    customMessage :: Core.Maybe CampaignCustomMessage
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MessageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aDMMessage', 'messageConfiguration_aDMMessage' - The message that the campaign sends through the ADM (Amazon Device
-- Messaging) channel. If specified, this message overrides the default
-- message.
--
-- 'defaultMessage', 'messageConfiguration_defaultMessage' - The default message that the campaign sends through all the channels
-- that are configured for the campaign.
--
-- 'gCMMessage', 'messageConfiguration_gCMMessage' - The message that the campaign sends through the GCM channel, which
-- enables Amazon Pinpoint to send push notifications through the Firebase
-- Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
-- If specified, this message overrides the default message.
--
-- 'aPNSMessage', 'messageConfiguration_aPNSMessage' - The message that the campaign sends through the APNs (Apple Push
-- Notification service) channel. If specified, this message overrides the
-- default message.
--
-- 'emailMessage', 'messageConfiguration_emailMessage' - The message that the campaign sends through the email channel. If
-- specified, this message overrides the default message.
--
-- 'sMSMessage', 'messageConfiguration_sMSMessage' - The message that the campaign sends through the SMS channel. If
-- specified, this message overrides the default message.
--
-- 'baiduMessage', 'messageConfiguration_baiduMessage' - The message that the campaign sends through the Baidu (Baidu Cloud Push)
-- channel. If specified, this message overrides the default message.
--
-- 'customMessage', 'messageConfiguration_customMessage' - The message that the campaign sends through a custom channel, as
-- specified by the delivery configuration (CustomDeliveryConfiguration)
-- settings for the campaign. If specified, this message overrides the
-- default message.
newMessageConfiguration ::
  MessageConfiguration
newMessageConfiguration =
  MessageConfiguration'
    { aDMMessage = Core.Nothing,
      defaultMessage = Core.Nothing,
      gCMMessage = Core.Nothing,
      aPNSMessage = Core.Nothing,
      emailMessage = Core.Nothing,
      sMSMessage = Core.Nothing,
      baiduMessage = Core.Nothing,
      customMessage = Core.Nothing
    }

-- | The message that the campaign sends through the ADM (Amazon Device
-- Messaging) channel. If specified, this message overrides the default
-- message.
messageConfiguration_aDMMessage :: Lens.Lens' MessageConfiguration (Core.Maybe Message)
messageConfiguration_aDMMessage = Lens.lens (\MessageConfiguration' {aDMMessage} -> aDMMessage) (\s@MessageConfiguration' {} a -> s {aDMMessage = a} :: MessageConfiguration)

-- | The default message that the campaign sends through all the channels
-- that are configured for the campaign.
messageConfiguration_defaultMessage :: Lens.Lens' MessageConfiguration (Core.Maybe Message)
messageConfiguration_defaultMessage = Lens.lens (\MessageConfiguration' {defaultMessage} -> defaultMessage) (\s@MessageConfiguration' {} a -> s {defaultMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the GCM channel, which
-- enables Amazon Pinpoint to send push notifications through the Firebase
-- Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
-- If specified, this message overrides the default message.
messageConfiguration_gCMMessage :: Lens.Lens' MessageConfiguration (Core.Maybe Message)
messageConfiguration_gCMMessage = Lens.lens (\MessageConfiguration' {gCMMessage} -> gCMMessage) (\s@MessageConfiguration' {} a -> s {gCMMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the APNs (Apple Push
-- Notification service) channel. If specified, this message overrides the
-- default message.
messageConfiguration_aPNSMessage :: Lens.Lens' MessageConfiguration (Core.Maybe Message)
messageConfiguration_aPNSMessage = Lens.lens (\MessageConfiguration' {aPNSMessage} -> aPNSMessage) (\s@MessageConfiguration' {} a -> s {aPNSMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the email channel. If
-- specified, this message overrides the default message.
messageConfiguration_emailMessage :: Lens.Lens' MessageConfiguration (Core.Maybe CampaignEmailMessage)
messageConfiguration_emailMessage = Lens.lens (\MessageConfiguration' {emailMessage} -> emailMessage) (\s@MessageConfiguration' {} a -> s {emailMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the SMS channel. If
-- specified, this message overrides the default message.
messageConfiguration_sMSMessage :: Lens.Lens' MessageConfiguration (Core.Maybe CampaignSmsMessage)
messageConfiguration_sMSMessage = Lens.lens (\MessageConfiguration' {sMSMessage} -> sMSMessage) (\s@MessageConfiguration' {} a -> s {sMSMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the Baidu (Baidu Cloud Push)
-- channel. If specified, this message overrides the default message.
messageConfiguration_baiduMessage :: Lens.Lens' MessageConfiguration (Core.Maybe Message)
messageConfiguration_baiduMessage = Lens.lens (\MessageConfiguration' {baiduMessage} -> baiduMessage) (\s@MessageConfiguration' {} a -> s {baiduMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through a custom channel, as
-- specified by the delivery configuration (CustomDeliveryConfiguration)
-- settings for the campaign. If specified, this message overrides the
-- default message.
messageConfiguration_customMessage :: Lens.Lens' MessageConfiguration (Core.Maybe CampaignCustomMessage)
messageConfiguration_customMessage = Lens.lens (\MessageConfiguration' {customMessage} -> customMessage) (\s@MessageConfiguration' {} a -> s {customMessage = a} :: MessageConfiguration)

instance Core.FromJSON MessageConfiguration where
  parseJSON =
    Core.withObject
      "MessageConfiguration"
      ( \x ->
          MessageConfiguration'
            Core.<$> (x Core..:? "ADMMessage")
            Core.<*> (x Core..:? "DefaultMessage")
            Core.<*> (x Core..:? "GCMMessage")
            Core.<*> (x Core..:? "APNSMessage")
            Core.<*> (x Core..:? "EmailMessage")
            Core.<*> (x Core..:? "SMSMessage")
            Core.<*> (x Core..:? "BaiduMessage")
            Core.<*> (x Core..:? "CustomMessage")
      )

instance Core.Hashable MessageConfiguration

instance Core.NFData MessageConfiguration

instance Core.ToJSON MessageConfiguration where
  toJSON MessageConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ADMMessage" Core..=) Core.<$> aDMMessage,
            ("DefaultMessage" Core..=) Core.<$> defaultMessage,
            ("GCMMessage" Core..=) Core.<$> gCMMessage,
            ("APNSMessage" Core..=) Core.<$> aPNSMessage,
            ("EmailMessage" Core..=) Core.<$> emailMessage,
            ("SMSMessage" Core..=) Core.<$> sMSMessage,
            ("BaiduMessage" Core..=) Core.<$> baiduMessage,
            ("CustomMessage" Core..=) Core.<$> customMessage
          ]
      )
