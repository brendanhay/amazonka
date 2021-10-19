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
import Network.AWS.Pinpoint.Types.CampaignInAppMessage
import Network.AWS.Pinpoint.Types.CampaignSmsMessage
import Network.AWS.Pinpoint.Types.Message
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the message configuration settings for a campaign.
--
-- /See:/ 'newMessageConfiguration' smart constructor.
data MessageConfiguration = MessageConfiguration'
  { -- | The message that the campaign sends through the APNs (Apple Push
    -- Notification service) channel. If specified, this message overrides the
    -- default message.
    aPNSMessage :: Prelude.Maybe Message,
    -- | The message that the campaign sends through the GCM channel, which
    -- enables Amazon Pinpoint to send push notifications through the Firebase
    -- Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
    -- If specified, this message overrides the default message.
    gCMMessage :: Prelude.Maybe Message,
    -- | The default message that the campaign sends through all the channels
    -- that are configured for the campaign.
    defaultMessage :: Prelude.Maybe Message,
    -- | The message that the campaign sends through a custom channel, as
    -- specified by the delivery configuration (CustomDeliveryConfiguration)
    -- settings for the campaign. If specified, this message overrides the
    -- default message.
    customMessage :: Prelude.Maybe CampaignCustomMessage,
    -- | The message that the campaign sends through the ADM (Amazon Device
    -- Messaging) channel. If specified, this message overrides the default
    -- message.
    aDMMessage :: Prelude.Maybe Message,
    -- | The message that the campaign sends through the SMS channel. If
    -- specified, this message overrides the default message.
    sMSMessage :: Prelude.Maybe CampaignSmsMessage,
    -- | The message that the campaign sends through the email channel. If
    -- specified, this message overrides the default message.
    emailMessage :: Prelude.Maybe CampaignEmailMessage,
    -- | The in-app message configuration.
    inAppMessage :: Prelude.Maybe CampaignInAppMessage,
    -- | The message that the campaign sends through the Baidu (Baidu Cloud Push)
    -- channel. If specified, this message overrides the default message.
    baiduMessage :: Prelude.Maybe Message
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aPNSMessage', 'messageConfiguration_aPNSMessage' - The message that the campaign sends through the APNs (Apple Push
-- Notification service) channel. If specified, this message overrides the
-- default message.
--
-- 'gCMMessage', 'messageConfiguration_gCMMessage' - The message that the campaign sends through the GCM channel, which
-- enables Amazon Pinpoint to send push notifications through the Firebase
-- Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
-- If specified, this message overrides the default message.
--
-- 'defaultMessage', 'messageConfiguration_defaultMessage' - The default message that the campaign sends through all the channels
-- that are configured for the campaign.
--
-- 'customMessage', 'messageConfiguration_customMessage' - The message that the campaign sends through a custom channel, as
-- specified by the delivery configuration (CustomDeliveryConfiguration)
-- settings for the campaign. If specified, this message overrides the
-- default message.
--
-- 'aDMMessage', 'messageConfiguration_aDMMessage' - The message that the campaign sends through the ADM (Amazon Device
-- Messaging) channel. If specified, this message overrides the default
-- message.
--
-- 'sMSMessage', 'messageConfiguration_sMSMessage' - The message that the campaign sends through the SMS channel. If
-- specified, this message overrides the default message.
--
-- 'emailMessage', 'messageConfiguration_emailMessage' - The message that the campaign sends through the email channel. If
-- specified, this message overrides the default message.
--
-- 'inAppMessage', 'messageConfiguration_inAppMessage' - The in-app message configuration.
--
-- 'baiduMessage', 'messageConfiguration_baiduMessage' - The message that the campaign sends through the Baidu (Baidu Cloud Push)
-- channel. If specified, this message overrides the default message.
newMessageConfiguration ::
  MessageConfiguration
newMessageConfiguration =
  MessageConfiguration'
    { aPNSMessage =
        Prelude.Nothing,
      gCMMessage = Prelude.Nothing,
      defaultMessage = Prelude.Nothing,
      customMessage = Prelude.Nothing,
      aDMMessage = Prelude.Nothing,
      sMSMessage = Prelude.Nothing,
      emailMessage = Prelude.Nothing,
      inAppMessage = Prelude.Nothing,
      baiduMessage = Prelude.Nothing
    }

-- | The message that the campaign sends through the APNs (Apple Push
-- Notification service) channel. If specified, this message overrides the
-- default message.
messageConfiguration_aPNSMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_aPNSMessage = Lens.lens (\MessageConfiguration' {aPNSMessage} -> aPNSMessage) (\s@MessageConfiguration' {} a -> s {aPNSMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the GCM channel, which
-- enables Amazon Pinpoint to send push notifications through the Firebase
-- Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
-- If specified, this message overrides the default message.
messageConfiguration_gCMMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_gCMMessage = Lens.lens (\MessageConfiguration' {gCMMessage} -> gCMMessage) (\s@MessageConfiguration' {} a -> s {gCMMessage = a} :: MessageConfiguration)

-- | The default message that the campaign sends through all the channels
-- that are configured for the campaign.
messageConfiguration_defaultMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_defaultMessage = Lens.lens (\MessageConfiguration' {defaultMessage} -> defaultMessage) (\s@MessageConfiguration' {} a -> s {defaultMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through a custom channel, as
-- specified by the delivery configuration (CustomDeliveryConfiguration)
-- settings for the campaign. If specified, this message overrides the
-- default message.
messageConfiguration_customMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe CampaignCustomMessage)
messageConfiguration_customMessage = Lens.lens (\MessageConfiguration' {customMessage} -> customMessage) (\s@MessageConfiguration' {} a -> s {customMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the ADM (Amazon Device
-- Messaging) channel. If specified, this message overrides the default
-- message.
messageConfiguration_aDMMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_aDMMessage = Lens.lens (\MessageConfiguration' {aDMMessage} -> aDMMessage) (\s@MessageConfiguration' {} a -> s {aDMMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the SMS channel. If
-- specified, this message overrides the default message.
messageConfiguration_sMSMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe CampaignSmsMessage)
messageConfiguration_sMSMessage = Lens.lens (\MessageConfiguration' {sMSMessage} -> sMSMessage) (\s@MessageConfiguration' {} a -> s {sMSMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the email channel. If
-- specified, this message overrides the default message.
messageConfiguration_emailMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe CampaignEmailMessage)
messageConfiguration_emailMessage = Lens.lens (\MessageConfiguration' {emailMessage} -> emailMessage) (\s@MessageConfiguration' {} a -> s {emailMessage = a} :: MessageConfiguration)

-- | The in-app message configuration.
messageConfiguration_inAppMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe CampaignInAppMessage)
messageConfiguration_inAppMessage = Lens.lens (\MessageConfiguration' {inAppMessage} -> inAppMessage) (\s@MessageConfiguration' {} a -> s {inAppMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the Baidu (Baidu Cloud Push)
-- channel. If specified, this message overrides the default message.
messageConfiguration_baiduMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_baiduMessage = Lens.lens (\MessageConfiguration' {baiduMessage} -> baiduMessage) (\s@MessageConfiguration' {} a -> s {baiduMessage = a} :: MessageConfiguration)

instance Core.FromJSON MessageConfiguration where
  parseJSON =
    Core.withObject
      "MessageConfiguration"
      ( \x ->
          MessageConfiguration'
            Prelude.<$> (x Core..:? "APNSMessage")
            Prelude.<*> (x Core..:? "GCMMessage")
            Prelude.<*> (x Core..:? "DefaultMessage")
            Prelude.<*> (x Core..:? "CustomMessage")
            Prelude.<*> (x Core..:? "ADMMessage")
            Prelude.<*> (x Core..:? "SMSMessage")
            Prelude.<*> (x Core..:? "EmailMessage")
            Prelude.<*> (x Core..:? "InAppMessage")
            Prelude.<*> (x Core..:? "BaiduMessage")
      )

instance Prelude.Hashable MessageConfiguration

instance Prelude.NFData MessageConfiguration

instance Core.ToJSON MessageConfiguration where
  toJSON MessageConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("APNSMessage" Core..=) Prelude.<$> aPNSMessage,
            ("GCMMessage" Core..=) Prelude.<$> gCMMessage,
            ("DefaultMessage" Core..=)
              Prelude.<$> defaultMessage,
            ("CustomMessage" Core..=) Prelude.<$> customMessage,
            ("ADMMessage" Core..=) Prelude.<$> aDMMessage,
            ("SMSMessage" Core..=) Prelude.<$> sMSMessage,
            ("EmailMessage" Core..=) Prelude.<$> emailMessage,
            ("InAppMessage" Core..=) Prelude.<$> inAppMessage,
            ("BaiduMessage" Core..=) Prelude.<$> baiduMessage
          ]
      )
