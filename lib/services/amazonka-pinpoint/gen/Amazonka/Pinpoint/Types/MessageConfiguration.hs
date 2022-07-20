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
-- Module      : Amazonka.Pinpoint.Types.MessageConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.MessageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.CampaignCustomMessage
import Amazonka.Pinpoint.Types.CampaignEmailMessage
import Amazonka.Pinpoint.Types.CampaignInAppMessage
import Amazonka.Pinpoint.Types.CampaignSmsMessage
import Amazonka.Pinpoint.Types.Message
import qualified Amazonka.Prelude as Prelude

-- | Specifies the message configuration settings for a campaign.
--
-- /See:/ 'newMessageConfiguration' smart constructor.
data MessageConfiguration = MessageConfiguration'
  { -- | The message that the campaign sends through the ADM (Amazon Device
    -- Messaging) channel. If specified, this message overrides the default
    -- message.
    aDMMessage :: Prelude.Maybe Message,
    -- | The message that the campaign sends through the SMS channel. If
    -- specified, this message overrides the default message.
    sMSMessage :: Prelude.Maybe CampaignSmsMessage,
    -- | The in-app message configuration.
    inAppMessage :: Prelude.Maybe CampaignInAppMessage,
    -- | The default message that the campaign sends through all the channels
    -- that are configured for the campaign.
    defaultMessage :: Prelude.Maybe Message,
    -- | The message that the campaign sends through the APNs (Apple Push
    -- Notification service) channel. If specified, this message overrides the
    -- default message.
    aPNSMessage :: Prelude.Maybe Message,
    -- | The message that the campaign sends through a custom channel, as
    -- specified by the delivery configuration (CustomDeliveryConfiguration)
    -- settings for the campaign. If specified, this message overrides the
    -- default message.
    customMessage :: Prelude.Maybe CampaignCustomMessage,
    -- | The message that the campaign sends through the GCM channel, which
    -- enables Amazon Pinpoint to send push notifications through the Firebase
    -- Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
    -- If specified, this message overrides the default message.
    gCMMessage :: Prelude.Maybe Message,
    -- | The message that the campaign sends through the email channel. If
    -- specified, this message overrides the default message.
    emailMessage :: Prelude.Maybe CampaignEmailMessage,
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
-- 'aDMMessage', 'messageConfiguration_aDMMessage' - The message that the campaign sends through the ADM (Amazon Device
-- Messaging) channel. If specified, this message overrides the default
-- message.
--
-- 'sMSMessage', 'messageConfiguration_sMSMessage' - The message that the campaign sends through the SMS channel. If
-- specified, this message overrides the default message.
--
-- 'inAppMessage', 'messageConfiguration_inAppMessage' - The in-app message configuration.
--
-- 'defaultMessage', 'messageConfiguration_defaultMessage' - The default message that the campaign sends through all the channels
-- that are configured for the campaign.
--
-- 'aPNSMessage', 'messageConfiguration_aPNSMessage' - The message that the campaign sends through the APNs (Apple Push
-- Notification service) channel. If specified, this message overrides the
-- default message.
--
-- 'customMessage', 'messageConfiguration_customMessage' - The message that the campaign sends through a custom channel, as
-- specified by the delivery configuration (CustomDeliveryConfiguration)
-- settings for the campaign. If specified, this message overrides the
-- default message.
--
-- 'gCMMessage', 'messageConfiguration_gCMMessage' - The message that the campaign sends through the GCM channel, which
-- enables Amazon Pinpoint to send push notifications through the Firebase
-- Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
-- If specified, this message overrides the default message.
--
-- 'emailMessage', 'messageConfiguration_emailMessage' - The message that the campaign sends through the email channel. If
-- specified, this message overrides the default message.
--
-- 'baiduMessage', 'messageConfiguration_baiduMessage' - The message that the campaign sends through the Baidu (Baidu Cloud Push)
-- channel. If specified, this message overrides the default message.
newMessageConfiguration ::
  MessageConfiguration
newMessageConfiguration =
  MessageConfiguration'
    { aDMMessage = Prelude.Nothing,
      sMSMessage = Prelude.Nothing,
      inAppMessage = Prelude.Nothing,
      defaultMessage = Prelude.Nothing,
      aPNSMessage = Prelude.Nothing,
      customMessage = Prelude.Nothing,
      gCMMessage = Prelude.Nothing,
      emailMessage = Prelude.Nothing,
      baiduMessage = Prelude.Nothing
    }

-- | The message that the campaign sends through the ADM (Amazon Device
-- Messaging) channel. If specified, this message overrides the default
-- message.
messageConfiguration_aDMMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_aDMMessage = Lens.lens (\MessageConfiguration' {aDMMessage} -> aDMMessage) (\s@MessageConfiguration' {} a -> s {aDMMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the SMS channel. If
-- specified, this message overrides the default message.
messageConfiguration_sMSMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe CampaignSmsMessage)
messageConfiguration_sMSMessage = Lens.lens (\MessageConfiguration' {sMSMessage} -> sMSMessage) (\s@MessageConfiguration' {} a -> s {sMSMessage = a} :: MessageConfiguration)

-- | The in-app message configuration.
messageConfiguration_inAppMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe CampaignInAppMessage)
messageConfiguration_inAppMessage = Lens.lens (\MessageConfiguration' {inAppMessage} -> inAppMessage) (\s@MessageConfiguration' {} a -> s {inAppMessage = a} :: MessageConfiguration)

-- | The default message that the campaign sends through all the channels
-- that are configured for the campaign.
messageConfiguration_defaultMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_defaultMessage = Lens.lens (\MessageConfiguration' {defaultMessage} -> defaultMessage) (\s@MessageConfiguration' {} a -> s {defaultMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the APNs (Apple Push
-- Notification service) channel. If specified, this message overrides the
-- default message.
messageConfiguration_aPNSMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_aPNSMessage = Lens.lens (\MessageConfiguration' {aPNSMessage} -> aPNSMessage) (\s@MessageConfiguration' {} a -> s {aPNSMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through a custom channel, as
-- specified by the delivery configuration (CustomDeliveryConfiguration)
-- settings for the campaign. If specified, this message overrides the
-- default message.
messageConfiguration_customMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe CampaignCustomMessage)
messageConfiguration_customMessage = Lens.lens (\MessageConfiguration' {customMessage} -> customMessage) (\s@MessageConfiguration' {} a -> s {customMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the GCM channel, which
-- enables Amazon Pinpoint to send push notifications through the Firebase
-- Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
-- If specified, this message overrides the default message.
messageConfiguration_gCMMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_gCMMessage = Lens.lens (\MessageConfiguration' {gCMMessage} -> gCMMessage) (\s@MessageConfiguration' {} a -> s {gCMMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the email channel. If
-- specified, this message overrides the default message.
messageConfiguration_emailMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe CampaignEmailMessage)
messageConfiguration_emailMessage = Lens.lens (\MessageConfiguration' {emailMessage} -> emailMessage) (\s@MessageConfiguration' {} a -> s {emailMessage = a} :: MessageConfiguration)

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
            Prelude.<$> (x Core..:? "ADMMessage")
            Prelude.<*> (x Core..:? "SMSMessage")
            Prelude.<*> (x Core..:? "InAppMessage")
            Prelude.<*> (x Core..:? "DefaultMessage")
            Prelude.<*> (x Core..:? "APNSMessage")
            Prelude.<*> (x Core..:? "CustomMessage")
            Prelude.<*> (x Core..:? "GCMMessage")
            Prelude.<*> (x Core..:? "EmailMessage")
            Prelude.<*> (x Core..:? "BaiduMessage")
      )

instance Prelude.Hashable MessageConfiguration where
  hashWithSalt _salt MessageConfiguration' {..} =
    _salt `Prelude.hashWithSalt` aDMMessage
      `Prelude.hashWithSalt` sMSMessage
      `Prelude.hashWithSalt` inAppMessage
      `Prelude.hashWithSalt` defaultMessage
      `Prelude.hashWithSalt` aPNSMessage
      `Prelude.hashWithSalt` customMessage
      `Prelude.hashWithSalt` gCMMessage
      `Prelude.hashWithSalt` emailMessage
      `Prelude.hashWithSalt` baiduMessage

instance Prelude.NFData MessageConfiguration where
  rnf MessageConfiguration' {..} =
    Prelude.rnf aDMMessage
      `Prelude.seq` Prelude.rnf sMSMessage
      `Prelude.seq` Prelude.rnf inAppMessage
      `Prelude.seq` Prelude.rnf defaultMessage
      `Prelude.seq` Prelude.rnf aPNSMessage
      `Prelude.seq` Prelude.rnf customMessage
      `Prelude.seq` Prelude.rnf gCMMessage
      `Prelude.seq` Prelude.rnf emailMessage
      `Prelude.seq` Prelude.rnf baiduMessage

instance Core.ToJSON MessageConfiguration where
  toJSON MessageConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ADMMessage" Core..=) Prelude.<$> aDMMessage,
            ("SMSMessage" Core..=) Prelude.<$> sMSMessage,
            ("InAppMessage" Core..=) Prelude.<$> inAppMessage,
            ("DefaultMessage" Core..=)
              Prelude.<$> defaultMessage,
            ("APNSMessage" Core..=) Prelude.<$> aPNSMessage,
            ("CustomMessage" Core..=) Prelude.<$> customMessage,
            ("GCMMessage" Core..=) Prelude.<$> gCMMessage,
            ("EmailMessage" Core..=) Prelude.<$> emailMessage,
            ("BaiduMessage" Core..=) Prelude.<$> baiduMessage
          ]
      )
