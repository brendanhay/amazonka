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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.MessageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- | The message that the campaign sends through the APNs (Apple Push
    -- Notification service) channel. If specified, this message overrides the
    -- default message.
    aPNSMessage :: Prelude.Maybe Message,
    -- | The message that the campaign sends through the Baidu (Baidu Cloud Push)
    -- channel. If specified, this message overrides the default message.
    baiduMessage :: Prelude.Maybe Message,
    -- | The message that the campaign sends through a custom channel, as
    -- specified by the delivery configuration (CustomDeliveryConfiguration)
    -- settings for the campaign. If specified, this message overrides the
    -- default message.
    customMessage :: Prelude.Maybe CampaignCustomMessage,
    -- | The default message that the campaign sends through all the channels
    -- that are configured for the campaign.
    defaultMessage :: Prelude.Maybe Message,
    -- | The message that the campaign sends through the email channel. If
    -- specified, this message overrides the default message.
    emailMessage :: Prelude.Maybe CampaignEmailMessage,
    -- | The message that the campaign sends through the GCM channel, which
    -- enables Amazon Pinpoint to send push notifications through the Firebase
    -- Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
    -- If specified, this message overrides the default message.
    gCMMessage :: Prelude.Maybe Message,
    -- | The in-app message configuration.
    inAppMessage :: Prelude.Maybe CampaignInAppMessage,
    -- | The message that the campaign sends through the SMS channel. If
    -- specified, this message overrides the default message.
    sMSMessage :: Prelude.Maybe CampaignSmsMessage
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
-- 'aPNSMessage', 'messageConfiguration_aPNSMessage' - The message that the campaign sends through the APNs (Apple Push
-- Notification service) channel. If specified, this message overrides the
-- default message.
--
-- 'baiduMessage', 'messageConfiguration_baiduMessage' - The message that the campaign sends through the Baidu (Baidu Cloud Push)
-- channel. If specified, this message overrides the default message.
--
-- 'customMessage', 'messageConfiguration_customMessage' - The message that the campaign sends through a custom channel, as
-- specified by the delivery configuration (CustomDeliveryConfiguration)
-- settings for the campaign. If specified, this message overrides the
-- default message.
--
-- 'defaultMessage', 'messageConfiguration_defaultMessage' - The default message that the campaign sends through all the channels
-- that are configured for the campaign.
--
-- 'emailMessage', 'messageConfiguration_emailMessage' - The message that the campaign sends through the email channel. If
-- specified, this message overrides the default message.
--
-- 'gCMMessage', 'messageConfiguration_gCMMessage' - The message that the campaign sends through the GCM channel, which
-- enables Amazon Pinpoint to send push notifications through the Firebase
-- Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
-- If specified, this message overrides the default message.
--
-- 'inAppMessage', 'messageConfiguration_inAppMessage' - The in-app message configuration.
--
-- 'sMSMessage', 'messageConfiguration_sMSMessage' - The message that the campaign sends through the SMS channel. If
-- specified, this message overrides the default message.
newMessageConfiguration ::
  MessageConfiguration
newMessageConfiguration =
  MessageConfiguration'
    { aDMMessage = Prelude.Nothing,
      aPNSMessage = Prelude.Nothing,
      baiduMessage = Prelude.Nothing,
      customMessage = Prelude.Nothing,
      defaultMessage = Prelude.Nothing,
      emailMessage = Prelude.Nothing,
      gCMMessage = Prelude.Nothing,
      inAppMessage = Prelude.Nothing,
      sMSMessage = Prelude.Nothing
    }

-- | The message that the campaign sends through the ADM (Amazon Device
-- Messaging) channel. If specified, this message overrides the default
-- message.
messageConfiguration_aDMMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_aDMMessage = Lens.lens (\MessageConfiguration' {aDMMessage} -> aDMMessage) (\s@MessageConfiguration' {} a -> s {aDMMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the APNs (Apple Push
-- Notification service) channel. If specified, this message overrides the
-- default message.
messageConfiguration_aPNSMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_aPNSMessage = Lens.lens (\MessageConfiguration' {aPNSMessage} -> aPNSMessage) (\s@MessageConfiguration' {} a -> s {aPNSMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the Baidu (Baidu Cloud Push)
-- channel. If specified, this message overrides the default message.
messageConfiguration_baiduMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_baiduMessage = Lens.lens (\MessageConfiguration' {baiduMessage} -> baiduMessage) (\s@MessageConfiguration' {} a -> s {baiduMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through a custom channel, as
-- specified by the delivery configuration (CustomDeliveryConfiguration)
-- settings for the campaign. If specified, this message overrides the
-- default message.
messageConfiguration_customMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe CampaignCustomMessage)
messageConfiguration_customMessage = Lens.lens (\MessageConfiguration' {customMessage} -> customMessage) (\s@MessageConfiguration' {} a -> s {customMessage = a} :: MessageConfiguration)

-- | The default message that the campaign sends through all the channels
-- that are configured for the campaign.
messageConfiguration_defaultMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_defaultMessage = Lens.lens (\MessageConfiguration' {defaultMessage} -> defaultMessage) (\s@MessageConfiguration' {} a -> s {defaultMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the email channel. If
-- specified, this message overrides the default message.
messageConfiguration_emailMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe CampaignEmailMessage)
messageConfiguration_emailMessage = Lens.lens (\MessageConfiguration' {emailMessage} -> emailMessage) (\s@MessageConfiguration' {} a -> s {emailMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the GCM channel, which
-- enables Amazon Pinpoint to send push notifications through the Firebase
-- Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
-- If specified, this message overrides the default message.
messageConfiguration_gCMMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe Message)
messageConfiguration_gCMMessage = Lens.lens (\MessageConfiguration' {gCMMessage} -> gCMMessage) (\s@MessageConfiguration' {} a -> s {gCMMessage = a} :: MessageConfiguration)

-- | The in-app message configuration.
messageConfiguration_inAppMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe CampaignInAppMessage)
messageConfiguration_inAppMessage = Lens.lens (\MessageConfiguration' {inAppMessage} -> inAppMessage) (\s@MessageConfiguration' {} a -> s {inAppMessage = a} :: MessageConfiguration)

-- | The message that the campaign sends through the SMS channel. If
-- specified, this message overrides the default message.
messageConfiguration_sMSMessage :: Lens.Lens' MessageConfiguration (Prelude.Maybe CampaignSmsMessage)
messageConfiguration_sMSMessage = Lens.lens (\MessageConfiguration' {sMSMessage} -> sMSMessage) (\s@MessageConfiguration' {} a -> s {sMSMessage = a} :: MessageConfiguration)

instance Data.FromJSON MessageConfiguration where
  parseJSON =
    Data.withObject
      "MessageConfiguration"
      ( \x ->
          MessageConfiguration'
            Prelude.<$> (x Data..:? "ADMMessage")
            Prelude.<*> (x Data..:? "APNSMessage")
            Prelude.<*> (x Data..:? "BaiduMessage")
            Prelude.<*> (x Data..:? "CustomMessage")
            Prelude.<*> (x Data..:? "DefaultMessage")
            Prelude.<*> (x Data..:? "EmailMessage")
            Prelude.<*> (x Data..:? "GCMMessage")
            Prelude.<*> (x Data..:? "InAppMessage")
            Prelude.<*> (x Data..:? "SMSMessage")
      )

instance Prelude.Hashable MessageConfiguration where
  hashWithSalt _salt MessageConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` aDMMessage
      `Prelude.hashWithSalt` aPNSMessage
      `Prelude.hashWithSalt` baiduMessage
      `Prelude.hashWithSalt` customMessage
      `Prelude.hashWithSalt` defaultMessage
      `Prelude.hashWithSalt` emailMessage
      `Prelude.hashWithSalt` gCMMessage
      `Prelude.hashWithSalt` inAppMessage
      `Prelude.hashWithSalt` sMSMessage

instance Prelude.NFData MessageConfiguration where
  rnf MessageConfiguration' {..} =
    Prelude.rnf aDMMessage `Prelude.seq`
      Prelude.rnf aPNSMessage `Prelude.seq`
        Prelude.rnf baiduMessage `Prelude.seq`
          Prelude.rnf customMessage `Prelude.seq`
            Prelude.rnf defaultMessage `Prelude.seq`
              Prelude.rnf emailMessage `Prelude.seq`
                Prelude.rnf gCMMessage `Prelude.seq`
                  Prelude.rnf inAppMessage `Prelude.seq`
                    Prelude.rnf sMSMessage

instance Data.ToJSON MessageConfiguration where
  toJSON MessageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ADMMessage" Data..=) Prelude.<$> aDMMessage,
            ("APNSMessage" Data..=) Prelude.<$> aPNSMessage,
            ("BaiduMessage" Data..=) Prelude.<$> baiduMessage,
            ("CustomMessage" Data..=) Prelude.<$> customMessage,
            ("DefaultMessage" Data..=)
              Prelude.<$> defaultMessage,
            ("EmailMessage" Data..=) Prelude.<$> emailMessage,
            ("GCMMessage" Data..=) Prelude.<$> gCMMessage,
            ("InAppMessage" Data..=) Prelude.<$> inAppMessage,
            ("SMSMessage" Data..=) Prelude.<$> sMSMessage
          ]
      )
