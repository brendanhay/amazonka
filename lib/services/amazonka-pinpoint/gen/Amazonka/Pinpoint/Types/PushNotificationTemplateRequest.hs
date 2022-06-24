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
-- Module      : Amazonka.Pinpoint.Types.PushNotificationTemplateRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.PushNotificationTemplateRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.APNSPushNotificationTemplate
import Amazonka.Pinpoint.Types.AndroidPushNotificationTemplate
import Amazonka.Pinpoint.Types.DefaultPushNotificationTemplate
import qualified Amazonka.Prelude as Prelude

-- | Specifies the content and settings for a message template that can be
-- used in messages that are sent through a push notification channel.
--
-- /See:/ 'newPushNotificationTemplateRequest' smart constructor.
data PushNotificationTemplateRequest = PushNotificationTemplateRequest'
  { -- | A string-to-string map of key-value pairs that defines the tags to
    -- associate with the message template. Each tag consists of a required tag
    -- key and an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The message template to use for the ADM (Amazon Device Messaging)
    -- channel. This message template overrides the default template for push
    -- notification channels (DefaultPushNotificationTemplate).
    adm :: Prelude.Maybe AndroidPushNotificationTemplate,
    -- | The message template to use for the APNs (Apple Push Notification
    -- service) channel. This message template overrides the default template
    -- for push notification channels (DefaultPushNotificationTemplate).
    apns :: Prelude.Maybe APNSPushNotificationTemplate,
    -- | The default message template to use for push notification channels.
    default' :: Prelude.Maybe DefaultPushNotificationTemplate,
    -- | The unique identifier for the recommender model to use for the message
    -- template. Amazon Pinpoint uses this value to determine how to retrieve
    -- and process data from a recommender model when it sends messages that
    -- use the template, if the template contains message variables for
    -- recommendation data.
    recommenderId :: Prelude.Maybe Prelude.Text,
    -- | A JSON object that specifies the default values to use for message
    -- variables in the message template. This object is a set of key-value
    -- pairs. Each key defines a message variable in the template. The
    -- corresponding value defines the default value for that variable. When
    -- you create a message that\'s based on the template, you can override
    -- these defaults with message-specific and address-specific variables and
    -- values.
    defaultSubstitutions :: Prelude.Maybe Prelude.Text,
    -- | The message template to use for the Baidu (Baidu Cloud Push) channel.
    -- This message template overrides the default template for push
    -- notification channels (DefaultPushNotificationTemplate).
    baidu :: Prelude.Maybe AndroidPushNotificationTemplate,
    -- | A custom description of the message template.
    templateDescription :: Prelude.Maybe Prelude.Text,
    -- | The message template to use for the GCM channel, which is used to send
    -- notifications through the Firebase Cloud Messaging (FCM), formerly
    -- Google Cloud Messaging (GCM), service. This message template overrides
    -- the default template for push notification channels
    -- (DefaultPushNotificationTemplate).
    gcm :: Prelude.Maybe AndroidPushNotificationTemplate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PushNotificationTemplateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'pushNotificationTemplateRequest_tags' - A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
--
-- 'adm', 'pushNotificationTemplateRequest_adm' - The message template to use for the ADM (Amazon Device Messaging)
-- channel. This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
--
-- 'apns', 'pushNotificationTemplateRequest_apns' - The message template to use for the APNs (Apple Push Notification
-- service) channel. This message template overrides the default template
-- for push notification channels (DefaultPushNotificationTemplate).
--
-- 'default'', 'pushNotificationTemplateRequest_default' - The default message template to use for push notification channels.
--
-- 'recommenderId', 'pushNotificationTemplateRequest_recommenderId' - The unique identifier for the recommender model to use for the message
-- template. Amazon Pinpoint uses this value to determine how to retrieve
-- and process data from a recommender model when it sends messages that
-- use the template, if the template contains message variables for
-- recommendation data.
--
-- 'defaultSubstitutions', 'pushNotificationTemplateRequest_defaultSubstitutions' - A JSON object that specifies the default values to use for message
-- variables in the message template. This object is a set of key-value
-- pairs. Each key defines a message variable in the template. The
-- corresponding value defines the default value for that variable. When
-- you create a message that\'s based on the template, you can override
-- these defaults with message-specific and address-specific variables and
-- values.
--
-- 'baidu', 'pushNotificationTemplateRequest_baidu' - The message template to use for the Baidu (Baidu Cloud Push) channel.
-- This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
--
-- 'templateDescription', 'pushNotificationTemplateRequest_templateDescription' - A custom description of the message template.
--
-- 'gcm', 'pushNotificationTemplateRequest_gcm' - The message template to use for the GCM channel, which is used to send
-- notifications through the Firebase Cloud Messaging (FCM), formerly
-- Google Cloud Messaging (GCM), service. This message template overrides
-- the default template for push notification channels
-- (DefaultPushNotificationTemplate).
newPushNotificationTemplateRequest ::
  PushNotificationTemplateRequest
newPushNotificationTemplateRequest =
  PushNotificationTemplateRequest'
    { tags =
        Prelude.Nothing,
      adm = Prelude.Nothing,
      apns = Prelude.Nothing,
      default' = Prelude.Nothing,
      recommenderId = Prelude.Nothing,
      defaultSubstitutions = Prelude.Nothing,
      baidu = Prelude.Nothing,
      templateDescription = Prelude.Nothing,
      gcm = Prelude.Nothing
    }

-- | A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
pushNotificationTemplateRequest_tags :: Lens.Lens' PushNotificationTemplateRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
pushNotificationTemplateRequest_tags = Lens.lens (\PushNotificationTemplateRequest' {tags} -> tags) (\s@PushNotificationTemplateRequest' {} a -> s {tags = a} :: PushNotificationTemplateRequest) Prelude.. Lens.mapping Lens.coerced

-- | The message template to use for the ADM (Amazon Device Messaging)
-- channel. This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
pushNotificationTemplateRequest_adm :: Lens.Lens' PushNotificationTemplateRequest (Prelude.Maybe AndroidPushNotificationTemplate)
pushNotificationTemplateRequest_adm = Lens.lens (\PushNotificationTemplateRequest' {adm} -> adm) (\s@PushNotificationTemplateRequest' {} a -> s {adm = a} :: PushNotificationTemplateRequest)

-- | The message template to use for the APNs (Apple Push Notification
-- service) channel. This message template overrides the default template
-- for push notification channels (DefaultPushNotificationTemplate).
pushNotificationTemplateRequest_apns :: Lens.Lens' PushNotificationTemplateRequest (Prelude.Maybe APNSPushNotificationTemplate)
pushNotificationTemplateRequest_apns = Lens.lens (\PushNotificationTemplateRequest' {apns} -> apns) (\s@PushNotificationTemplateRequest' {} a -> s {apns = a} :: PushNotificationTemplateRequest)

-- | The default message template to use for push notification channels.
pushNotificationTemplateRequest_default :: Lens.Lens' PushNotificationTemplateRequest (Prelude.Maybe DefaultPushNotificationTemplate)
pushNotificationTemplateRequest_default = Lens.lens (\PushNotificationTemplateRequest' {default'} -> default') (\s@PushNotificationTemplateRequest' {} a -> s {default' = a} :: PushNotificationTemplateRequest)

-- | The unique identifier for the recommender model to use for the message
-- template. Amazon Pinpoint uses this value to determine how to retrieve
-- and process data from a recommender model when it sends messages that
-- use the template, if the template contains message variables for
-- recommendation data.
pushNotificationTemplateRequest_recommenderId :: Lens.Lens' PushNotificationTemplateRequest (Prelude.Maybe Prelude.Text)
pushNotificationTemplateRequest_recommenderId = Lens.lens (\PushNotificationTemplateRequest' {recommenderId} -> recommenderId) (\s@PushNotificationTemplateRequest' {} a -> s {recommenderId = a} :: PushNotificationTemplateRequest)

-- | A JSON object that specifies the default values to use for message
-- variables in the message template. This object is a set of key-value
-- pairs. Each key defines a message variable in the template. The
-- corresponding value defines the default value for that variable. When
-- you create a message that\'s based on the template, you can override
-- these defaults with message-specific and address-specific variables and
-- values.
pushNotificationTemplateRequest_defaultSubstitutions :: Lens.Lens' PushNotificationTemplateRequest (Prelude.Maybe Prelude.Text)
pushNotificationTemplateRequest_defaultSubstitutions = Lens.lens (\PushNotificationTemplateRequest' {defaultSubstitutions} -> defaultSubstitutions) (\s@PushNotificationTemplateRequest' {} a -> s {defaultSubstitutions = a} :: PushNotificationTemplateRequest)

-- | The message template to use for the Baidu (Baidu Cloud Push) channel.
-- This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
pushNotificationTemplateRequest_baidu :: Lens.Lens' PushNotificationTemplateRequest (Prelude.Maybe AndroidPushNotificationTemplate)
pushNotificationTemplateRequest_baidu = Lens.lens (\PushNotificationTemplateRequest' {baidu} -> baidu) (\s@PushNotificationTemplateRequest' {} a -> s {baidu = a} :: PushNotificationTemplateRequest)

-- | A custom description of the message template.
pushNotificationTemplateRequest_templateDescription :: Lens.Lens' PushNotificationTemplateRequest (Prelude.Maybe Prelude.Text)
pushNotificationTemplateRequest_templateDescription = Lens.lens (\PushNotificationTemplateRequest' {templateDescription} -> templateDescription) (\s@PushNotificationTemplateRequest' {} a -> s {templateDescription = a} :: PushNotificationTemplateRequest)

-- | The message template to use for the GCM channel, which is used to send
-- notifications through the Firebase Cloud Messaging (FCM), formerly
-- Google Cloud Messaging (GCM), service. This message template overrides
-- the default template for push notification channels
-- (DefaultPushNotificationTemplate).
pushNotificationTemplateRequest_gcm :: Lens.Lens' PushNotificationTemplateRequest (Prelude.Maybe AndroidPushNotificationTemplate)
pushNotificationTemplateRequest_gcm = Lens.lens (\PushNotificationTemplateRequest' {gcm} -> gcm) (\s@PushNotificationTemplateRequest' {} a -> s {gcm = a} :: PushNotificationTemplateRequest)

instance
  Prelude.Hashable
    PushNotificationTemplateRequest
  where
  hashWithSalt
    _salt
    PushNotificationTemplateRequest' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` adm
        `Prelude.hashWithSalt` apns
        `Prelude.hashWithSalt` default'
        `Prelude.hashWithSalt` recommenderId
        `Prelude.hashWithSalt` defaultSubstitutions
        `Prelude.hashWithSalt` baidu
        `Prelude.hashWithSalt` templateDescription
        `Prelude.hashWithSalt` gcm

instance
  Prelude.NFData
    PushNotificationTemplateRequest
  where
  rnf PushNotificationTemplateRequest' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf adm
      `Prelude.seq` Prelude.rnf apns
      `Prelude.seq` Prelude.rnf default'
      `Prelude.seq` Prelude.rnf recommenderId
      `Prelude.seq` Prelude.rnf defaultSubstitutions
      `Prelude.seq` Prelude.rnf baidu
      `Prelude.seq` Prelude.rnf templateDescription
      `Prelude.seq` Prelude.rnf gcm

instance Core.ToJSON PushNotificationTemplateRequest where
  toJSON PushNotificationTemplateRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("ADM" Core..=) Prelude.<$> adm,
            ("APNS" Core..=) Prelude.<$> apns,
            ("Default" Core..=) Prelude.<$> default',
            ("RecommenderId" Core..=) Prelude.<$> recommenderId,
            ("DefaultSubstitutions" Core..=)
              Prelude.<$> defaultSubstitutions,
            ("Baidu" Core..=) Prelude.<$> baidu,
            ("TemplateDescription" Core..=)
              Prelude.<$> templateDescription,
            ("GCM" Core..=) Prelude.<$> gcm
          ]
      )
