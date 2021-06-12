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
-- Module      : Network.AWS.Pinpoint.Types.PushNotificationTemplateRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.PushNotificationTemplateRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate
import Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate
import Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate

-- | Specifies the content and settings for a message template that can be
-- used in messages that are sent through a push notification channel.
--
-- /See:/ 'newPushNotificationTemplateRequest' smart constructor.
data PushNotificationTemplateRequest = PushNotificationTemplateRequest'
  { -- | A custom description of the message template.
    templateDescription :: Core.Maybe Core.Text,
    -- | The message template to use for the Baidu (Baidu Cloud Push) channel.
    -- This message template overrides the default template for push
    -- notification channels (DefaultPushNotificationTemplate).
    baidu :: Core.Maybe AndroidPushNotificationTemplate,
    -- | The message template to use for the ADM (Amazon Device Messaging)
    -- channel. This message template overrides the default template for push
    -- notification channels (DefaultPushNotificationTemplate).
    adm :: Core.Maybe AndroidPushNotificationTemplate,
    -- | A JSON object that specifies the default values to use for message
    -- variables in the message template. This object is a set of key-value
    -- pairs. Each key defines a message variable in the template. The
    -- corresponding value defines the default value for that variable. When
    -- you create a message that\'s based on the template, you can override
    -- these defaults with message-specific and address-specific variables and
    -- values.
    defaultSubstitutions :: Core.Maybe Core.Text,
    -- | The message template to use for the APNs (Apple Push Notification
    -- service) channel. This message template overrides the default template
    -- for push notification channels (DefaultPushNotificationTemplate).
    apns :: Core.Maybe APNSPushNotificationTemplate,
    -- | The message template to use for the GCM channel, which is used to send
    -- notifications through the Firebase Cloud Messaging (FCM), formerly
    -- Google Cloud Messaging (GCM), service. This message template overrides
    -- the default template for push notification channels
    -- (DefaultPushNotificationTemplate).
    gcm :: Core.Maybe AndroidPushNotificationTemplate,
    -- | A string-to-string map of key-value pairs that defines the tags to
    -- associate with the message template. Each tag consists of a required tag
    -- key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The unique identifier for the recommender model to use for the message
    -- template. Amazon Pinpoint uses this value to determine how to retrieve
    -- and process data from a recommender model when it sends messages that
    -- use the template, if the template contains message variables for
    -- recommendation data.
    recommenderId :: Core.Maybe Core.Text,
    -- | The default message template to use for push notification channels.
    default' :: Core.Maybe DefaultPushNotificationTemplate
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PushNotificationTemplateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateDescription', 'pushNotificationTemplateRequest_templateDescription' - A custom description of the message template.
--
-- 'baidu', 'pushNotificationTemplateRequest_baidu' - The message template to use for the Baidu (Baidu Cloud Push) channel.
-- This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
--
-- 'adm', 'pushNotificationTemplateRequest_adm' - The message template to use for the ADM (Amazon Device Messaging)
-- channel. This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
--
-- 'defaultSubstitutions', 'pushNotificationTemplateRequest_defaultSubstitutions' - A JSON object that specifies the default values to use for message
-- variables in the message template. This object is a set of key-value
-- pairs. Each key defines a message variable in the template. The
-- corresponding value defines the default value for that variable. When
-- you create a message that\'s based on the template, you can override
-- these defaults with message-specific and address-specific variables and
-- values.
--
-- 'apns', 'pushNotificationTemplateRequest_apns' - The message template to use for the APNs (Apple Push Notification
-- service) channel. This message template overrides the default template
-- for push notification channels (DefaultPushNotificationTemplate).
--
-- 'gcm', 'pushNotificationTemplateRequest_gcm' - The message template to use for the GCM channel, which is used to send
-- notifications through the Firebase Cloud Messaging (FCM), formerly
-- Google Cloud Messaging (GCM), service. This message template overrides
-- the default template for push notification channels
-- (DefaultPushNotificationTemplate).
--
-- 'tags', 'pushNotificationTemplateRequest_tags' - A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
--
-- 'recommenderId', 'pushNotificationTemplateRequest_recommenderId' - The unique identifier for the recommender model to use for the message
-- template. Amazon Pinpoint uses this value to determine how to retrieve
-- and process data from a recommender model when it sends messages that
-- use the template, if the template contains message variables for
-- recommendation data.
--
-- 'default'', 'pushNotificationTemplateRequest_default' - The default message template to use for push notification channels.
newPushNotificationTemplateRequest ::
  PushNotificationTemplateRequest
newPushNotificationTemplateRequest =
  PushNotificationTemplateRequest'
    { templateDescription =
        Core.Nothing,
      baidu = Core.Nothing,
      adm = Core.Nothing,
      defaultSubstitutions = Core.Nothing,
      apns = Core.Nothing,
      gcm = Core.Nothing,
      tags = Core.Nothing,
      recommenderId = Core.Nothing,
      default' = Core.Nothing
    }

-- | A custom description of the message template.
pushNotificationTemplateRequest_templateDescription :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe Core.Text)
pushNotificationTemplateRequest_templateDescription = Lens.lens (\PushNotificationTemplateRequest' {templateDescription} -> templateDescription) (\s@PushNotificationTemplateRequest' {} a -> s {templateDescription = a} :: PushNotificationTemplateRequest)

-- | The message template to use for the Baidu (Baidu Cloud Push) channel.
-- This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
pushNotificationTemplateRequest_baidu :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe AndroidPushNotificationTemplate)
pushNotificationTemplateRequest_baidu = Lens.lens (\PushNotificationTemplateRequest' {baidu} -> baidu) (\s@PushNotificationTemplateRequest' {} a -> s {baidu = a} :: PushNotificationTemplateRequest)

-- | The message template to use for the ADM (Amazon Device Messaging)
-- channel. This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
pushNotificationTemplateRequest_adm :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe AndroidPushNotificationTemplate)
pushNotificationTemplateRequest_adm = Lens.lens (\PushNotificationTemplateRequest' {adm} -> adm) (\s@PushNotificationTemplateRequest' {} a -> s {adm = a} :: PushNotificationTemplateRequest)

-- | A JSON object that specifies the default values to use for message
-- variables in the message template. This object is a set of key-value
-- pairs. Each key defines a message variable in the template. The
-- corresponding value defines the default value for that variable. When
-- you create a message that\'s based on the template, you can override
-- these defaults with message-specific and address-specific variables and
-- values.
pushNotificationTemplateRequest_defaultSubstitutions :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe Core.Text)
pushNotificationTemplateRequest_defaultSubstitutions = Lens.lens (\PushNotificationTemplateRequest' {defaultSubstitutions} -> defaultSubstitutions) (\s@PushNotificationTemplateRequest' {} a -> s {defaultSubstitutions = a} :: PushNotificationTemplateRequest)

-- | The message template to use for the APNs (Apple Push Notification
-- service) channel. This message template overrides the default template
-- for push notification channels (DefaultPushNotificationTemplate).
pushNotificationTemplateRequest_apns :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe APNSPushNotificationTemplate)
pushNotificationTemplateRequest_apns = Lens.lens (\PushNotificationTemplateRequest' {apns} -> apns) (\s@PushNotificationTemplateRequest' {} a -> s {apns = a} :: PushNotificationTemplateRequest)

-- | The message template to use for the GCM channel, which is used to send
-- notifications through the Firebase Cloud Messaging (FCM), formerly
-- Google Cloud Messaging (GCM), service. This message template overrides
-- the default template for push notification channels
-- (DefaultPushNotificationTemplate).
pushNotificationTemplateRequest_gcm :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe AndroidPushNotificationTemplate)
pushNotificationTemplateRequest_gcm = Lens.lens (\PushNotificationTemplateRequest' {gcm} -> gcm) (\s@PushNotificationTemplateRequest' {} a -> s {gcm = a} :: PushNotificationTemplateRequest)

-- | A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
pushNotificationTemplateRequest_tags :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe (Core.HashMap Core.Text Core.Text))
pushNotificationTemplateRequest_tags = Lens.lens (\PushNotificationTemplateRequest' {tags} -> tags) (\s@PushNotificationTemplateRequest' {} a -> s {tags = a} :: PushNotificationTemplateRequest) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier for the recommender model to use for the message
-- template. Amazon Pinpoint uses this value to determine how to retrieve
-- and process data from a recommender model when it sends messages that
-- use the template, if the template contains message variables for
-- recommendation data.
pushNotificationTemplateRequest_recommenderId :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe Core.Text)
pushNotificationTemplateRequest_recommenderId = Lens.lens (\PushNotificationTemplateRequest' {recommenderId} -> recommenderId) (\s@PushNotificationTemplateRequest' {} a -> s {recommenderId = a} :: PushNotificationTemplateRequest)

-- | The default message template to use for push notification channels.
pushNotificationTemplateRequest_default :: Lens.Lens' PushNotificationTemplateRequest (Core.Maybe DefaultPushNotificationTemplate)
pushNotificationTemplateRequest_default = Lens.lens (\PushNotificationTemplateRequest' {default'} -> default') (\s@PushNotificationTemplateRequest' {} a -> s {default' = a} :: PushNotificationTemplateRequest)

instance
  Core.Hashable
    PushNotificationTemplateRequest

instance Core.NFData PushNotificationTemplateRequest

instance Core.ToJSON PushNotificationTemplateRequest where
  toJSON PushNotificationTemplateRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TemplateDescription" Core..=)
              Core.<$> templateDescription,
            ("Baidu" Core..=) Core.<$> baidu,
            ("ADM" Core..=) Core.<$> adm,
            ("DefaultSubstitutions" Core..=)
              Core.<$> defaultSubstitutions,
            ("APNS" Core..=) Core.<$> apns,
            ("GCM" Core..=) Core.<$> gcm,
            ("tags" Core..=) Core.<$> tags,
            ("RecommenderId" Core..=) Core.<$> recommenderId,
            ("Default" Core..=) Core.<$> default'
          ]
      )
