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
-- Module      : Amazonka.Pinpoint.Types.PushNotificationTemplateResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.PushNotificationTemplateResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.APNSPushNotificationTemplate
import Amazonka.Pinpoint.Types.AndroidPushNotificationTemplate
import Amazonka.Pinpoint.Types.DefaultPushNotificationTemplate
import Amazonka.Pinpoint.Types.TemplateType
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the content and settings for a message
-- template that can be used in messages that are sent through a push
-- notification channel.
--
-- /See:/ 'newPushNotificationTemplateResponse' smart constructor.
data PushNotificationTemplateResponse = PushNotificationTemplateResponse'
  { -- | A string-to-string map of key-value pairs that identifies the tags that
    -- are associated with the message template. Each tag consists of a
    -- required tag key and an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The message template that\'s used for the ADM (Amazon Device Messaging)
    -- channel. This message template overrides the default template for push
    -- notification channels (DefaultPushNotificationTemplate).
    adm :: Prelude.Maybe AndroidPushNotificationTemplate,
    -- | The message template that\'s used for the APNs (Apple Push Notification
    -- service) channel. This message template overrides the default template
    -- for push notification channels (DefaultPushNotificationTemplate).
    apns :: Prelude.Maybe APNSPushNotificationTemplate,
    -- | The default message template that\'s used for push notification
    -- channels.
    default' :: Prelude.Maybe DefaultPushNotificationTemplate,
    -- | The Amazon Resource Name (ARN) of the message template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the recommender model that\'s used by the
    -- message template.
    recommenderId :: Prelude.Maybe Prelude.Text,
    -- | The JSON object that specifies the default values that are used for
    -- message variables in the message template. This object is a set of
    -- key-value pairs. Each key defines a message variable in the template.
    -- The corresponding value defines the default value for that variable.
    defaultSubstitutions :: Prelude.Maybe Prelude.Text,
    -- | The message template that\'s used for the Baidu (Baidu Cloud Push)
    -- channel. This message template overrides the default template for push
    -- notification channels (DefaultPushNotificationTemplate).
    baidu :: Prelude.Maybe AndroidPushNotificationTemplate,
    -- | The custom description of the message template.
    templateDescription :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier, as an integer, for the active version of the
    -- message template, or the version of the template that you specified by
    -- using the version parameter in your request.
    version :: Prelude.Maybe Prelude.Text,
    -- | The message template that\'s used for the GCM channel, which is used to
    -- send notifications through the Firebase Cloud Messaging (FCM), formerly
    -- Google Cloud Messaging (GCM), service. This message template overrides
    -- the default template for push notification channels
    -- (DefaultPushNotificationTemplate).
    gcm :: Prelude.Maybe AndroidPushNotificationTemplate,
    -- | The date, in ISO 8601 format, when the message template was last
    -- modified.
    lastModifiedDate :: Prelude.Text,
    -- | The date, in ISO 8601 format, when the message template was created.
    creationDate :: Prelude.Text,
    -- | The type of channel that the message template is designed for. For a
    -- push notification template, this value is PUSH.
    templateType :: TemplateType,
    -- | The name of the message template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PushNotificationTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'pushNotificationTemplateResponse_tags' - A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the message template. Each tag consists of a
-- required tag key and an associated tag value.
--
-- 'adm', 'pushNotificationTemplateResponse_adm' - The message template that\'s used for the ADM (Amazon Device Messaging)
-- channel. This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
--
-- 'apns', 'pushNotificationTemplateResponse_apns' - The message template that\'s used for the APNs (Apple Push Notification
-- service) channel. This message template overrides the default template
-- for push notification channels (DefaultPushNotificationTemplate).
--
-- 'default'', 'pushNotificationTemplateResponse_default' - The default message template that\'s used for push notification
-- channels.
--
-- 'arn', 'pushNotificationTemplateResponse_arn' - The Amazon Resource Name (ARN) of the message template.
--
-- 'recommenderId', 'pushNotificationTemplateResponse_recommenderId' - The unique identifier for the recommender model that\'s used by the
-- message template.
--
-- 'defaultSubstitutions', 'pushNotificationTemplateResponse_defaultSubstitutions' - The JSON object that specifies the default values that are used for
-- message variables in the message template. This object is a set of
-- key-value pairs. Each key defines a message variable in the template.
-- The corresponding value defines the default value for that variable.
--
-- 'baidu', 'pushNotificationTemplateResponse_baidu' - The message template that\'s used for the Baidu (Baidu Cloud Push)
-- channel. This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
--
-- 'templateDescription', 'pushNotificationTemplateResponse_templateDescription' - The custom description of the message template.
--
-- 'version', 'pushNotificationTemplateResponse_version' - The unique identifier, as an integer, for the active version of the
-- message template, or the version of the template that you specified by
-- using the version parameter in your request.
--
-- 'gcm', 'pushNotificationTemplateResponse_gcm' - The message template that\'s used for the GCM channel, which is used to
-- send notifications through the Firebase Cloud Messaging (FCM), formerly
-- Google Cloud Messaging (GCM), service. This message template overrides
-- the default template for push notification channels
-- (DefaultPushNotificationTemplate).
--
-- 'lastModifiedDate', 'pushNotificationTemplateResponse_lastModifiedDate' - The date, in ISO 8601 format, when the message template was last
-- modified.
--
-- 'creationDate', 'pushNotificationTemplateResponse_creationDate' - The date, in ISO 8601 format, when the message template was created.
--
-- 'templateType', 'pushNotificationTemplateResponse_templateType' - The type of channel that the message template is designed for. For a
-- push notification template, this value is PUSH.
--
-- 'templateName', 'pushNotificationTemplateResponse_templateName' - The name of the message template.
newPushNotificationTemplateResponse ::
  -- | 'lastModifiedDate'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.Text ->
  -- | 'templateType'
  TemplateType ->
  -- | 'templateName'
  Prelude.Text ->
  PushNotificationTemplateResponse
newPushNotificationTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateType_
  pTemplateName_ =
    PushNotificationTemplateResponse'
      { tags =
          Prelude.Nothing,
        adm = Prelude.Nothing,
        apns = Prelude.Nothing,
        default' = Prelude.Nothing,
        arn = Prelude.Nothing,
        recommenderId = Prelude.Nothing,
        defaultSubstitutions = Prelude.Nothing,
        baidu = Prelude.Nothing,
        templateDescription = Prelude.Nothing,
        version = Prelude.Nothing,
        gcm = Prelude.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        templateType = pTemplateType_,
        templateName = pTemplateName_
      }

-- | A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the message template. Each tag consists of a
-- required tag key and an associated tag value.
pushNotificationTemplateResponse_tags :: Lens.Lens' PushNotificationTemplateResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
pushNotificationTemplateResponse_tags = Lens.lens (\PushNotificationTemplateResponse' {tags} -> tags) (\s@PushNotificationTemplateResponse' {} a -> s {tags = a} :: PushNotificationTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The message template that\'s used for the ADM (Amazon Device Messaging)
-- channel. This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
pushNotificationTemplateResponse_adm :: Lens.Lens' PushNotificationTemplateResponse (Prelude.Maybe AndroidPushNotificationTemplate)
pushNotificationTemplateResponse_adm = Lens.lens (\PushNotificationTemplateResponse' {adm} -> adm) (\s@PushNotificationTemplateResponse' {} a -> s {adm = a} :: PushNotificationTemplateResponse)

-- | The message template that\'s used for the APNs (Apple Push Notification
-- service) channel. This message template overrides the default template
-- for push notification channels (DefaultPushNotificationTemplate).
pushNotificationTemplateResponse_apns :: Lens.Lens' PushNotificationTemplateResponse (Prelude.Maybe APNSPushNotificationTemplate)
pushNotificationTemplateResponse_apns = Lens.lens (\PushNotificationTemplateResponse' {apns} -> apns) (\s@PushNotificationTemplateResponse' {} a -> s {apns = a} :: PushNotificationTemplateResponse)

-- | The default message template that\'s used for push notification
-- channels.
pushNotificationTemplateResponse_default :: Lens.Lens' PushNotificationTemplateResponse (Prelude.Maybe DefaultPushNotificationTemplate)
pushNotificationTemplateResponse_default = Lens.lens (\PushNotificationTemplateResponse' {default'} -> default') (\s@PushNotificationTemplateResponse' {} a -> s {default' = a} :: PushNotificationTemplateResponse)

-- | The Amazon Resource Name (ARN) of the message template.
pushNotificationTemplateResponse_arn :: Lens.Lens' PushNotificationTemplateResponse (Prelude.Maybe Prelude.Text)
pushNotificationTemplateResponse_arn = Lens.lens (\PushNotificationTemplateResponse' {arn} -> arn) (\s@PushNotificationTemplateResponse' {} a -> s {arn = a} :: PushNotificationTemplateResponse)

-- | The unique identifier for the recommender model that\'s used by the
-- message template.
pushNotificationTemplateResponse_recommenderId :: Lens.Lens' PushNotificationTemplateResponse (Prelude.Maybe Prelude.Text)
pushNotificationTemplateResponse_recommenderId = Lens.lens (\PushNotificationTemplateResponse' {recommenderId} -> recommenderId) (\s@PushNotificationTemplateResponse' {} a -> s {recommenderId = a} :: PushNotificationTemplateResponse)

-- | The JSON object that specifies the default values that are used for
-- message variables in the message template. This object is a set of
-- key-value pairs. Each key defines a message variable in the template.
-- The corresponding value defines the default value for that variable.
pushNotificationTemplateResponse_defaultSubstitutions :: Lens.Lens' PushNotificationTemplateResponse (Prelude.Maybe Prelude.Text)
pushNotificationTemplateResponse_defaultSubstitutions = Lens.lens (\PushNotificationTemplateResponse' {defaultSubstitutions} -> defaultSubstitutions) (\s@PushNotificationTemplateResponse' {} a -> s {defaultSubstitutions = a} :: PushNotificationTemplateResponse)

-- | The message template that\'s used for the Baidu (Baidu Cloud Push)
-- channel. This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
pushNotificationTemplateResponse_baidu :: Lens.Lens' PushNotificationTemplateResponse (Prelude.Maybe AndroidPushNotificationTemplate)
pushNotificationTemplateResponse_baidu = Lens.lens (\PushNotificationTemplateResponse' {baidu} -> baidu) (\s@PushNotificationTemplateResponse' {} a -> s {baidu = a} :: PushNotificationTemplateResponse)

-- | The custom description of the message template.
pushNotificationTemplateResponse_templateDescription :: Lens.Lens' PushNotificationTemplateResponse (Prelude.Maybe Prelude.Text)
pushNotificationTemplateResponse_templateDescription = Lens.lens (\PushNotificationTemplateResponse' {templateDescription} -> templateDescription) (\s@PushNotificationTemplateResponse' {} a -> s {templateDescription = a} :: PushNotificationTemplateResponse)

-- | The unique identifier, as an integer, for the active version of the
-- message template, or the version of the template that you specified by
-- using the version parameter in your request.
pushNotificationTemplateResponse_version :: Lens.Lens' PushNotificationTemplateResponse (Prelude.Maybe Prelude.Text)
pushNotificationTemplateResponse_version = Lens.lens (\PushNotificationTemplateResponse' {version} -> version) (\s@PushNotificationTemplateResponse' {} a -> s {version = a} :: PushNotificationTemplateResponse)

-- | The message template that\'s used for the GCM channel, which is used to
-- send notifications through the Firebase Cloud Messaging (FCM), formerly
-- Google Cloud Messaging (GCM), service. This message template overrides
-- the default template for push notification channels
-- (DefaultPushNotificationTemplate).
pushNotificationTemplateResponse_gcm :: Lens.Lens' PushNotificationTemplateResponse (Prelude.Maybe AndroidPushNotificationTemplate)
pushNotificationTemplateResponse_gcm = Lens.lens (\PushNotificationTemplateResponse' {gcm} -> gcm) (\s@PushNotificationTemplateResponse' {} a -> s {gcm = a} :: PushNotificationTemplateResponse)

-- | The date, in ISO 8601 format, when the message template was last
-- modified.
pushNotificationTemplateResponse_lastModifiedDate :: Lens.Lens' PushNotificationTemplateResponse Prelude.Text
pushNotificationTemplateResponse_lastModifiedDate = Lens.lens (\PushNotificationTemplateResponse' {lastModifiedDate} -> lastModifiedDate) (\s@PushNotificationTemplateResponse' {} a -> s {lastModifiedDate = a} :: PushNotificationTemplateResponse)

-- | The date, in ISO 8601 format, when the message template was created.
pushNotificationTemplateResponse_creationDate :: Lens.Lens' PushNotificationTemplateResponse Prelude.Text
pushNotificationTemplateResponse_creationDate = Lens.lens (\PushNotificationTemplateResponse' {creationDate} -> creationDate) (\s@PushNotificationTemplateResponse' {} a -> s {creationDate = a} :: PushNotificationTemplateResponse)

-- | The type of channel that the message template is designed for. For a
-- push notification template, this value is PUSH.
pushNotificationTemplateResponse_templateType :: Lens.Lens' PushNotificationTemplateResponse TemplateType
pushNotificationTemplateResponse_templateType = Lens.lens (\PushNotificationTemplateResponse' {templateType} -> templateType) (\s@PushNotificationTemplateResponse' {} a -> s {templateType = a} :: PushNotificationTemplateResponse)

-- | The name of the message template.
pushNotificationTemplateResponse_templateName :: Lens.Lens' PushNotificationTemplateResponse Prelude.Text
pushNotificationTemplateResponse_templateName = Lens.lens (\PushNotificationTemplateResponse' {templateName} -> templateName) (\s@PushNotificationTemplateResponse' {} a -> s {templateName = a} :: PushNotificationTemplateResponse)

instance
  Core.FromJSON
    PushNotificationTemplateResponse
  where
  parseJSON =
    Core.withObject
      "PushNotificationTemplateResponse"
      ( \x ->
          PushNotificationTemplateResponse'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ADM")
            Prelude.<*> (x Core..:? "APNS")
            Prelude.<*> (x Core..:? "Default")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "RecommenderId")
            Prelude.<*> (x Core..:? "DefaultSubstitutions")
            Prelude.<*> (x Core..:? "Baidu")
            Prelude.<*> (x Core..:? "TemplateDescription")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "GCM")
            Prelude.<*> (x Core..: "LastModifiedDate")
            Prelude.<*> (x Core..: "CreationDate")
            Prelude.<*> (x Core..: "TemplateType")
            Prelude.<*> (x Core..: "TemplateName")
      )

instance
  Prelude.Hashable
    PushNotificationTemplateResponse
  where
  hashWithSalt
    _salt
    PushNotificationTemplateResponse' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` adm
        `Prelude.hashWithSalt` apns
        `Prelude.hashWithSalt` default'
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` recommenderId
        `Prelude.hashWithSalt` defaultSubstitutions
        `Prelude.hashWithSalt` baidu
        `Prelude.hashWithSalt` templateDescription
        `Prelude.hashWithSalt` version
        `Prelude.hashWithSalt` gcm
        `Prelude.hashWithSalt` lastModifiedDate
        `Prelude.hashWithSalt` creationDate
        `Prelude.hashWithSalt` templateType
        `Prelude.hashWithSalt` templateName

instance
  Prelude.NFData
    PushNotificationTemplateResponse
  where
  rnf PushNotificationTemplateResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf adm
      `Prelude.seq` Prelude.rnf apns
      `Prelude.seq` Prelude.rnf default'
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf recommenderId
      `Prelude.seq` Prelude.rnf defaultSubstitutions
      `Prelude.seq` Prelude.rnf baidu
      `Prelude.seq` Prelude.rnf templateDescription
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf gcm
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf templateType
      `Prelude.seq` Prelude.rnf templateName
