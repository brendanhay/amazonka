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
-- Module      : Network.AWS.Pinpoint.Types.PushNotificationTemplateResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.PushNotificationTemplateResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate
import Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate
import Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate
import Network.AWS.Pinpoint.Types.TemplateType

-- | Provides information about the content and settings for a message
-- template that can be used in messages that are sent through a push
-- notification channel.
--
-- /See:/ 'newPushNotificationTemplateResponse' smart constructor.
data PushNotificationTemplateResponse = PushNotificationTemplateResponse'
  { -- | The custom description of the message template.
    templateDescription :: Core.Maybe Core.Text,
    -- | The message template that\'s used for the Baidu (Baidu Cloud Push)
    -- channel. This message template overrides the default template for push
    -- notification channels (DefaultPushNotificationTemplate).
    baidu :: Core.Maybe AndroidPushNotificationTemplate,
    -- | The message template that\'s used for the ADM (Amazon Device Messaging)
    -- channel. This message template overrides the default template for push
    -- notification channels (DefaultPushNotificationTemplate).
    adm :: Core.Maybe AndroidPushNotificationTemplate,
    -- | The Amazon Resource Name (ARN) of the message template.
    arn :: Core.Maybe Core.Text,
    -- | The unique identifier, as an integer, for the active version of the
    -- message template, or the version of the template that you specified by
    -- using the version parameter in your request.
    version :: Core.Maybe Core.Text,
    -- | The JSON object that specifies the default values that are used for
    -- message variables in the message template. This object is a set of
    -- key-value pairs. Each key defines a message variable in the template.
    -- The corresponding value defines the default value for that variable.
    defaultSubstitutions :: Core.Maybe Core.Text,
    -- | The message template that\'s used for the APNs (Apple Push Notification
    -- service) channel. This message template overrides the default template
    -- for push notification channels (DefaultPushNotificationTemplate).
    apns :: Core.Maybe APNSPushNotificationTemplate,
    -- | The message template that\'s used for the GCM channel, which is used to
    -- send notifications through the Firebase Cloud Messaging (FCM), formerly
    -- Google Cloud Messaging (GCM), service. This message template overrides
    -- the default template for push notification channels
    -- (DefaultPushNotificationTemplate).
    gcm :: Core.Maybe AndroidPushNotificationTemplate,
    -- | A string-to-string map of key-value pairs that identifies the tags that
    -- are associated with the message template. Each tag consists of a
    -- required tag key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The unique identifier for the recommender model that\'s used by the
    -- message template.
    recommenderId :: Core.Maybe Core.Text,
    -- | The default message template that\'s used for push notification
    -- channels.
    default' :: Core.Maybe DefaultPushNotificationTemplate,
    -- | The date, in ISO 8601 format, when the message template was last
    -- modified.
    lastModifiedDate :: Core.Text,
    -- | The date, in ISO 8601 format, when the message template was created.
    creationDate :: Core.Text,
    -- | The type of channel that the message template is designed for. For a
    -- push notification template, this value is PUSH.
    templateType :: TemplateType,
    -- | The name of the message template.
    templateName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PushNotificationTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateDescription', 'pushNotificationTemplateResponse_templateDescription' - The custom description of the message template.
--
-- 'baidu', 'pushNotificationTemplateResponse_baidu' - The message template that\'s used for the Baidu (Baidu Cloud Push)
-- channel. This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
--
-- 'adm', 'pushNotificationTemplateResponse_adm' - The message template that\'s used for the ADM (Amazon Device Messaging)
-- channel. This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
--
-- 'arn', 'pushNotificationTemplateResponse_arn' - The Amazon Resource Name (ARN) of the message template.
--
-- 'version', 'pushNotificationTemplateResponse_version' - The unique identifier, as an integer, for the active version of the
-- message template, or the version of the template that you specified by
-- using the version parameter in your request.
--
-- 'defaultSubstitutions', 'pushNotificationTemplateResponse_defaultSubstitutions' - The JSON object that specifies the default values that are used for
-- message variables in the message template. This object is a set of
-- key-value pairs. Each key defines a message variable in the template.
-- The corresponding value defines the default value for that variable.
--
-- 'apns', 'pushNotificationTemplateResponse_apns' - The message template that\'s used for the APNs (Apple Push Notification
-- service) channel. This message template overrides the default template
-- for push notification channels (DefaultPushNotificationTemplate).
--
-- 'gcm', 'pushNotificationTemplateResponse_gcm' - The message template that\'s used for the GCM channel, which is used to
-- send notifications through the Firebase Cloud Messaging (FCM), formerly
-- Google Cloud Messaging (GCM), service. This message template overrides
-- the default template for push notification channels
-- (DefaultPushNotificationTemplate).
--
-- 'tags', 'pushNotificationTemplateResponse_tags' - A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the message template. Each tag consists of a
-- required tag key and an associated tag value.
--
-- 'recommenderId', 'pushNotificationTemplateResponse_recommenderId' - The unique identifier for the recommender model that\'s used by the
-- message template.
--
-- 'default'', 'pushNotificationTemplateResponse_default' - The default message template that\'s used for push notification
-- channels.
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
  Core.Text ->
  -- | 'creationDate'
  Core.Text ->
  -- | 'templateType'
  TemplateType ->
  -- | 'templateName'
  Core.Text ->
  PushNotificationTemplateResponse
newPushNotificationTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateType_
  pTemplateName_ =
    PushNotificationTemplateResponse'
      { templateDescription =
          Core.Nothing,
        baidu = Core.Nothing,
        adm = Core.Nothing,
        arn = Core.Nothing,
        version = Core.Nothing,
        defaultSubstitutions = Core.Nothing,
        apns = Core.Nothing,
        gcm = Core.Nothing,
        tags = Core.Nothing,
        recommenderId = Core.Nothing,
        default' = Core.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        templateType = pTemplateType_,
        templateName = pTemplateName_
      }

-- | The custom description of the message template.
pushNotificationTemplateResponse_templateDescription :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Core.Text)
pushNotificationTemplateResponse_templateDescription = Lens.lens (\PushNotificationTemplateResponse' {templateDescription} -> templateDescription) (\s@PushNotificationTemplateResponse' {} a -> s {templateDescription = a} :: PushNotificationTemplateResponse)

-- | The message template that\'s used for the Baidu (Baidu Cloud Push)
-- channel. This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
pushNotificationTemplateResponse_baidu :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe AndroidPushNotificationTemplate)
pushNotificationTemplateResponse_baidu = Lens.lens (\PushNotificationTemplateResponse' {baidu} -> baidu) (\s@PushNotificationTemplateResponse' {} a -> s {baidu = a} :: PushNotificationTemplateResponse)

-- | The message template that\'s used for the ADM (Amazon Device Messaging)
-- channel. This message template overrides the default template for push
-- notification channels (DefaultPushNotificationTemplate).
pushNotificationTemplateResponse_adm :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe AndroidPushNotificationTemplate)
pushNotificationTemplateResponse_adm = Lens.lens (\PushNotificationTemplateResponse' {adm} -> adm) (\s@PushNotificationTemplateResponse' {} a -> s {adm = a} :: PushNotificationTemplateResponse)

-- | The Amazon Resource Name (ARN) of the message template.
pushNotificationTemplateResponse_arn :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Core.Text)
pushNotificationTemplateResponse_arn = Lens.lens (\PushNotificationTemplateResponse' {arn} -> arn) (\s@PushNotificationTemplateResponse' {} a -> s {arn = a} :: PushNotificationTemplateResponse)

-- | The unique identifier, as an integer, for the active version of the
-- message template, or the version of the template that you specified by
-- using the version parameter in your request.
pushNotificationTemplateResponse_version :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Core.Text)
pushNotificationTemplateResponse_version = Lens.lens (\PushNotificationTemplateResponse' {version} -> version) (\s@PushNotificationTemplateResponse' {} a -> s {version = a} :: PushNotificationTemplateResponse)

-- | The JSON object that specifies the default values that are used for
-- message variables in the message template. This object is a set of
-- key-value pairs. Each key defines a message variable in the template.
-- The corresponding value defines the default value for that variable.
pushNotificationTemplateResponse_defaultSubstitutions :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Core.Text)
pushNotificationTemplateResponse_defaultSubstitutions = Lens.lens (\PushNotificationTemplateResponse' {defaultSubstitutions} -> defaultSubstitutions) (\s@PushNotificationTemplateResponse' {} a -> s {defaultSubstitutions = a} :: PushNotificationTemplateResponse)

-- | The message template that\'s used for the APNs (Apple Push Notification
-- service) channel. This message template overrides the default template
-- for push notification channels (DefaultPushNotificationTemplate).
pushNotificationTemplateResponse_apns :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe APNSPushNotificationTemplate)
pushNotificationTemplateResponse_apns = Lens.lens (\PushNotificationTemplateResponse' {apns} -> apns) (\s@PushNotificationTemplateResponse' {} a -> s {apns = a} :: PushNotificationTemplateResponse)

-- | The message template that\'s used for the GCM channel, which is used to
-- send notifications through the Firebase Cloud Messaging (FCM), formerly
-- Google Cloud Messaging (GCM), service. This message template overrides
-- the default template for push notification channels
-- (DefaultPushNotificationTemplate).
pushNotificationTemplateResponse_gcm :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe AndroidPushNotificationTemplate)
pushNotificationTemplateResponse_gcm = Lens.lens (\PushNotificationTemplateResponse' {gcm} -> gcm) (\s@PushNotificationTemplateResponse' {} a -> s {gcm = a} :: PushNotificationTemplateResponse)

-- | A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the message template. Each tag consists of a
-- required tag key and an associated tag value.
pushNotificationTemplateResponse_tags :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
pushNotificationTemplateResponse_tags = Lens.lens (\PushNotificationTemplateResponse' {tags} -> tags) (\s@PushNotificationTemplateResponse' {} a -> s {tags = a} :: PushNotificationTemplateResponse) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier for the recommender model that\'s used by the
-- message template.
pushNotificationTemplateResponse_recommenderId :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe Core.Text)
pushNotificationTemplateResponse_recommenderId = Lens.lens (\PushNotificationTemplateResponse' {recommenderId} -> recommenderId) (\s@PushNotificationTemplateResponse' {} a -> s {recommenderId = a} :: PushNotificationTemplateResponse)

-- | The default message template that\'s used for push notification
-- channels.
pushNotificationTemplateResponse_default :: Lens.Lens' PushNotificationTemplateResponse (Core.Maybe DefaultPushNotificationTemplate)
pushNotificationTemplateResponse_default = Lens.lens (\PushNotificationTemplateResponse' {default'} -> default') (\s@PushNotificationTemplateResponse' {} a -> s {default' = a} :: PushNotificationTemplateResponse)

-- | The date, in ISO 8601 format, when the message template was last
-- modified.
pushNotificationTemplateResponse_lastModifiedDate :: Lens.Lens' PushNotificationTemplateResponse Core.Text
pushNotificationTemplateResponse_lastModifiedDate = Lens.lens (\PushNotificationTemplateResponse' {lastModifiedDate} -> lastModifiedDate) (\s@PushNotificationTemplateResponse' {} a -> s {lastModifiedDate = a} :: PushNotificationTemplateResponse)

-- | The date, in ISO 8601 format, when the message template was created.
pushNotificationTemplateResponse_creationDate :: Lens.Lens' PushNotificationTemplateResponse Core.Text
pushNotificationTemplateResponse_creationDate = Lens.lens (\PushNotificationTemplateResponse' {creationDate} -> creationDate) (\s@PushNotificationTemplateResponse' {} a -> s {creationDate = a} :: PushNotificationTemplateResponse)

-- | The type of channel that the message template is designed for. For a
-- push notification template, this value is PUSH.
pushNotificationTemplateResponse_templateType :: Lens.Lens' PushNotificationTemplateResponse TemplateType
pushNotificationTemplateResponse_templateType = Lens.lens (\PushNotificationTemplateResponse' {templateType} -> templateType) (\s@PushNotificationTemplateResponse' {} a -> s {templateType = a} :: PushNotificationTemplateResponse)

-- | The name of the message template.
pushNotificationTemplateResponse_templateName :: Lens.Lens' PushNotificationTemplateResponse Core.Text
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
            Core.<$> (x Core..:? "TemplateDescription")
            Core.<*> (x Core..:? "Baidu")
            Core.<*> (x Core..:? "ADM")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "DefaultSubstitutions")
            Core.<*> (x Core..:? "APNS")
            Core.<*> (x Core..:? "GCM")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "RecommenderId")
            Core.<*> (x Core..:? "Default")
            Core.<*> (x Core..: "LastModifiedDate")
            Core.<*> (x Core..: "CreationDate")
            Core.<*> (x Core..: "TemplateType")
            Core.<*> (x Core..: "TemplateName")
      )

instance
  Core.Hashable
    PushNotificationTemplateResponse

instance Core.NFData PushNotificationTemplateResponse
