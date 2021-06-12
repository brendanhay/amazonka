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
-- Module      : Network.AWS.Pinpoint.Types.SMSTemplateResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSTemplateResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.TemplateType

-- | Provides information about the content and settings for a message
-- template that can be used in text messages that are sent through the SMS
-- channel.
--
-- /See:/ 'newSMSTemplateResponse' smart constructor.
data SMSTemplateResponse = SMSTemplateResponse'
  { -- | The custom description of the message template.
    templateDescription :: Core.Maybe Core.Text,
    -- | The message body that\'s used in text messages that are based on the
    -- message template.
    body :: Core.Maybe Core.Text,
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
    -- | A string-to-string map of key-value pairs that identifies the tags that
    -- are associated with the message template. Each tag consists of a
    -- required tag key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The unique identifier for the recommender model that\'s used by the
    -- message template.
    recommenderId :: Core.Maybe Core.Text,
    -- | The date, in ISO 8601 format, when the message template was last
    -- modified.
    lastModifiedDate :: Core.Text,
    -- | The date, in ISO 8601 format, when the message template was created.
    creationDate :: Core.Text,
    -- | The name of the message template.
    templateName :: Core.Text,
    -- | The type of channel that the message template is designed for. For an
    -- SMS template, this value is SMS.
    templateType :: TemplateType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SMSTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateDescription', 'sMSTemplateResponse_templateDescription' - The custom description of the message template.
--
-- 'body', 'sMSTemplateResponse_body' - The message body that\'s used in text messages that are based on the
-- message template.
--
-- 'arn', 'sMSTemplateResponse_arn' - The Amazon Resource Name (ARN) of the message template.
--
-- 'version', 'sMSTemplateResponse_version' - The unique identifier, as an integer, for the active version of the
-- message template, or the version of the template that you specified by
-- using the version parameter in your request.
--
-- 'defaultSubstitutions', 'sMSTemplateResponse_defaultSubstitutions' - The JSON object that specifies the default values that are used for
-- message variables in the message template. This object is a set of
-- key-value pairs. Each key defines a message variable in the template.
-- The corresponding value defines the default value for that variable.
--
-- 'tags', 'sMSTemplateResponse_tags' - A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the message template. Each tag consists of a
-- required tag key and an associated tag value.
--
-- 'recommenderId', 'sMSTemplateResponse_recommenderId' - The unique identifier for the recommender model that\'s used by the
-- message template.
--
-- 'lastModifiedDate', 'sMSTemplateResponse_lastModifiedDate' - The date, in ISO 8601 format, when the message template was last
-- modified.
--
-- 'creationDate', 'sMSTemplateResponse_creationDate' - The date, in ISO 8601 format, when the message template was created.
--
-- 'templateName', 'sMSTemplateResponse_templateName' - The name of the message template.
--
-- 'templateType', 'sMSTemplateResponse_templateType' - The type of channel that the message template is designed for. For an
-- SMS template, this value is SMS.
newSMSTemplateResponse ::
  -- | 'lastModifiedDate'
  Core.Text ->
  -- | 'creationDate'
  Core.Text ->
  -- | 'templateName'
  Core.Text ->
  -- | 'templateType'
  TemplateType ->
  SMSTemplateResponse
newSMSTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    SMSTemplateResponse'
      { templateDescription =
          Core.Nothing,
        body = Core.Nothing,
        arn = Core.Nothing,
        version = Core.Nothing,
        defaultSubstitutions = Core.Nothing,
        tags = Core.Nothing,
        recommenderId = Core.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        templateName = pTemplateName_,
        templateType = pTemplateType_
      }

-- | The custom description of the message template.
sMSTemplateResponse_templateDescription :: Lens.Lens' SMSTemplateResponse (Core.Maybe Core.Text)
sMSTemplateResponse_templateDescription = Lens.lens (\SMSTemplateResponse' {templateDescription} -> templateDescription) (\s@SMSTemplateResponse' {} a -> s {templateDescription = a} :: SMSTemplateResponse)

-- | The message body that\'s used in text messages that are based on the
-- message template.
sMSTemplateResponse_body :: Lens.Lens' SMSTemplateResponse (Core.Maybe Core.Text)
sMSTemplateResponse_body = Lens.lens (\SMSTemplateResponse' {body} -> body) (\s@SMSTemplateResponse' {} a -> s {body = a} :: SMSTemplateResponse)

-- | The Amazon Resource Name (ARN) of the message template.
sMSTemplateResponse_arn :: Lens.Lens' SMSTemplateResponse (Core.Maybe Core.Text)
sMSTemplateResponse_arn = Lens.lens (\SMSTemplateResponse' {arn} -> arn) (\s@SMSTemplateResponse' {} a -> s {arn = a} :: SMSTemplateResponse)

-- | The unique identifier, as an integer, for the active version of the
-- message template, or the version of the template that you specified by
-- using the version parameter in your request.
sMSTemplateResponse_version :: Lens.Lens' SMSTemplateResponse (Core.Maybe Core.Text)
sMSTemplateResponse_version = Lens.lens (\SMSTemplateResponse' {version} -> version) (\s@SMSTemplateResponse' {} a -> s {version = a} :: SMSTemplateResponse)

-- | The JSON object that specifies the default values that are used for
-- message variables in the message template. This object is a set of
-- key-value pairs. Each key defines a message variable in the template.
-- The corresponding value defines the default value for that variable.
sMSTemplateResponse_defaultSubstitutions :: Lens.Lens' SMSTemplateResponse (Core.Maybe Core.Text)
sMSTemplateResponse_defaultSubstitutions = Lens.lens (\SMSTemplateResponse' {defaultSubstitutions} -> defaultSubstitutions) (\s@SMSTemplateResponse' {} a -> s {defaultSubstitutions = a} :: SMSTemplateResponse)

-- | A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the message template. Each tag consists of a
-- required tag key and an associated tag value.
sMSTemplateResponse_tags :: Lens.Lens' SMSTemplateResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
sMSTemplateResponse_tags = Lens.lens (\SMSTemplateResponse' {tags} -> tags) (\s@SMSTemplateResponse' {} a -> s {tags = a} :: SMSTemplateResponse) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier for the recommender model that\'s used by the
-- message template.
sMSTemplateResponse_recommenderId :: Lens.Lens' SMSTemplateResponse (Core.Maybe Core.Text)
sMSTemplateResponse_recommenderId = Lens.lens (\SMSTemplateResponse' {recommenderId} -> recommenderId) (\s@SMSTemplateResponse' {} a -> s {recommenderId = a} :: SMSTemplateResponse)

-- | The date, in ISO 8601 format, when the message template was last
-- modified.
sMSTemplateResponse_lastModifiedDate :: Lens.Lens' SMSTemplateResponse Core.Text
sMSTemplateResponse_lastModifiedDate = Lens.lens (\SMSTemplateResponse' {lastModifiedDate} -> lastModifiedDate) (\s@SMSTemplateResponse' {} a -> s {lastModifiedDate = a} :: SMSTemplateResponse)

-- | The date, in ISO 8601 format, when the message template was created.
sMSTemplateResponse_creationDate :: Lens.Lens' SMSTemplateResponse Core.Text
sMSTemplateResponse_creationDate = Lens.lens (\SMSTemplateResponse' {creationDate} -> creationDate) (\s@SMSTemplateResponse' {} a -> s {creationDate = a} :: SMSTemplateResponse)

-- | The name of the message template.
sMSTemplateResponse_templateName :: Lens.Lens' SMSTemplateResponse Core.Text
sMSTemplateResponse_templateName = Lens.lens (\SMSTemplateResponse' {templateName} -> templateName) (\s@SMSTemplateResponse' {} a -> s {templateName = a} :: SMSTemplateResponse)

-- | The type of channel that the message template is designed for. For an
-- SMS template, this value is SMS.
sMSTemplateResponse_templateType :: Lens.Lens' SMSTemplateResponse TemplateType
sMSTemplateResponse_templateType = Lens.lens (\SMSTemplateResponse' {templateType} -> templateType) (\s@SMSTemplateResponse' {} a -> s {templateType = a} :: SMSTemplateResponse)

instance Core.FromJSON SMSTemplateResponse where
  parseJSON =
    Core.withObject
      "SMSTemplateResponse"
      ( \x ->
          SMSTemplateResponse'
            Core.<$> (x Core..:? "TemplateDescription")
            Core.<*> (x Core..:? "Body")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "DefaultSubstitutions")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "RecommenderId")
            Core.<*> (x Core..: "LastModifiedDate")
            Core.<*> (x Core..: "CreationDate")
            Core.<*> (x Core..: "TemplateName")
            Core.<*> (x Core..: "TemplateType")
      )

instance Core.Hashable SMSTemplateResponse

instance Core.NFData SMSTemplateResponse
