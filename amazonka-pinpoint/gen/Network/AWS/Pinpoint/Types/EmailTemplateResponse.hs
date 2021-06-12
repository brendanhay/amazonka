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
-- Module      : Network.AWS.Pinpoint.Types.EmailTemplateResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailTemplateResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.TemplateType

-- | Provides information about the content and settings for a message
-- template that can be used in messages that are sent through the email
-- channel.
--
-- /See:/ 'newEmailTemplateResponse' smart constructor.
data EmailTemplateResponse = EmailTemplateResponse'
  { -- | The custom description of the message template.
    templateDescription :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the message template.
    arn :: Core.Maybe Core.Text,
    -- | The unique identifier, as an integer, for the active version of the
    -- message template, or the version of the template that you specified by
    -- using the version parameter in your request.
    version :: Core.Maybe Core.Text,
    -- | The message body, in plain text format, that\'s used in email messages
    -- that are based on the message template.
    textPart :: Core.Maybe Core.Text,
    -- | The JSON object that specifies the default values that are used for
    -- message variables in the message template. This object is a set of
    -- key-value pairs. Each key defines a message variable in the template.
    -- The corresponding value defines the default value for that variable.
    defaultSubstitutions :: Core.Maybe Core.Text,
    -- | The subject line, or title, that\'s used in email messages that are
    -- based on the message template.
    subject :: Core.Maybe Core.Text,
    -- | A string-to-string map of key-value pairs that identifies the tags that
    -- are associated with the message template. Each tag consists of a
    -- required tag key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The unique identifier for the recommender model that\'s used by the
    -- message template.
    recommenderId :: Core.Maybe Core.Text,
    -- | The message body, in HTML format, that\'s used in email messages that
    -- are based on the message template.
    htmlPart :: Core.Maybe Core.Text,
    -- | The date, in ISO 8601 format, when the message template was last
    -- modified.
    lastModifiedDate :: Core.Text,
    -- | The date, in ISO 8601 format, when the message template was created.
    creationDate :: Core.Text,
    -- | The name of the message template.
    templateName :: Core.Text,
    -- | The type of channel that the message template is designed for. For an
    -- email template, this value is EMAIL.
    templateType :: TemplateType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateDescription', 'emailTemplateResponse_templateDescription' - The custom description of the message template.
--
-- 'arn', 'emailTemplateResponse_arn' - The Amazon Resource Name (ARN) of the message template.
--
-- 'version', 'emailTemplateResponse_version' - The unique identifier, as an integer, for the active version of the
-- message template, or the version of the template that you specified by
-- using the version parameter in your request.
--
-- 'textPart', 'emailTemplateResponse_textPart' - The message body, in plain text format, that\'s used in email messages
-- that are based on the message template.
--
-- 'defaultSubstitutions', 'emailTemplateResponse_defaultSubstitutions' - The JSON object that specifies the default values that are used for
-- message variables in the message template. This object is a set of
-- key-value pairs. Each key defines a message variable in the template.
-- The corresponding value defines the default value for that variable.
--
-- 'subject', 'emailTemplateResponse_subject' - The subject line, or title, that\'s used in email messages that are
-- based on the message template.
--
-- 'tags', 'emailTemplateResponse_tags' - A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the message template. Each tag consists of a
-- required tag key and an associated tag value.
--
-- 'recommenderId', 'emailTemplateResponse_recommenderId' - The unique identifier for the recommender model that\'s used by the
-- message template.
--
-- 'htmlPart', 'emailTemplateResponse_htmlPart' - The message body, in HTML format, that\'s used in email messages that
-- are based on the message template.
--
-- 'lastModifiedDate', 'emailTemplateResponse_lastModifiedDate' - The date, in ISO 8601 format, when the message template was last
-- modified.
--
-- 'creationDate', 'emailTemplateResponse_creationDate' - The date, in ISO 8601 format, when the message template was created.
--
-- 'templateName', 'emailTemplateResponse_templateName' - The name of the message template.
--
-- 'templateType', 'emailTemplateResponse_templateType' - The type of channel that the message template is designed for. For an
-- email template, this value is EMAIL.
newEmailTemplateResponse ::
  -- | 'lastModifiedDate'
  Core.Text ->
  -- | 'creationDate'
  Core.Text ->
  -- | 'templateName'
  Core.Text ->
  -- | 'templateType'
  TemplateType ->
  EmailTemplateResponse
newEmailTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    EmailTemplateResponse'
      { templateDescription =
          Core.Nothing,
        arn = Core.Nothing,
        version = Core.Nothing,
        textPart = Core.Nothing,
        defaultSubstitutions = Core.Nothing,
        subject = Core.Nothing,
        tags = Core.Nothing,
        recommenderId = Core.Nothing,
        htmlPart = Core.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        templateName = pTemplateName_,
        templateType = pTemplateType_
      }

-- | The custom description of the message template.
emailTemplateResponse_templateDescription :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
emailTemplateResponse_templateDescription = Lens.lens (\EmailTemplateResponse' {templateDescription} -> templateDescription) (\s@EmailTemplateResponse' {} a -> s {templateDescription = a} :: EmailTemplateResponse)

-- | The Amazon Resource Name (ARN) of the message template.
emailTemplateResponse_arn :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
emailTemplateResponse_arn = Lens.lens (\EmailTemplateResponse' {arn} -> arn) (\s@EmailTemplateResponse' {} a -> s {arn = a} :: EmailTemplateResponse)

-- | The unique identifier, as an integer, for the active version of the
-- message template, or the version of the template that you specified by
-- using the version parameter in your request.
emailTemplateResponse_version :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
emailTemplateResponse_version = Lens.lens (\EmailTemplateResponse' {version} -> version) (\s@EmailTemplateResponse' {} a -> s {version = a} :: EmailTemplateResponse)

-- | The message body, in plain text format, that\'s used in email messages
-- that are based on the message template.
emailTemplateResponse_textPart :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
emailTemplateResponse_textPart = Lens.lens (\EmailTemplateResponse' {textPart} -> textPart) (\s@EmailTemplateResponse' {} a -> s {textPart = a} :: EmailTemplateResponse)

-- | The JSON object that specifies the default values that are used for
-- message variables in the message template. This object is a set of
-- key-value pairs. Each key defines a message variable in the template.
-- The corresponding value defines the default value for that variable.
emailTemplateResponse_defaultSubstitutions :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
emailTemplateResponse_defaultSubstitutions = Lens.lens (\EmailTemplateResponse' {defaultSubstitutions} -> defaultSubstitutions) (\s@EmailTemplateResponse' {} a -> s {defaultSubstitutions = a} :: EmailTemplateResponse)

-- | The subject line, or title, that\'s used in email messages that are
-- based on the message template.
emailTemplateResponse_subject :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
emailTemplateResponse_subject = Lens.lens (\EmailTemplateResponse' {subject} -> subject) (\s@EmailTemplateResponse' {} a -> s {subject = a} :: EmailTemplateResponse)

-- | A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the message template. Each tag consists of a
-- required tag key and an associated tag value.
emailTemplateResponse_tags :: Lens.Lens' EmailTemplateResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
emailTemplateResponse_tags = Lens.lens (\EmailTemplateResponse' {tags} -> tags) (\s@EmailTemplateResponse' {} a -> s {tags = a} :: EmailTemplateResponse) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier for the recommender model that\'s used by the
-- message template.
emailTemplateResponse_recommenderId :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
emailTemplateResponse_recommenderId = Lens.lens (\EmailTemplateResponse' {recommenderId} -> recommenderId) (\s@EmailTemplateResponse' {} a -> s {recommenderId = a} :: EmailTemplateResponse)

-- | The message body, in HTML format, that\'s used in email messages that
-- are based on the message template.
emailTemplateResponse_htmlPart :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
emailTemplateResponse_htmlPart = Lens.lens (\EmailTemplateResponse' {htmlPart} -> htmlPart) (\s@EmailTemplateResponse' {} a -> s {htmlPart = a} :: EmailTemplateResponse)

-- | The date, in ISO 8601 format, when the message template was last
-- modified.
emailTemplateResponse_lastModifiedDate :: Lens.Lens' EmailTemplateResponse Core.Text
emailTemplateResponse_lastModifiedDate = Lens.lens (\EmailTemplateResponse' {lastModifiedDate} -> lastModifiedDate) (\s@EmailTemplateResponse' {} a -> s {lastModifiedDate = a} :: EmailTemplateResponse)

-- | The date, in ISO 8601 format, when the message template was created.
emailTemplateResponse_creationDate :: Lens.Lens' EmailTemplateResponse Core.Text
emailTemplateResponse_creationDate = Lens.lens (\EmailTemplateResponse' {creationDate} -> creationDate) (\s@EmailTemplateResponse' {} a -> s {creationDate = a} :: EmailTemplateResponse)

-- | The name of the message template.
emailTemplateResponse_templateName :: Lens.Lens' EmailTemplateResponse Core.Text
emailTemplateResponse_templateName = Lens.lens (\EmailTemplateResponse' {templateName} -> templateName) (\s@EmailTemplateResponse' {} a -> s {templateName = a} :: EmailTemplateResponse)

-- | The type of channel that the message template is designed for. For an
-- email template, this value is EMAIL.
emailTemplateResponse_templateType :: Lens.Lens' EmailTemplateResponse TemplateType
emailTemplateResponse_templateType = Lens.lens (\EmailTemplateResponse' {templateType} -> templateType) (\s@EmailTemplateResponse' {} a -> s {templateType = a} :: EmailTemplateResponse)

instance Core.FromJSON EmailTemplateResponse where
  parseJSON =
    Core.withObject
      "EmailTemplateResponse"
      ( \x ->
          EmailTemplateResponse'
            Core.<$> (x Core..:? "TemplateDescription")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "TextPart")
            Core.<*> (x Core..:? "DefaultSubstitutions")
            Core.<*> (x Core..:? "Subject")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "RecommenderId")
            Core.<*> (x Core..:? "HtmlPart")
            Core.<*> (x Core..: "LastModifiedDate")
            Core.<*> (x Core..: "CreationDate")
            Core.<*> (x Core..: "TemplateName")
            Core.<*> (x Core..: "TemplateType")
      )

instance Core.Hashable EmailTemplateResponse

instance Core.NFData EmailTemplateResponse
