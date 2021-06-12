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
-- Module      : Network.AWS.Pinpoint.Types.VoiceTemplateResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.VoiceTemplateResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.TemplateType

-- | Provides information about the content and settings for a message
-- template that can be used in messages that are sent through the voice
-- channel.
--
-- /See:/ 'newVoiceTemplateResponse' smart constructor.
data VoiceTemplateResponse = VoiceTemplateResponse'
  { -- | The code for the language that\'s used when synthesizing the text of the
    -- script in messages that are based on the message template. For a list of
    -- supported languages and the code for each one, see the
    -- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
    languageCode :: Core.Maybe Core.Text,
    -- | The custom description of the message template.
    templateDescription :: Core.Maybe Core.Text,
    -- | The name of the voice that\'s used when delivering messages that are
    -- based on the message template. For a list of supported voices, see the
    -- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
    voiceId :: Core.Maybe Core.Text,
    -- | The text of the script that\'s used in messages that are based on the
    -- message template, in plain text format.
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
    -- | The date, in ISO 8601 format, when the message template was last
    -- modified.
    lastModifiedDate :: Core.Text,
    -- | The date, in ISO 8601 format, when the message template was created.
    creationDate :: Core.Text,
    -- | The name of the message template.
    templateName :: Core.Text,
    -- | The type of channel that the message template is designed for. For a
    -- voice template, this value is VOICE.
    templateType :: TemplateType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VoiceTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'voiceTemplateResponse_languageCode' - The code for the language that\'s used when synthesizing the text of the
-- script in messages that are based on the message template. For a list of
-- supported languages and the code for each one, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
--
-- 'templateDescription', 'voiceTemplateResponse_templateDescription' - The custom description of the message template.
--
-- 'voiceId', 'voiceTemplateResponse_voiceId' - The name of the voice that\'s used when delivering messages that are
-- based on the message template. For a list of supported voices, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
--
-- 'body', 'voiceTemplateResponse_body' - The text of the script that\'s used in messages that are based on the
-- message template, in plain text format.
--
-- 'arn', 'voiceTemplateResponse_arn' - The Amazon Resource Name (ARN) of the message template.
--
-- 'version', 'voiceTemplateResponse_version' - The unique identifier, as an integer, for the active version of the
-- message template, or the version of the template that you specified by
-- using the version parameter in your request.
--
-- 'defaultSubstitutions', 'voiceTemplateResponse_defaultSubstitutions' - The JSON object that specifies the default values that are used for
-- message variables in the message template. This object is a set of
-- key-value pairs. Each key defines a message variable in the template.
-- The corresponding value defines the default value for that variable.
--
-- 'tags', 'voiceTemplateResponse_tags' - A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the message template. Each tag consists of a
-- required tag key and an associated tag value.
--
-- 'lastModifiedDate', 'voiceTemplateResponse_lastModifiedDate' - The date, in ISO 8601 format, when the message template was last
-- modified.
--
-- 'creationDate', 'voiceTemplateResponse_creationDate' - The date, in ISO 8601 format, when the message template was created.
--
-- 'templateName', 'voiceTemplateResponse_templateName' - The name of the message template.
--
-- 'templateType', 'voiceTemplateResponse_templateType' - The type of channel that the message template is designed for. For a
-- voice template, this value is VOICE.
newVoiceTemplateResponse ::
  -- | 'lastModifiedDate'
  Core.Text ->
  -- | 'creationDate'
  Core.Text ->
  -- | 'templateName'
  Core.Text ->
  -- | 'templateType'
  TemplateType ->
  VoiceTemplateResponse
newVoiceTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    VoiceTemplateResponse'
      { languageCode = Core.Nothing,
        templateDescription = Core.Nothing,
        voiceId = Core.Nothing,
        body = Core.Nothing,
        arn = Core.Nothing,
        version = Core.Nothing,
        defaultSubstitutions = Core.Nothing,
        tags = Core.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        templateName = pTemplateName_,
        templateType = pTemplateType_
      }

-- | The code for the language that\'s used when synthesizing the text of the
-- script in messages that are based on the message template. For a list of
-- supported languages and the code for each one, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
voiceTemplateResponse_languageCode :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
voiceTemplateResponse_languageCode = Lens.lens (\VoiceTemplateResponse' {languageCode} -> languageCode) (\s@VoiceTemplateResponse' {} a -> s {languageCode = a} :: VoiceTemplateResponse)

-- | The custom description of the message template.
voiceTemplateResponse_templateDescription :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
voiceTemplateResponse_templateDescription = Lens.lens (\VoiceTemplateResponse' {templateDescription} -> templateDescription) (\s@VoiceTemplateResponse' {} a -> s {templateDescription = a} :: VoiceTemplateResponse)

-- | The name of the voice that\'s used when delivering messages that are
-- based on the message template. For a list of supported voices, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
voiceTemplateResponse_voiceId :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
voiceTemplateResponse_voiceId = Lens.lens (\VoiceTemplateResponse' {voiceId} -> voiceId) (\s@VoiceTemplateResponse' {} a -> s {voiceId = a} :: VoiceTemplateResponse)

-- | The text of the script that\'s used in messages that are based on the
-- message template, in plain text format.
voiceTemplateResponse_body :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
voiceTemplateResponse_body = Lens.lens (\VoiceTemplateResponse' {body} -> body) (\s@VoiceTemplateResponse' {} a -> s {body = a} :: VoiceTemplateResponse)

-- | The Amazon Resource Name (ARN) of the message template.
voiceTemplateResponse_arn :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
voiceTemplateResponse_arn = Lens.lens (\VoiceTemplateResponse' {arn} -> arn) (\s@VoiceTemplateResponse' {} a -> s {arn = a} :: VoiceTemplateResponse)

-- | The unique identifier, as an integer, for the active version of the
-- message template, or the version of the template that you specified by
-- using the version parameter in your request.
voiceTemplateResponse_version :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
voiceTemplateResponse_version = Lens.lens (\VoiceTemplateResponse' {version} -> version) (\s@VoiceTemplateResponse' {} a -> s {version = a} :: VoiceTemplateResponse)

-- | The JSON object that specifies the default values that are used for
-- message variables in the message template. This object is a set of
-- key-value pairs. Each key defines a message variable in the template.
-- The corresponding value defines the default value for that variable.
voiceTemplateResponse_defaultSubstitutions :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
voiceTemplateResponse_defaultSubstitutions = Lens.lens (\VoiceTemplateResponse' {defaultSubstitutions} -> defaultSubstitutions) (\s@VoiceTemplateResponse' {} a -> s {defaultSubstitutions = a} :: VoiceTemplateResponse)

-- | A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the message template. Each tag consists of a
-- required tag key and an associated tag value.
voiceTemplateResponse_tags :: Lens.Lens' VoiceTemplateResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
voiceTemplateResponse_tags = Lens.lens (\VoiceTemplateResponse' {tags} -> tags) (\s@VoiceTemplateResponse' {} a -> s {tags = a} :: VoiceTemplateResponse) Core.. Lens.mapping Lens._Coerce

-- | The date, in ISO 8601 format, when the message template was last
-- modified.
voiceTemplateResponse_lastModifiedDate :: Lens.Lens' VoiceTemplateResponse Core.Text
voiceTemplateResponse_lastModifiedDate = Lens.lens (\VoiceTemplateResponse' {lastModifiedDate} -> lastModifiedDate) (\s@VoiceTemplateResponse' {} a -> s {lastModifiedDate = a} :: VoiceTemplateResponse)

-- | The date, in ISO 8601 format, when the message template was created.
voiceTemplateResponse_creationDate :: Lens.Lens' VoiceTemplateResponse Core.Text
voiceTemplateResponse_creationDate = Lens.lens (\VoiceTemplateResponse' {creationDate} -> creationDate) (\s@VoiceTemplateResponse' {} a -> s {creationDate = a} :: VoiceTemplateResponse)

-- | The name of the message template.
voiceTemplateResponse_templateName :: Lens.Lens' VoiceTemplateResponse Core.Text
voiceTemplateResponse_templateName = Lens.lens (\VoiceTemplateResponse' {templateName} -> templateName) (\s@VoiceTemplateResponse' {} a -> s {templateName = a} :: VoiceTemplateResponse)

-- | The type of channel that the message template is designed for. For a
-- voice template, this value is VOICE.
voiceTemplateResponse_templateType :: Lens.Lens' VoiceTemplateResponse TemplateType
voiceTemplateResponse_templateType = Lens.lens (\VoiceTemplateResponse' {templateType} -> templateType) (\s@VoiceTemplateResponse' {} a -> s {templateType = a} :: VoiceTemplateResponse)

instance Core.FromJSON VoiceTemplateResponse where
  parseJSON =
    Core.withObject
      "VoiceTemplateResponse"
      ( \x ->
          VoiceTemplateResponse'
            Core.<$> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "TemplateDescription")
            Core.<*> (x Core..:? "VoiceId")
            Core.<*> (x Core..:? "Body")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "DefaultSubstitutions")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..: "LastModifiedDate")
            Core.<*> (x Core..: "CreationDate")
            Core.<*> (x Core..: "TemplateName")
            Core.<*> (x Core..: "TemplateType")
      )

instance Core.Hashable VoiceTemplateResponse

instance Core.NFData VoiceTemplateResponse
