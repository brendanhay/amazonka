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
-- Module      : Amazonka.Pinpoint.Types.VoiceTemplateResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.VoiceTemplateResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.TemplateType
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the content and settings for a message
-- template that can be used in messages that are sent through the voice
-- channel.
--
-- /See:/ 'newVoiceTemplateResponse' smart constructor.
data VoiceTemplateResponse = VoiceTemplateResponse'
  { -- | A string-to-string map of key-value pairs that identifies the tags that
    -- are associated with the message template. Each tag consists of a
    -- required tag key and an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the voice that\'s used when delivering messages that are
    -- based on the message template. For a list of supported voices, see the
    -- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
    voiceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the message template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The text of the script that\'s used in messages that are based on the
    -- message template, in plain text format.
    body :: Prelude.Maybe Prelude.Text,
    -- | The JSON object that specifies the default values that are used for
    -- message variables in the message template. This object is a set of
    -- key-value pairs. Each key defines a message variable in the template.
    -- The corresponding value defines the default value for that variable.
    defaultSubstitutions :: Prelude.Maybe Prelude.Text,
    -- | The code for the language that\'s used when synthesizing the text of the
    -- script in messages that are based on the message template. For a list of
    -- supported languages and the code for each one, see the
    -- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The custom description of the message template.
    templateDescription :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier, as an integer, for the active version of the
    -- message template, or the version of the template that you specified by
    -- using the version parameter in your request.
    version :: Prelude.Maybe Prelude.Text,
    -- | The date, in ISO 8601 format, when the message template was last
    -- modified.
    lastModifiedDate :: Prelude.Text,
    -- | The date, in ISO 8601 format, when the message template was created.
    creationDate :: Prelude.Text,
    -- | The name of the message template.
    templateName :: Prelude.Text,
    -- | The type of channel that the message template is designed for. For a
    -- voice template, this value is VOICE.
    templateType :: TemplateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'voiceTemplateResponse_tags' - A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the message template. Each tag consists of a
-- required tag key and an associated tag value.
--
-- 'voiceId', 'voiceTemplateResponse_voiceId' - The name of the voice that\'s used when delivering messages that are
-- based on the message template. For a list of supported voices, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
--
-- 'arn', 'voiceTemplateResponse_arn' - The Amazon Resource Name (ARN) of the message template.
--
-- 'body', 'voiceTemplateResponse_body' - The text of the script that\'s used in messages that are based on the
-- message template, in plain text format.
--
-- 'defaultSubstitutions', 'voiceTemplateResponse_defaultSubstitutions' - The JSON object that specifies the default values that are used for
-- message variables in the message template. This object is a set of
-- key-value pairs. Each key defines a message variable in the template.
-- The corresponding value defines the default value for that variable.
--
-- 'languageCode', 'voiceTemplateResponse_languageCode' - The code for the language that\'s used when synthesizing the text of the
-- script in messages that are based on the message template. For a list of
-- supported languages and the code for each one, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
--
-- 'templateDescription', 'voiceTemplateResponse_templateDescription' - The custom description of the message template.
--
-- 'version', 'voiceTemplateResponse_version' - The unique identifier, as an integer, for the active version of the
-- message template, or the version of the template that you specified by
-- using the version parameter in your request.
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
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateType'
  TemplateType ->
  VoiceTemplateResponse
newVoiceTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    VoiceTemplateResponse'
      { tags = Prelude.Nothing,
        voiceId = Prelude.Nothing,
        arn = Prelude.Nothing,
        body = Prelude.Nothing,
        defaultSubstitutions = Prelude.Nothing,
        languageCode = Prelude.Nothing,
        templateDescription = Prelude.Nothing,
        version = Prelude.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        templateName = pTemplateName_,
        templateType = pTemplateType_
      }

-- | A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the message template. Each tag consists of a
-- required tag key and an associated tag value.
voiceTemplateResponse_tags :: Lens.Lens' VoiceTemplateResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
voiceTemplateResponse_tags = Lens.lens (\VoiceTemplateResponse' {tags} -> tags) (\s@VoiceTemplateResponse' {} a -> s {tags = a} :: VoiceTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the voice that\'s used when delivering messages that are
-- based on the message template. For a list of supported voices, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
voiceTemplateResponse_voiceId :: Lens.Lens' VoiceTemplateResponse (Prelude.Maybe Prelude.Text)
voiceTemplateResponse_voiceId = Lens.lens (\VoiceTemplateResponse' {voiceId} -> voiceId) (\s@VoiceTemplateResponse' {} a -> s {voiceId = a} :: VoiceTemplateResponse)

-- | The Amazon Resource Name (ARN) of the message template.
voiceTemplateResponse_arn :: Lens.Lens' VoiceTemplateResponse (Prelude.Maybe Prelude.Text)
voiceTemplateResponse_arn = Lens.lens (\VoiceTemplateResponse' {arn} -> arn) (\s@VoiceTemplateResponse' {} a -> s {arn = a} :: VoiceTemplateResponse)

-- | The text of the script that\'s used in messages that are based on the
-- message template, in plain text format.
voiceTemplateResponse_body :: Lens.Lens' VoiceTemplateResponse (Prelude.Maybe Prelude.Text)
voiceTemplateResponse_body = Lens.lens (\VoiceTemplateResponse' {body} -> body) (\s@VoiceTemplateResponse' {} a -> s {body = a} :: VoiceTemplateResponse)

-- | The JSON object that specifies the default values that are used for
-- message variables in the message template. This object is a set of
-- key-value pairs. Each key defines a message variable in the template.
-- The corresponding value defines the default value for that variable.
voiceTemplateResponse_defaultSubstitutions :: Lens.Lens' VoiceTemplateResponse (Prelude.Maybe Prelude.Text)
voiceTemplateResponse_defaultSubstitutions = Lens.lens (\VoiceTemplateResponse' {defaultSubstitutions} -> defaultSubstitutions) (\s@VoiceTemplateResponse' {} a -> s {defaultSubstitutions = a} :: VoiceTemplateResponse)

-- | The code for the language that\'s used when synthesizing the text of the
-- script in messages that are based on the message template. For a list of
-- supported languages and the code for each one, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
voiceTemplateResponse_languageCode :: Lens.Lens' VoiceTemplateResponse (Prelude.Maybe Prelude.Text)
voiceTemplateResponse_languageCode = Lens.lens (\VoiceTemplateResponse' {languageCode} -> languageCode) (\s@VoiceTemplateResponse' {} a -> s {languageCode = a} :: VoiceTemplateResponse)

-- | The custom description of the message template.
voiceTemplateResponse_templateDescription :: Lens.Lens' VoiceTemplateResponse (Prelude.Maybe Prelude.Text)
voiceTemplateResponse_templateDescription = Lens.lens (\VoiceTemplateResponse' {templateDescription} -> templateDescription) (\s@VoiceTemplateResponse' {} a -> s {templateDescription = a} :: VoiceTemplateResponse)

-- | The unique identifier, as an integer, for the active version of the
-- message template, or the version of the template that you specified by
-- using the version parameter in your request.
voiceTemplateResponse_version :: Lens.Lens' VoiceTemplateResponse (Prelude.Maybe Prelude.Text)
voiceTemplateResponse_version = Lens.lens (\VoiceTemplateResponse' {version} -> version) (\s@VoiceTemplateResponse' {} a -> s {version = a} :: VoiceTemplateResponse)

-- | The date, in ISO 8601 format, when the message template was last
-- modified.
voiceTemplateResponse_lastModifiedDate :: Lens.Lens' VoiceTemplateResponse Prelude.Text
voiceTemplateResponse_lastModifiedDate = Lens.lens (\VoiceTemplateResponse' {lastModifiedDate} -> lastModifiedDate) (\s@VoiceTemplateResponse' {} a -> s {lastModifiedDate = a} :: VoiceTemplateResponse)

-- | The date, in ISO 8601 format, when the message template was created.
voiceTemplateResponse_creationDate :: Lens.Lens' VoiceTemplateResponse Prelude.Text
voiceTemplateResponse_creationDate = Lens.lens (\VoiceTemplateResponse' {creationDate} -> creationDate) (\s@VoiceTemplateResponse' {} a -> s {creationDate = a} :: VoiceTemplateResponse)

-- | The name of the message template.
voiceTemplateResponse_templateName :: Lens.Lens' VoiceTemplateResponse Prelude.Text
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
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "VoiceId")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Body")
            Prelude.<*> (x Core..:? "DefaultSubstitutions")
            Prelude.<*> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "TemplateDescription")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..: "LastModifiedDate")
            Prelude.<*> (x Core..: "CreationDate")
            Prelude.<*> (x Core..: "TemplateName")
            Prelude.<*> (x Core..: "TemplateType")
      )

instance Prelude.Hashable VoiceTemplateResponse where
  hashWithSalt _salt VoiceTemplateResponse' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` voiceId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` defaultSubstitutions
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` templateDescription
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateType

instance Prelude.NFData VoiceTemplateResponse where
  rnf VoiceTemplateResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf voiceId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf body
      `Prelude.seq` Prelude.rnf defaultSubstitutions
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf templateDescription
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateType
