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
-- Module      : Amazonka.Pinpoint.Types.VoiceTemplateRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.VoiceTemplateRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the content and settings for a message template that can be
-- used in messages that are sent through the voice channel.
--
-- /See:/ 'newVoiceTemplateRequest' smart constructor.
data VoiceTemplateRequest = VoiceTemplateRequest'
  { -- | The text of the script to use in messages that are based on the message
    -- template, in plain text format.
    body :: Prelude.Maybe Prelude.Text,
    -- | A JSON object that specifies the default values to use for message
    -- variables in the message template. This object is a set of key-value
    -- pairs. Each key defines a message variable in the template. The
    -- corresponding value defines the default value for that variable. When
    -- you create a message that\'s based on the template, you can override
    -- these defaults with message-specific and address-specific variables and
    -- values.
    defaultSubstitutions :: Prelude.Maybe Prelude.Text,
    -- | The code for the language to use when synthesizing the text of the
    -- script in messages that are based on the message template. For a list of
    -- supported languages and the code for each one, see the
    -- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | A custom description of the message template.
    templateDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the voice to use when delivering messages that are based on
    -- the message template. For a list of supported voices, see the
    -- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
    voiceId :: Prelude.Maybe Prelude.Text,
    -- | A string-to-string map of key-value pairs that defines the tags to
    -- associate with the message template. Each tag consists of a required tag
    -- key and an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceTemplateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'voiceTemplateRequest_body' - The text of the script to use in messages that are based on the message
-- template, in plain text format.
--
-- 'defaultSubstitutions', 'voiceTemplateRequest_defaultSubstitutions' - A JSON object that specifies the default values to use for message
-- variables in the message template. This object is a set of key-value
-- pairs. Each key defines a message variable in the template. The
-- corresponding value defines the default value for that variable. When
-- you create a message that\'s based on the template, you can override
-- these defaults with message-specific and address-specific variables and
-- values.
--
-- 'languageCode', 'voiceTemplateRequest_languageCode' - The code for the language to use when synthesizing the text of the
-- script in messages that are based on the message template. For a list of
-- supported languages and the code for each one, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
--
-- 'templateDescription', 'voiceTemplateRequest_templateDescription' - A custom description of the message template.
--
-- 'voiceId', 'voiceTemplateRequest_voiceId' - The name of the voice to use when delivering messages that are based on
-- the message template. For a list of supported voices, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
--
-- 'tags', 'voiceTemplateRequest_tags' - A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
newVoiceTemplateRequest ::
  VoiceTemplateRequest
newVoiceTemplateRequest =
  VoiceTemplateRequest'
    { body = Prelude.Nothing,
      defaultSubstitutions = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      templateDescription = Prelude.Nothing,
      voiceId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The text of the script to use in messages that are based on the message
-- template, in plain text format.
voiceTemplateRequest_body :: Lens.Lens' VoiceTemplateRequest (Prelude.Maybe Prelude.Text)
voiceTemplateRequest_body = Lens.lens (\VoiceTemplateRequest' {body} -> body) (\s@VoiceTemplateRequest' {} a -> s {body = a} :: VoiceTemplateRequest)

-- | A JSON object that specifies the default values to use for message
-- variables in the message template. This object is a set of key-value
-- pairs. Each key defines a message variable in the template. The
-- corresponding value defines the default value for that variable. When
-- you create a message that\'s based on the template, you can override
-- these defaults with message-specific and address-specific variables and
-- values.
voiceTemplateRequest_defaultSubstitutions :: Lens.Lens' VoiceTemplateRequest (Prelude.Maybe Prelude.Text)
voiceTemplateRequest_defaultSubstitutions = Lens.lens (\VoiceTemplateRequest' {defaultSubstitutions} -> defaultSubstitutions) (\s@VoiceTemplateRequest' {} a -> s {defaultSubstitutions = a} :: VoiceTemplateRequest)

-- | The code for the language to use when synthesizing the text of the
-- script in messages that are based on the message template. For a list of
-- supported languages and the code for each one, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
voiceTemplateRequest_languageCode :: Lens.Lens' VoiceTemplateRequest (Prelude.Maybe Prelude.Text)
voiceTemplateRequest_languageCode = Lens.lens (\VoiceTemplateRequest' {languageCode} -> languageCode) (\s@VoiceTemplateRequest' {} a -> s {languageCode = a} :: VoiceTemplateRequest)

-- | A custom description of the message template.
voiceTemplateRequest_templateDescription :: Lens.Lens' VoiceTemplateRequest (Prelude.Maybe Prelude.Text)
voiceTemplateRequest_templateDescription = Lens.lens (\VoiceTemplateRequest' {templateDescription} -> templateDescription) (\s@VoiceTemplateRequest' {} a -> s {templateDescription = a} :: VoiceTemplateRequest)

-- | The name of the voice to use when delivering messages that are based on
-- the message template. For a list of supported voices, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
voiceTemplateRequest_voiceId :: Lens.Lens' VoiceTemplateRequest (Prelude.Maybe Prelude.Text)
voiceTemplateRequest_voiceId = Lens.lens (\VoiceTemplateRequest' {voiceId} -> voiceId) (\s@VoiceTemplateRequest' {} a -> s {voiceId = a} :: VoiceTemplateRequest)

-- | A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
voiceTemplateRequest_tags :: Lens.Lens' VoiceTemplateRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
voiceTemplateRequest_tags = Lens.lens (\VoiceTemplateRequest' {tags} -> tags) (\s@VoiceTemplateRequest' {} a -> s {tags = a} :: VoiceTemplateRequest) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable VoiceTemplateRequest where
  hashWithSalt _salt VoiceTemplateRequest' {..} =
    _salt
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` defaultSubstitutions
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` templateDescription
      `Prelude.hashWithSalt` voiceId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData VoiceTemplateRequest where
  rnf VoiceTemplateRequest' {..} =
    Prelude.rnf body `Prelude.seq`
      Prelude.rnf defaultSubstitutions `Prelude.seq`
        Prelude.rnf languageCode `Prelude.seq`
          Prelude.rnf templateDescription `Prelude.seq`
            Prelude.rnf voiceId `Prelude.seq`
              Prelude.rnf tags

instance Data.ToJSON VoiceTemplateRequest where
  toJSON VoiceTemplateRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Body" Data..=) Prelude.<$> body,
            ("DefaultSubstitutions" Data..=)
              Prelude.<$> defaultSubstitutions,
            ("LanguageCode" Data..=) Prelude.<$> languageCode,
            ("TemplateDescription" Data..=)
              Prelude.<$> templateDescription,
            ("VoiceId" Data..=) Prelude.<$> voiceId,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )
