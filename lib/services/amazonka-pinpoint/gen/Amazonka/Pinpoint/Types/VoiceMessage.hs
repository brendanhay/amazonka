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
-- Module      : Amazonka.Pinpoint.Types.VoiceMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.VoiceMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings for a one-time voice message that\'s sent
-- directly to an endpoint through the voice channel.
--
-- /See:/ 'newVoiceMessage' smart constructor.
data VoiceMessage = VoiceMessage'
  { -- | The text of the script to use for the voice message.
    body :: Prelude.Maybe Prelude.Text,
    -- | The code for the language to use when synthesizing the text of the
    -- message script. For a list of supported languages and the code for each
    -- one, see the
    -- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The long code to send the voice message from. This value should be one
    -- of the dedicated long codes that\'s assigned to your AWS account.
    -- Although it isn\'t required, we recommend that you specify the long code
    -- in E.164 format, for example +12065550100, to ensure prompt and accurate
    -- delivery of the message.
    originationNumber :: Prelude.Maybe Prelude.Text,
    -- | The default message variables to use in the voice message. You can
    -- override the default variables with individual address variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The name of the voice to use when delivering the message. For a list of
    -- supported voices, see the
    -- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
    voiceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'voiceMessage_body' - The text of the script to use for the voice message.
--
-- 'languageCode', 'voiceMessage_languageCode' - The code for the language to use when synthesizing the text of the
-- message script. For a list of supported languages and the code for each
-- one, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
--
-- 'originationNumber', 'voiceMessage_originationNumber' - The long code to send the voice message from. This value should be one
-- of the dedicated long codes that\'s assigned to your AWS account.
-- Although it isn\'t required, we recommend that you specify the long code
-- in E.164 format, for example +12065550100, to ensure prompt and accurate
-- delivery of the message.
--
-- 'substitutions', 'voiceMessage_substitutions' - The default message variables to use in the voice message. You can
-- override the default variables with individual address variables.
--
-- 'voiceId', 'voiceMessage_voiceId' - The name of the voice to use when delivering the message. For a list of
-- supported voices, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
newVoiceMessage ::
  VoiceMessage
newVoiceMessage =
  VoiceMessage'
    { body = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      originationNumber = Prelude.Nothing,
      substitutions = Prelude.Nothing,
      voiceId = Prelude.Nothing
    }

-- | The text of the script to use for the voice message.
voiceMessage_body :: Lens.Lens' VoiceMessage (Prelude.Maybe Prelude.Text)
voiceMessage_body = Lens.lens (\VoiceMessage' {body} -> body) (\s@VoiceMessage' {} a -> s {body = a} :: VoiceMessage)

-- | The code for the language to use when synthesizing the text of the
-- message script. For a list of supported languages and the code for each
-- one, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
voiceMessage_languageCode :: Lens.Lens' VoiceMessage (Prelude.Maybe Prelude.Text)
voiceMessage_languageCode = Lens.lens (\VoiceMessage' {languageCode} -> languageCode) (\s@VoiceMessage' {} a -> s {languageCode = a} :: VoiceMessage)

-- | The long code to send the voice message from. This value should be one
-- of the dedicated long codes that\'s assigned to your AWS account.
-- Although it isn\'t required, we recommend that you specify the long code
-- in E.164 format, for example +12065550100, to ensure prompt and accurate
-- delivery of the message.
voiceMessage_originationNumber :: Lens.Lens' VoiceMessage (Prelude.Maybe Prelude.Text)
voiceMessage_originationNumber = Lens.lens (\VoiceMessage' {originationNumber} -> originationNumber) (\s@VoiceMessage' {} a -> s {originationNumber = a} :: VoiceMessage)

-- | The default message variables to use in the voice message. You can
-- override the default variables with individual address variables.
voiceMessage_substitutions :: Lens.Lens' VoiceMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
voiceMessage_substitutions = Lens.lens (\VoiceMessage' {substitutions} -> substitutions) (\s@VoiceMessage' {} a -> s {substitutions = a} :: VoiceMessage) Prelude.. Lens.mapping Lens.coerced

-- | The name of the voice to use when delivering the message. For a list of
-- supported voices, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
voiceMessage_voiceId :: Lens.Lens' VoiceMessage (Prelude.Maybe Prelude.Text)
voiceMessage_voiceId = Lens.lens (\VoiceMessage' {voiceId} -> voiceId) (\s@VoiceMessage' {} a -> s {voiceId = a} :: VoiceMessage)

instance Prelude.Hashable VoiceMessage where
  hashWithSalt _salt VoiceMessage' {..} =
    _salt
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` originationNumber
      `Prelude.hashWithSalt` substitutions
      `Prelude.hashWithSalt` voiceId

instance Prelude.NFData VoiceMessage where
  rnf VoiceMessage' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf originationNumber
      `Prelude.seq` Prelude.rnf substitutions
      `Prelude.seq` Prelude.rnf voiceId

instance Data.ToJSON VoiceMessage where
  toJSON VoiceMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Body" Data..=) Prelude.<$> body,
            ("LanguageCode" Data..=) Prelude.<$> languageCode,
            ("OriginationNumber" Data..=)
              Prelude.<$> originationNumber,
            ("Substitutions" Data..=) Prelude.<$> substitutions,
            ("VoiceId" Data..=) Prelude.<$> voiceId
          ]
      )
