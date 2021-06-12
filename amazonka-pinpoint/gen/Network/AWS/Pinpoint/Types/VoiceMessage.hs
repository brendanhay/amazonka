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
-- Module      : Network.AWS.Pinpoint.Types.VoiceMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.VoiceMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the settings for a one-time voice message that\'s sent
-- directly to an endpoint through the voice channel.
--
-- /See:/ 'newVoiceMessage' smart constructor.
data VoiceMessage = VoiceMessage'
  { -- | The code for the language to use when synthesizing the text of the
    -- message script. For a list of supported languages and the code for each
    -- one, see the
    -- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
    languageCode :: Core.Maybe Core.Text,
    -- | The name of the voice to use when delivering the message. For a list of
    -- supported voices, see the
    -- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
    voiceId :: Core.Maybe Core.Text,
    -- | The text of the script to use for the voice message.
    body :: Core.Maybe Core.Text,
    -- | The default message variables to use in the voice message. You can
    -- override the default variables with individual address variables.
    substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The long code to send the voice message from. This value should be one
    -- of the dedicated long codes that\'s assigned to your AWS account.
    -- Although it isn\'t required, we recommend that you specify the long code
    -- in E.164 format, for example +12065550100, to ensure prompt and accurate
    -- delivery of the message.
    originationNumber :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VoiceMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'voiceMessage_languageCode' - The code for the language to use when synthesizing the text of the
-- message script. For a list of supported languages and the code for each
-- one, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
--
-- 'voiceId', 'voiceMessage_voiceId' - The name of the voice to use when delivering the message. For a list of
-- supported voices, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
--
-- 'body', 'voiceMessage_body' - The text of the script to use for the voice message.
--
-- 'substitutions', 'voiceMessage_substitutions' - The default message variables to use in the voice message. You can
-- override the default variables with individual address variables.
--
-- 'originationNumber', 'voiceMessage_originationNumber' - The long code to send the voice message from. This value should be one
-- of the dedicated long codes that\'s assigned to your AWS account.
-- Although it isn\'t required, we recommend that you specify the long code
-- in E.164 format, for example +12065550100, to ensure prompt and accurate
-- delivery of the message.
newVoiceMessage ::
  VoiceMessage
newVoiceMessage =
  VoiceMessage'
    { languageCode = Core.Nothing,
      voiceId = Core.Nothing,
      body = Core.Nothing,
      substitutions = Core.Nothing,
      originationNumber = Core.Nothing
    }

-- | The code for the language to use when synthesizing the text of the
-- message script. For a list of supported languages and the code for each
-- one, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
voiceMessage_languageCode :: Lens.Lens' VoiceMessage (Core.Maybe Core.Text)
voiceMessage_languageCode = Lens.lens (\VoiceMessage' {languageCode} -> languageCode) (\s@VoiceMessage' {} a -> s {languageCode = a} :: VoiceMessage)

-- | The name of the voice to use when delivering the message. For a list of
-- supported voices, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide>.
voiceMessage_voiceId :: Lens.Lens' VoiceMessage (Core.Maybe Core.Text)
voiceMessage_voiceId = Lens.lens (\VoiceMessage' {voiceId} -> voiceId) (\s@VoiceMessage' {} a -> s {voiceId = a} :: VoiceMessage)

-- | The text of the script to use for the voice message.
voiceMessage_body :: Lens.Lens' VoiceMessage (Core.Maybe Core.Text)
voiceMessage_body = Lens.lens (\VoiceMessage' {body} -> body) (\s@VoiceMessage' {} a -> s {body = a} :: VoiceMessage)

-- | The default message variables to use in the voice message. You can
-- override the default variables with individual address variables.
voiceMessage_substitutions :: Lens.Lens' VoiceMessage (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
voiceMessage_substitutions = Lens.lens (\VoiceMessage' {substitutions} -> substitutions) (\s@VoiceMessage' {} a -> s {substitutions = a} :: VoiceMessage) Core.. Lens.mapping Lens._Coerce

-- | The long code to send the voice message from. This value should be one
-- of the dedicated long codes that\'s assigned to your AWS account.
-- Although it isn\'t required, we recommend that you specify the long code
-- in E.164 format, for example +12065550100, to ensure prompt and accurate
-- delivery of the message.
voiceMessage_originationNumber :: Lens.Lens' VoiceMessage (Core.Maybe Core.Text)
voiceMessage_originationNumber = Lens.lens (\VoiceMessage' {originationNumber} -> originationNumber) (\s@VoiceMessage' {} a -> s {originationNumber = a} :: VoiceMessage)

instance Core.Hashable VoiceMessage

instance Core.NFData VoiceMessage

instance Core.ToJSON VoiceMessage where
  toJSON VoiceMessage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LanguageCode" Core..=) Core.<$> languageCode,
            ("VoiceId" Core..=) Core.<$> voiceId,
            ("Body" Core..=) Core.<$> body,
            ("Substitutions" Core..=) Core.<$> substitutions,
            ("OriginationNumber" Core..=)
              Core.<$> originationNumber
          ]
      )
