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
-- Module      : Amazonka.PinpointSMSVoice.Types.VoiceMessageContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSMSVoice.Types.VoiceMessageContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSMSVoice.Types.CallInstructionsMessageType
import Amazonka.PinpointSMSVoice.Types.PlainTextMessageType
import Amazonka.PinpointSMSVoice.Types.SSMLMessageType
import qualified Amazonka.Prelude as Prelude

-- | An object that contains a voice message and information about the
-- recipient that you want to send it to.
--
-- /See:/ 'newVoiceMessageContent' smart constructor.
data VoiceMessageContent = VoiceMessageContent'
  { callInstructionsMessage :: Prelude.Maybe CallInstructionsMessageType,
    plainTextMessage :: Prelude.Maybe PlainTextMessageType,
    sSMLMessage :: Prelude.Maybe SSMLMessageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceMessageContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callInstructionsMessage', 'voiceMessageContent_callInstructionsMessage' - Undocumented member.
--
-- 'plainTextMessage', 'voiceMessageContent_plainTextMessage' - Undocumented member.
--
-- 'sSMLMessage', 'voiceMessageContent_sSMLMessage' - Undocumented member.
newVoiceMessageContent ::
  VoiceMessageContent
newVoiceMessageContent =
  VoiceMessageContent'
    { callInstructionsMessage =
        Prelude.Nothing,
      plainTextMessage = Prelude.Nothing,
      sSMLMessage = Prelude.Nothing
    }

-- | Undocumented member.
voiceMessageContent_callInstructionsMessage :: Lens.Lens' VoiceMessageContent (Prelude.Maybe CallInstructionsMessageType)
voiceMessageContent_callInstructionsMessage = Lens.lens (\VoiceMessageContent' {callInstructionsMessage} -> callInstructionsMessage) (\s@VoiceMessageContent' {} a -> s {callInstructionsMessage = a} :: VoiceMessageContent)

-- | Undocumented member.
voiceMessageContent_plainTextMessage :: Lens.Lens' VoiceMessageContent (Prelude.Maybe PlainTextMessageType)
voiceMessageContent_plainTextMessage = Lens.lens (\VoiceMessageContent' {plainTextMessage} -> plainTextMessage) (\s@VoiceMessageContent' {} a -> s {plainTextMessage = a} :: VoiceMessageContent)

-- | Undocumented member.
voiceMessageContent_sSMLMessage :: Lens.Lens' VoiceMessageContent (Prelude.Maybe SSMLMessageType)
voiceMessageContent_sSMLMessage = Lens.lens (\VoiceMessageContent' {sSMLMessage} -> sSMLMessage) (\s@VoiceMessageContent' {} a -> s {sSMLMessage = a} :: VoiceMessageContent)

instance Prelude.Hashable VoiceMessageContent where
  hashWithSalt _salt VoiceMessageContent' {..} =
    _salt
      `Prelude.hashWithSalt` callInstructionsMessage
      `Prelude.hashWithSalt` plainTextMessage
      `Prelude.hashWithSalt` sSMLMessage

instance Prelude.NFData VoiceMessageContent where
  rnf VoiceMessageContent' {..} =
    Prelude.rnf callInstructionsMessage `Prelude.seq`
      Prelude.rnf plainTextMessage `Prelude.seq`
        Prelude.rnf sSMLMessage

instance Data.ToJSON VoiceMessageContent where
  toJSON VoiceMessageContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CallInstructionsMessage" Data..=)
              Prelude.<$> callInstructionsMessage,
            ("PlainTextMessage" Data..=)
              Prelude.<$> plainTextMessage,
            ("SSMLMessage" Data..=) Prelude.<$> sSMLMessage
          ]
      )
