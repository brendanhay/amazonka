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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSMSVoice.Types.VoiceMessageContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PinpointSMSVoice.Types.CallInstructionsMessageType
import Amazonka.PinpointSMSVoice.Types.PlainTextMessageType
import Amazonka.PinpointSMSVoice.Types.SSMLMessageType
import qualified Amazonka.Prelude as Prelude

-- | An object that contains a voice message and information about the
-- recipient that you want to send it to.
--
-- /See:/ 'newVoiceMessageContent' smart constructor.
data VoiceMessageContent = VoiceMessageContent'
  { sSMLMessage :: Prelude.Maybe SSMLMessageType,
    callInstructionsMessage :: Prelude.Maybe CallInstructionsMessageType,
    plainTextMessage :: Prelude.Maybe PlainTextMessageType
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
-- 'sSMLMessage', 'voiceMessageContent_sSMLMessage' - Undocumented member.
--
-- 'callInstructionsMessage', 'voiceMessageContent_callInstructionsMessage' - Undocumented member.
--
-- 'plainTextMessage', 'voiceMessageContent_plainTextMessage' - Undocumented member.
newVoiceMessageContent ::
  VoiceMessageContent
newVoiceMessageContent =
  VoiceMessageContent'
    { sSMLMessage = Prelude.Nothing,
      callInstructionsMessage = Prelude.Nothing,
      plainTextMessage = Prelude.Nothing
    }

-- | Undocumented member.
voiceMessageContent_sSMLMessage :: Lens.Lens' VoiceMessageContent (Prelude.Maybe SSMLMessageType)
voiceMessageContent_sSMLMessage = Lens.lens (\VoiceMessageContent' {sSMLMessage} -> sSMLMessage) (\s@VoiceMessageContent' {} a -> s {sSMLMessage = a} :: VoiceMessageContent)

-- | Undocumented member.
voiceMessageContent_callInstructionsMessage :: Lens.Lens' VoiceMessageContent (Prelude.Maybe CallInstructionsMessageType)
voiceMessageContent_callInstructionsMessage = Lens.lens (\VoiceMessageContent' {callInstructionsMessage} -> callInstructionsMessage) (\s@VoiceMessageContent' {} a -> s {callInstructionsMessage = a} :: VoiceMessageContent)

-- | Undocumented member.
voiceMessageContent_plainTextMessage :: Lens.Lens' VoiceMessageContent (Prelude.Maybe PlainTextMessageType)
voiceMessageContent_plainTextMessage = Lens.lens (\VoiceMessageContent' {plainTextMessage} -> plainTextMessage) (\s@VoiceMessageContent' {} a -> s {plainTextMessage = a} :: VoiceMessageContent)

instance Prelude.Hashable VoiceMessageContent where
  hashWithSalt _salt VoiceMessageContent' {..} =
    _salt `Prelude.hashWithSalt` sSMLMessage
      `Prelude.hashWithSalt` callInstructionsMessage
      `Prelude.hashWithSalt` plainTextMessage

instance Prelude.NFData VoiceMessageContent where
  rnf VoiceMessageContent' {..} =
    Prelude.rnf sSMLMessage
      `Prelude.seq` Prelude.rnf callInstructionsMessage
      `Prelude.seq` Prelude.rnf plainTextMessage

instance Core.ToJSON VoiceMessageContent where
  toJSON VoiceMessageContent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SSMLMessage" Core..=) Prelude.<$> sSMLMessage,
            ("CallInstructionsMessage" Core..=)
              Prelude.<$> callInstructionsMessage,
            ("PlainTextMessage" Core..=)
              Prelude.<$> plainTextMessage
          ]
      )
