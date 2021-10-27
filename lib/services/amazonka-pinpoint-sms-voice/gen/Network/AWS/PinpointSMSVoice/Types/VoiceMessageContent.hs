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
-- Module      : Network.AWS.PinpointSMSVoice.Types.VoiceMessageContent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.PinpointSMSVoice.Types.VoiceMessageContent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.PinpointSMSVoice.Types.CallInstructionsMessageType
import Network.AWS.PinpointSMSVoice.Types.PlainTextMessageType
import Network.AWS.PinpointSMSVoice.Types.SSMLMessageType
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains a voice message and information about the
-- recipient that you want to send it to.
--
-- /See:/ 'newVoiceMessageContent' smart constructor.
data VoiceMessageContent = VoiceMessageContent'
  { callInstructionsMessage :: Prelude.Maybe CallInstructionsMessageType,
    sSMLMessage :: Prelude.Maybe SSMLMessageType,
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
-- 'callInstructionsMessage', 'voiceMessageContent_callInstructionsMessage' - Undocumented member.
--
-- 'sSMLMessage', 'voiceMessageContent_sSMLMessage' - Undocumented member.
--
-- 'plainTextMessage', 'voiceMessageContent_plainTextMessage' - Undocumented member.
newVoiceMessageContent ::
  VoiceMessageContent
newVoiceMessageContent =
  VoiceMessageContent'
    { callInstructionsMessage =
        Prelude.Nothing,
      sSMLMessage = Prelude.Nothing,
      plainTextMessage = Prelude.Nothing
    }

-- | Undocumented member.
voiceMessageContent_callInstructionsMessage :: Lens.Lens' VoiceMessageContent (Prelude.Maybe CallInstructionsMessageType)
voiceMessageContent_callInstructionsMessage = Lens.lens (\VoiceMessageContent' {callInstructionsMessage} -> callInstructionsMessage) (\s@VoiceMessageContent' {} a -> s {callInstructionsMessage = a} :: VoiceMessageContent)

-- | Undocumented member.
voiceMessageContent_sSMLMessage :: Lens.Lens' VoiceMessageContent (Prelude.Maybe SSMLMessageType)
voiceMessageContent_sSMLMessage = Lens.lens (\VoiceMessageContent' {sSMLMessage} -> sSMLMessage) (\s@VoiceMessageContent' {} a -> s {sSMLMessage = a} :: VoiceMessageContent)

-- | Undocumented member.
voiceMessageContent_plainTextMessage :: Lens.Lens' VoiceMessageContent (Prelude.Maybe PlainTextMessageType)
voiceMessageContent_plainTextMessage = Lens.lens (\VoiceMessageContent' {plainTextMessage} -> plainTextMessage) (\s@VoiceMessageContent' {} a -> s {plainTextMessage = a} :: VoiceMessageContent)

instance Prelude.Hashable VoiceMessageContent

instance Prelude.NFData VoiceMessageContent

instance Core.ToJSON VoiceMessageContent where
  toJSON VoiceMessageContent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CallInstructionsMessage" Core..=)
              Prelude.<$> callInstructionsMessage,
            ("SSMLMessage" Core..=) Prelude.<$> sSMLMessage,
            ("PlainTextMessage" Core..=)
              Prelude.<$> plainTextMessage
          ]
      )
