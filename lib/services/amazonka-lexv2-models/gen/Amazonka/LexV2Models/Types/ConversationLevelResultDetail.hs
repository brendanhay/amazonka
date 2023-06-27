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
-- Module      : Amazonka.LexV2Models.Types.ConversationLevelResultDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ConversationLevelResultDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TestResultMatchStatus
import qualified Amazonka.Prelude as Prelude

-- | The conversation level details of the conversation used in the test set.
--
-- /See:/ 'newConversationLevelResultDetail' smart constructor.
data ConversationLevelResultDetail = ConversationLevelResultDetail'
  { -- | The speech transcription success or failure details of the conversation.
    speechTranscriptionResult :: Prelude.Maybe TestResultMatchStatus,
    -- | The success or failure of the streaming of the conversation.
    endToEndResult :: TestResultMatchStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConversationLevelResultDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'speechTranscriptionResult', 'conversationLevelResultDetail_speechTranscriptionResult' - The speech transcription success or failure details of the conversation.
--
-- 'endToEndResult', 'conversationLevelResultDetail_endToEndResult' - The success or failure of the streaming of the conversation.
newConversationLevelResultDetail ::
  -- | 'endToEndResult'
  TestResultMatchStatus ->
  ConversationLevelResultDetail
newConversationLevelResultDetail pEndToEndResult_ =
  ConversationLevelResultDetail'
    { speechTranscriptionResult =
        Prelude.Nothing,
      endToEndResult = pEndToEndResult_
    }

-- | The speech transcription success or failure details of the conversation.
conversationLevelResultDetail_speechTranscriptionResult :: Lens.Lens' ConversationLevelResultDetail (Prelude.Maybe TestResultMatchStatus)
conversationLevelResultDetail_speechTranscriptionResult = Lens.lens (\ConversationLevelResultDetail' {speechTranscriptionResult} -> speechTranscriptionResult) (\s@ConversationLevelResultDetail' {} a -> s {speechTranscriptionResult = a} :: ConversationLevelResultDetail)

-- | The success or failure of the streaming of the conversation.
conversationLevelResultDetail_endToEndResult :: Lens.Lens' ConversationLevelResultDetail TestResultMatchStatus
conversationLevelResultDetail_endToEndResult = Lens.lens (\ConversationLevelResultDetail' {endToEndResult} -> endToEndResult) (\s@ConversationLevelResultDetail' {} a -> s {endToEndResult = a} :: ConversationLevelResultDetail)

instance Data.FromJSON ConversationLevelResultDetail where
  parseJSON =
    Data.withObject
      "ConversationLevelResultDetail"
      ( \x ->
          ConversationLevelResultDetail'
            Prelude.<$> (x Data..:? "speechTranscriptionResult")
            Prelude.<*> (x Data..: "endToEndResult")
      )

instance
  Prelude.Hashable
    ConversationLevelResultDetail
  where
  hashWithSalt _salt ConversationLevelResultDetail' {..} =
    _salt
      `Prelude.hashWithSalt` speechTranscriptionResult
      `Prelude.hashWithSalt` endToEndResult

instance Prelude.NFData ConversationLevelResultDetail where
  rnf ConversationLevelResultDetail' {..} =
    Prelude.rnf speechTranscriptionResult
      `Prelude.seq` Prelude.rnf endToEndResult
