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
-- Module      : Amazonka.LexV2Models.Types.ConversationLevelTestResultItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ConversationLevelTestResultItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ConversationLevelIntentClassificationResultItem
import Amazonka.LexV2Models.Types.ConversationLevelSlotResolutionResultItem
import Amazonka.LexV2Models.Types.TestResultMatchStatus
import qualified Amazonka.Prelude as Prelude

-- | The test result evaluation item at the conversation level.
--
-- /See:/ 'newConversationLevelTestResultItem' smart constructor.
data ConversationLevelTestResultItem = ConversationLevelTestResultItem'
  { -- | The speech transcription success or failure of the test result
    -- evaluation item.
    speechTranscriptionResult :: Prelude.Maybe TestResultMatchStatus,
    -- | The conversation Id of the test result evaluation item.
    conversationId :: Prelude.Text,
    -- | The end-to-end success or failure of the test result evaluation item.
    endToEndResult :: TestResultMatchStatus,
    -- | The intent classification of the test result evaluation item.
    intentClassificationResults :: [ConversationLevelIntentClassificationResultItem],
    -- | The slot success or failure of the test result evaluation item.
    slotResolutionResults :: [ConversationLevelSlotResolutionResultItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConversationLevelTestResultItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'speechTranscriptionResult', 'conversationLevelTestResultItem_speechTranscriptionResult' - The speech transcription success or failure of the test result
-- evaluation item.
--
-- 'conversationId', 'conversationLevelTestResultItem_conversationId' - The conversation Id of the test result evaluation item.
--
-- 'endToEndResult', 'conversationLevelTestResultItem_endToEndResult' - The end-to-end success or failure of the test result evaluation item.
--
-- 'intentClassificationResults', 'conversationLevelTestResultItem_intentClassificationResults' - The intent classification of the test result evaluation item.
--
-- 'slotResolutionResults', 'conversationLevelTestResultItem_slotResolutionResults' - The slot success or failure of the test result evaluation item.
newConversationLevelTestResultItem ::
  -- | 'conversationId'
  Prelude.Text ->
  -- | 'endToEndResult'
  TestResultMatchStatus ->
  ConversationLevelTestResultItem
newConversationLevelTestResultItem
  pConversationId_
  pEndToEndResult_ =
    ConversationLevelTestResultItem'
      { speechTranscriptionResult =
          Prelude.Nothing,
        conversationId = pConversationId_,
        endToEndResult = pEndToEndResult_,
        intentClassificationResults =
          Prelude.mempty,
        slotResolutionResults = Prelude.mempty
      }

-- | The speech transcription success or failure of the test result
-- evaluation item.
conversationLevelTestResultItem_speechTranscriptionResult :: Lens.Lens' ConversationLevelTestResultItem (Prelude.Maybe TestResultMatchStatus)
conversationLevelTestResultItem_speechTranscriptionResult = Lens.lens (\ConversationLevelTestResultItem' {speechTranscriptionResult} -> speechTranscriptionResult) (\s@ConversationLevelTestResultItem' {} a -> s {speechTranscriptionResult = a} :: ConversationLevelTestResultItem)

-- | The conversation Id of the test result evaluation item.
conversationLevelTestResultItem_conversationId :: Lens.Lens' ConversationLevelTestResultItem Prelude.Text
conversationLevelTestResultItem_conversationId = Lens.lens (\ConversationLevelTestResultItem' {conversationId} -> conversationId) (\s@ConversationLevelTestResultItem' {} a -> s {conversationId = a} :: ConversationLevelTestResultItem)

-- | The end-to-end success or failure of the test result evaluation item.
conversationLevelTestResultItem_endToEndResult :: Lens.Lens' ConversationLevelTestResultItem TestResultMatchStatus
conversationLevelTestResultItem_endToEndResult = Lens.lens (\ConversationLevelTestResultItem' {endToEndResult} -> endToEndResult) (\s@ConversationLevelTestResultItem' {} a -> s {endToEndResult = a} :: ConversationLevelTestResultItem)

-- | The intent classification of the test result evaluation item.
conversationLevelTestResultItem_intentClassificationResults :: Lens.Lens' ConversationLevelTestResultItem [ConversationLevelIntentClassificationResultItem]
conversationLevelTestResultItem_intentClassificationResults = Lens.lens (\ConversationLevelTestResultItem' {intentClassificationResults} -> intentClassificationResults) (\s@ConversationLevelTestResultItem' {} a -> s {intentClassificationResults = a} :: ConversationLevelTestResultItem) Prelude.. Lens.coerced

-- | The slot success or failure of the test result evaluation item.
conversationLevelTestResultItem_slotResolutionResults :: Lens.Lens' ConversationLevelTestResultItem [ConversationLevelSlotResolutionResultItem]
conversationLevelTestResultItem_slotResolutionResults = Lens.lens (\ConversationLevelTestResultItem' {slotResolutionResults} -> slotResolutionResults) (\s@ConversationLevelTestResultItem' {} a -> s {slotResolutionResults = a} :: ConversationLevelTestResultItem) Prelude.. Lens.coerced

instance
  Data.FromJSON
    ConversationLevelTestResultItem
  where
  parseJSON =
    Data.withObject
      "ConversationLevelTestResultItem"
      ( \x ->
          ConversationLevelTestResultItem'
            Prelude.<$> (x Data..:? "speechTranscriptionResult")
            Prelude.<*> (x Data..: "conversationId")
            Prelude.<*> (x Data..: "endToEndResult")
            Prelude.<*> ( x
                            Data..:? "intentClassificationResults"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "slotResolutionResults"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ConversationLevelTestResultItem
  where
  hashWithSalt
    _salt
    ConversationLevelTestResultItem' {..} =
      _salt
        `Prelude.hashWithSalt` speechTranscriptionResult
        `Prelude.hashWithSalt` conversationId
        `Prelude.hashWithSalt` endToEndResult
        `Prelude.hashWithSalt` intentClassificationResults
        `Prelude.hashWithSalt` slotResolutionResults

instance
  Prelude.NFData
    ConversationLevelTestResultItem
  where
  rnf ConversationLevelTestResultItem' {..} =
    Prelude.rnf speechTranscriptionResult
      `Prelude.seq` Prelude.rnf conversationId
      `Prelude.seq` Prelude.rnf endToEndResult
      `Prelude.seq` Prelude.rnf intentClassificationResults
      `Prelude.seq` Prelude.rnf slotResolutionResults
