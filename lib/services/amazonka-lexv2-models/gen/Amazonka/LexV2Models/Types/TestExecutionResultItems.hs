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
-- Module      : Amazonka.LexV2Models.Types.TestExecutionResultItems
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestExecutionResultItems where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ConversationLevelTestResults
import Amazonka.LexV2Models.Types.IntentClassificationTestResults
import Amazonka.LexV2Models.Types.IntentLevelSlotResolutionTestResults
import Amazonka.LexV2Models.Types.OverallTestResults
import Amazonka.LexV2Models.Types.UtteranceLevelTestResults
import qualified Amazonka.Prelude as Prelude

-- | Contains the results of the test execution, grouped by type of results.
-- See
-- <https://docs.aws.amazon.com/lexv2/latest/dg/test-results-details-test-set.html Test result details\">Test results details>
-- for details about different types of results.
--
-- /See:/ 'newTestExecutionResultItems' smart constructor.
data TestExecutionResultItems = TestExecutionResultItems'
  { -- | Results related to conversations in the test set, including metrics
    -- about success and failure of conversations and intent and slot failures.
    conversationLevelTestResults :: Prelude.Maybe ConversationLevelTestResults,
    -- | Intent recognition results aggregated by intent name. The aggregated
    -- results contain success and failure rates of intent recognition, speech
    -- transcriptions, and end-to-end conversations.
    intentClassificationTestResults :: Prelude.Maybe IntentClassificationTestResults,
    -- | Slot resolution results aggregated by intent and slot name. The
    -- aggregated results contain success and failure rates of slot resolution,
    -- speech transcriptions, and end-to-end conversations
    intentLevelSlotResolutionTestResults :: Prelude.Maybe IntentLevelSlotResolutionTestResults,
    -- | Overall results for the test execution, including the breakdown of
    -- conversations and single-input utterances.
    overallTestResults :: Prelude.Maybe OverallTestResults,
    -- | Results related to utterances in the test set.
    utteranceLevelTestResults :: Prelude.Maybe UtteranceLevelTestResults
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestExecutionResultItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conversationLevelTestResults', 'testExecutionResultItems_conversationLevelTestResults' - Results related to conversations in the test set, including metrics
-- about success and failure of conversations and intent and slot failures.
--
-- 'intentClassificationTestResults', 'testExecutionResultItems_intentClassificationTestResults' - Intent recognition results aggregated by intent name. The aggregated
-- results contain success and failure rates of intent recognition, speech
-- transcriptions, and end-to-end conversations.
--
-- 'intentLevelSlotResolutionTestResults', 'testExecutionResultItems_intentLevelSlotResolutionTestResults' - Slot resolution results aggregated by intent and slot name. The
-- aggregated results contain success and failure rates of slot resolution,
-- speech transcriptions, and end-to-end conversations
--
-- 'overallTestResults', 'testExecutionResultItems_overallTestResults' - Overall results for the test execution, including the breakdown of
-- conversations and single-input utterances.
--
-- 'utteranceLevelTestResults', 'testExecutionResultItems_utteranceLevelTestResults' - Results related to utterances in the test set.
newTestExecutionResultItems ::
  TestExecutionResultItems
newTestExecutionResultItems =
  TestExecutionResultItems'
    { conversationLevelTestResults =
        Prelude.Nothing,
      intentClassificationTestResults = Prelude.Nothing,
      intentLevelSlotResolutionTestResults =
        Prelude.Nothing,
      overallTestResults = Prelude.Nothing,
      utteranceLevelTestResults = Prelude.Nothing
    }

-- | Results related to conversations in the test set, including metrics
-- about success and failure of conversations and intent and slot failures.
testExecutionResultItems_conversationLevelTestResults :: Lens.Lens' TestExecutionResultItems (Prelude.Maybe ConversationLevelTestResults)
testExecutionResultItems_conversationLevelTestResults = Lens.lens (\TestExecutionResultItems' {conversationLevelTestResults} -> conversationLevelTestResults) (\s@TestExecutionResultItems' {} a -> s {conversationLevelTestResults = a} :: TestExecutionResultItems)

-- | Intent recognition results aggregated by intent name. The aggregated
-- results contain success and failure rates of intent recognition, speech
-- transcriptions, and end-to-end conversations.
testExecutionResultItems_intentClassificationTestResults :: Lens.Lens' TestExecutionResultItems (Prelude.Maybe IntentClassificationTestResults)
testExecutionResultItems_intentClassificationTestResults = Lens.lens (\TestExecutionResultItems' {intentClassificationTestResults} -> intentClassificationTestResults) (\s@TestExecutionResultItems' {} a -> s {intentClassificationTestResults = a} :: TestExecutionResultItems)

-- | Slot resolution results aggregated by intent and slot name. The
-- aggregated results contain success and failure rates of slot resolution,
-- speech transcriptions, and end-to-end conversations
testExecutionResultItems_intentLevelSlotResolutionTestResults :: Lens.Lens' TestExecutionResultItems (Prelude.Maybe IntentLevelSlotResolutionTestResults)
testExecutionResultItems_intentLevelSlotResolutionTestResults = Lens.lens (\TestExecutionResultItems' {intentLevelSlotResolutionTestResults} -> intentLevelSlotResolutionTestResults) (\s@TestExecutionResultItems' {} a -> s {intentLevelSlotResolutionTestResults = a} :: TestExecutionResultItems)

-- | Overall results for the test execution, including the breakdown of
-- conversations and single-input utterances.
testExecutionResultItems_overallTestResults :: Lens.Lens' TestExecutionResultItems (Prelude.Maybe OverallTestResults)
testExecutionResultItems_overallTestResults = Lens.lens (\TestExecutionResultItems' {overallTestResults} -> overallTestResults) (\s@TestExecutionResultItems' {} a -> s {overallTestResults = a} :: TestExecutionResultItems)

-- | Results related to utterances in the test set.
testExecutionResultItems_utteranceLevelTestResults :: Lens.Lens' TestExecutionResultItems (Prelude.Maybe UtteranceLevelTestResults)
testExecutionResultItems_utteranceLevelTestResults = Lens.lens (\TestExecutionResultItems' {utteranceLevelTestResults} -> utteranceLevelTestResults) (\s@TestExecutionResultItems' {} a -> s {utteranceLevelTestResults = a} :: TestExecutionResultItems)

instance Data.FromJSON TestExecutionResultItems where
  parseJSON =
    Data.withObject
      "TestExecutionResultItems"
      ( \x ->
          TestExecutionResultItems'
            Prelude.<$> (x Data..:? "conversationLevelTestResults")
            Prelude.<*> (x Data..:? "intentClassificationTestResults")
            Prelude.<*> (x Data..:? "intentLevelSlotResolutionTestResults")
            Prelude.<*> (x Data..:? "overallTestResults")
            Prelude.<*> (x Data..:? "utteranceLevelTestResults")
      )

instance Prelude.Hashable TestExecutionResultItems where
  hashWithSalt _salt TestExecutionResultItems' {..} =
    _salt
      `Prelude.hashWithSalt` conversationLevelTestResults
      `Prelude.hashWithSalt` intentClassificationTestResults
      `Prelude.hashWithSalt` intentLevelSlotResolutionTestResults
      `Prelude.hashWithSalt` overallTestResults
      `Prelude.hashWithSalt` utteranceLevelTestResults

instance Prelude.NFData TestExecutionResultItems where
  rnf TestExecutionResultItems' {..} =
    Prelude.rnf conversationLevelTestResults
      `Prelude.seq` Prelude.rnf intentClassificationTestResults
      `Prelude.seq` Prelude.rnf intentLevelSlotResolutionTestResults
      `Prelude.seq` Prelude.rnf overallTestResults
      `Prelude.seq` Prelude.rnf utteranceLevelTestResults
