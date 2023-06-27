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
-- Module      : Amazonka.LexV2Models.Types.OverallTestResultItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.OverallTestResultItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TestResultMatchStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the overall results for a test execution result.
--
-- /See:/ 'newOverallTestResultItem' smart constructor.
data OverallTestResultItem = OverallTestResultItem'
  { -- | The number of speech transcription results in the overall test.
    speechTranscriptionResultCounts :: Prelude.Maybe (Prelude.HashMap TestResultMatchStatus Prelude.Int),
    -- | Indicates whether the conversation contains multiple turns or not.
    multiTurnConversation :: Prelude.Bool,
    -- | The total number of overall results in the result of the test execution.
    totalResultCount :: Prelude.Int,
    -- | The number of results that succeeded.
    endToEndResultCounts :: Prelude.HashMap TestResultMatchStatus Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OverallTestResultItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'speechTranscriptionResultCounts', 'overallTestResultItem_speechTranscriptionResultCounts' - The number of speech transcription results in the overall test.
--
-- 'multiTurnConversation', 'overallTestResultItem_multiTurnConversation' - Indicates whether the conversation contains multiple turns or not.
--
-- 'totalResultCount', 'overallTestResultItem_totalResultCount' - The total number of overall results in the result of the test execution.
--
-- 'endToEndResultCounts', 'overallTestResultItem_endToEndResultCounts' - The number of results that succeeded.
newOverallTestResultItem ::
  -- | 'multiTurnConversation'
  Prelude.Bool ->
  -- | 'totalResultCount'
  Prelude.Int ->
  OverallTestResultItem
newOverallTestResultItem
  pMultiTurnConversation_
  pTotalResultCount_ =
    OverallTestResultItem'
      { speechTranscriptionResultCounts =
          Prelude.Nothing,
        multiTurnConversation = pMultiTurnConversation_,
        totalResultCount = pTotalResultCount_,
        endToEndResultCounts = Prelude.mempty
      }

-- | The number of speech transcription results in the overall test.
overallTestResultItem_speechTranscriptionResultCounts :: Lens.Lens' OverallTestResultItem (Prelude.Maybe (Prelude.HashMap TestResultMatchStatus Prelude.Int))
overallTestResultItem_speechTranscriptionResultCounts = Lens.lens (\OverallTestResultItem' {speechTranscriptionResultCounts} -> speechTranscriptionResultCounts) (\s@OverallTestResultItem' {} a -> s {speechTranscriptionResultCounts = a} :: OverallTestResultItem) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the conversation contains multiple turns or not.
overallTestResultItem_multiTurnConversation :: Lens.Lens' OverallTestResultItem Prelude.Bool
overallTestResultItem_multiTurnConversation = Lens.lens (\OverallTestResultItem' {multiTurnConversation} -> multiTurnConversation) (\s@OverallTestResultItem' {} a -> s {multiTurnConversation = a} :: OverallTestResultItem)

-- | The total number of overall results in the result of the test execution.
overallTestResultItem_totalResultCount :: Lens.Lens' OverallTestResultItem Prelude.Int
overallTestResultItem_totalResultCount = Lens.lens (\OverallTestResultItem' {totalResultCount} -> totalResultCount) (\s@OverallTestResultItem' {} a -> s {totalResultCount = a} :: OverallTestResultItem)

-- | The number of results that succeeded.
overallTestResultItem_endToEndResultCounts :: Lens.Lens' OverallTestResultItem (Prelude.HashMap TestResultMatchStatus Prelude.Int)
overallTestResultItem_endToEndResultCounts = Lens.lens (\OverallTestResultItem' {endToEndResultCounts} -> endToEndResultCounts) (\s@OverallTestResultItem' {} a -> s {endToEndResultCounts = a} :: OverallTestResultItem) Prelude.. Lens.coerced

instance Data.FromJSON OverallTestResultItem where
  parseJSON =
    Data.withObject
      "OverallTestResultItem"
      ( \x ->
          OverallTestResultItem'
            Prelude.<$> ( x
                            Data..:? "speechTranscriptionResultCounts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "multiTurnConversation")
            Prelude.<*> (x Data..: "totalResultCount")
            Prelude.<*> ( x
                            Data..:? "endToEndResultCounts"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable OverallTestResultItem where
  hashWithSalt _salt OverallTestResultItem' {..} =
    _salt
      `Prelude.hashWithSalt` speechTranscriptionResultCounts
      `Prelude.hashWithSalt` multiTurnConversation
      `Prelude.hashWithSalt` totalResultCount
      `Prelude.hashWithSalt` endToEndResultCounts

instance Prelude.NFData OverallTestResultItem where
  rnf OverallTestResultItem' {..} =
    Prelude.rnf speechTranscriptionResultCounts
      `Prelude.seq` Prelude.rnf multiTurnConversation
      `Prelude.seq` Prelude.rnf totalResultCount
      `Prelude.seq` Prelude.rnf endToEndResultCounts
