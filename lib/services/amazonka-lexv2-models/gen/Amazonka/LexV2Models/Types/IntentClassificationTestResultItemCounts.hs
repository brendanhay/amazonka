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
-- Module      : Amazonka.LexV2Models.Types.IntentClassificationTestResultItemCounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.IntentClassificationTestResultItemCounts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TestResultMatchStatus
import qualified Amazonka.Prelude as Prelude

-- | The number of items in the intent classification test.
--
-- /See:/ 'newIntentClassificationTestResultItemCounts' smart constructor.
data IntentClassificationTestResultItemCounts = IntentClassificationTestResultItemCounts'
  { -- | The number of matched, mismatched, and execution error results for
    -- speech transcription for the intent.
    speechTranscriptionResultCounts :: Prelude.Maybe (Prelude.HashMap TestResultMatchStatus Prelude.Int),
    -- | The total number of results in the intent classification test.
    totalResultCount :: Prelude.Int,
    -- | The number of matched and mismatched results for intent recognition for
    -- the intent.
    intentMatchResultCounts :: Prelude.HashMap TestResultMatchStatus Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentClassificationTestResultItemCounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'speechTranscriptionResultCounts', 'intentClassificationTestResultItemCounts_speechTranscriptionResultCounts' - The number of matched, mismatched, and execution error results for
-- speech transcription for the intent.
--
-- 'totalResultCount', 'intentClassificationTestResultItemCounts_totalResultCount' - The total number of results in the intent classification test.
--
-- 'intentMatchResultCounts', 'intentClassificationTestResultItemCounts_intentMatchResultCounts' - The number of matched and mismatched results for intent recognition for
-- the intent.
newIntentClassificationTestResultItemCounts ::
  -- | 'totalResultCount'
  Prelude.Int ->
  IntentClassificationTestResultItemCounts
newIntentClassificationTestResultItemCounts
  pTotalResultCount_ =
    IntentClassificationTestResultItemCounts'
      { speechTranscriptionResultCounts =
          Prelude.Nothing,
        totalResultCount =
          pTotalResultCount_,
        intentMatchResultCounts =
          Prelude.mempty
      }

-- | The number of matched, mismatched, and execution error results for
-- speech transcription for the intent.
intentClassificationTestResultItemCounts_speechTranscriptionResultCounts :: Lens.Lens' IntentClassificationTestResultItemCounts (Prelude.Maybe (Prelude.HashMap TestResultMatchStatus Prelude.Int))
intentClassificationTestResultItemCounts_speechTranscriptionResultCounts = Lens.lens (\IntentClassificationTestResultItemCounts' {speechTranscriptionResultCounts} -> speechTranscriptionResultCounts) (\s@IntentClassificationTestResultItemCounts' {} a -> s {speechTranscriptionResultCounts = a} :: IntentClassificationTestResultItemCounts) Prelude.. Lens.mapping Lens.coerced

-- | The total number of results in the intent classification test.
intentClassificationTestResultItemCounts_totalResultCount :: Lens.Lens' IntentClassificationTestResultItemCounts Prelude.Int
intentClassificationTestResultItemCounts_totalResultCount = Lens.lens (\IntentClassificationTestResultItemCounts' {totalResultCount} -> totalResultCount) (\s@IntentClassificationTestResultItemCounts' {} a -> s {totalResultCount = a} :: IntentClassificationTestResultItemCounts)

-- | The number of matched and mismatched results for intent recognition for
-- the intent.
intentClassificationTestResultItemCounts_intentMatchResultCounts :: Lens.Lens' IntentClassificationTestResultItemCounts (Prelude.HashMap TestResultMatchStatus Prelude.Int)
intentClassificationTestResultItemCounts_intentMatchResultCounts = Lens.lens (\IntentClassificationTestResultItemCounts' {intentMatchResultCounts} -> intentMatchResultCounts) (\s@IntentClassificationTestResultItemCounts' {} a -> s {intentMatchResultCounts = a} :: IntentClassificationTestResultItemCounts) Prelude.. Lens.coerced

instance
  Data.FromJSON
    IntentClassificationTestResultItemCounts
  where
  parseJSON =
    Data.withObject
      "IntentClassificationTestResultItemCounts"
      ( \x ->
          IntentClassificationTestResultItemCounts'
            Prelude.<$> ( x
                            Data..:? "speechTranscriptionResultCounts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "totalResultCount")
            Prelude.<*> ( x
                            Data..:? "intentMatchResultCounts"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    IntentClassificationTestResultItemCounts
  where
  hashWithSalt
    _salt
    IntentClassificationTestResultItemCounts' {..} =
      _salt
        `Prelude.hashWithSalt` speechTranscriptionResultCounts
        `Prelude.hashWithSalt` totalResultCount
        `Prelude.hashWithSalt` intentMatchResultCounts

instance
  Prelude.NFData
    IntentClassificationTestResultItemCounts
  where
  rnf IntentClassificationTestResultItemCounts' {..} =
    Prelude.rnf speechTranscriptionResultCounts
      `Prelude.seq` Prelude.rnf totalResultCount
      `Prelude.seq` Prelude.rnf intentMatchResultCounts
