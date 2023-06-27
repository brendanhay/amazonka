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
-- Module      : Amazonka.LexV2Models.Types.SlotResolutionTestResultItemCounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotResolutionTestResultItemCounts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TestResultMatchStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the counts for a slot resolution in the results of a
-- test execution.
--
-- /See:/ 'newSlotResolutionTestResultItemCounts' smart constructor.
data SlotResolutionTestResultItemCounts = SlotResolutionTestResultItemCounts'
  { -- | The number of matched, mismatched and execution error results for speech
    -- transcription for the slot.
    speechTranscriptionResultCounts :: Prelude.Maybe (Prelude.HashMap TestResultMatchStatus Prelude.Int),
    -- | The total number of results.
    totalResultCount :: Prelude.Int,
    -- | The number of matched and mismatched results for slot resolution for the
    -- slot.
    slotMatchResultCounts :: Prelude.HashMap TestResultMatchStatus Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotResolutionTestResultItemCounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'speechTranscriptionResultCounts', 'slotResolutionTestResultItemCounts_speechTranscriptionResultCounts' - The number of matched, mismatched and execution error results for speech
-- transcription for the slot.
--
-- 'totalResultCount', 'slotResolutionTestResultItemCounts_totalResultCount' - The total number of results.
--
-- 'slotMatchResultCounts', 'slotResolutionTestResultItemCounts_slotMatchResultCounts' - The number of matched and mismatched results for slot resolution for the
-- slot.
newSlotResolutionTestResultItemCounts ::
  -- | 'totalResultCount'
  Prelude.Int ->
  SlotResolutionTestResultItemCounts
newSlotResolutionTestResultItemCounts
  pTotalResultCount_ =
    SlotResolutionTestResultItemCounts'
      { speechTranscriptionResultCounts =
          Prelude.Nothing,
        totalResultCount = pTotalResultCount_,
        slotMatchResultCounts = Prelude.mempty
      }

-- | The number of matched, mismatched and execution error results for speech
-- transcription for the slot.
slotResolutionTestResultItemCounts_speechTranscriptionResultCounts :: Lens.Lens' SlotResolutionTestResultItemCounts (Prelude.Maybe (Prelude.HashMap TestResultMatchStatus Prelude.Int))
slotResolutionTestResultItemCounts_speechTranscriptionResultCounts = Lens.lens (\SlotResolutionTestResultItemCounts' {speechTranscriptionResultCounts} -> speechTranscriptionResultCounts) (\s@SlotResolutionTestResultItemCounts' {} a -> s {speechTranscriptionResultCounts = a} :: SlotResolutionTestResultItemCounts) Prelude.. Lens.mapping Lens.coerced

-- | The total number of results.
slotResolutionTestResultItemCounts_totalResultCount :: Lens.Lens' SlotResolutionTestResultItemCounts Prelude.Int
slotResolutionTestResultItemCounts_totalResultCount = Lens.lens (\SlotResolutionTestResultItemCounts' {totalResultCount} -> totalResultCount) (\s@SlotResolutionTestResultItemCounts' {} a -> s {totalResultCount = a} :: SlotResolutionTestResultItemCounts)

-- | The number of matched and mismatched results for slot resolution for the
-- slot.
slotResolutionTestResultItemCounts_slotMatchResultCounts :: Lens.Lens' SlotResolutionTestResultItemCounts (Prelude.HashMap TestResultMatchStatus Prelude.Int)
slotResolutionTestResultItemCounts_slotMatchResultCounts = Lens.lens (\SlotResolutionTestResultItemCounts' {slotMatchResultCounts} -> slotMatchResultCounts) (\s@SlotResolutionTestResultItemCounts' {} a -> s {slotMatchResultCounts = a} :: SlotResolutionTestResultItemCounts) Prelude.. Lens.coerced

instance
  Data.FromJSON
    SlotResolutionTestResultItemCounts
  where
  parseJSON =
    Data.withObject
      "SlotResolutionTestResultItemCounts"
      ( \x ->
          SlotResolutionTestResultItemCounts'
            Prelude.<$> ( x
                            Data..:? "speechTranscriptionResultCounts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "totalResultCount")
            Prelude.<*> ( x
                            Data..:? "slotMatchResultCounts"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    SlotResolutionTestResultItemCounts
  where
  hashWithSalt
    _salt
    SlotResolutionTestResultItemCounts' {..} =
      _salt
        `Prelude.hashWithSalt` speechTranscriptionResultCounts
        `Prelude.hashWithSalt` totalResultCount
        `Prelude.hashWithSalt` slotMatchResultCounts

instance
  Prelude.NFData
    SlotResolutionTestResultItemCounts
  where
  rnf SlotResolutionTestResultItemCounts' {..} =
    Prelude.rnf speechTranscriptionResultCounts
      `Prelude.seq` Prelude.rnf totalResultCount
      `Prelude.seq` Prelude.rnf slotMatchResultCounts
