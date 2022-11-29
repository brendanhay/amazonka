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
-- Module      : Amazonka.CustomerProfiles.Types.Batch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.Batch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Batch defines the boundaries for ingestion for each step in
-- @APPFLOW_INTEGRATION@ workflow. @APPFLOW_INTEGRATION@ workflow splits
-- ingestion based on these boundaries.
--
-- /See:/ 'newBatch' smart constructor.
data Batch = Batch'
  { -- | Start time of batch to split ingestion.
    startTime :: Core.POSIX,
    -- | End time of batch to split ingestion.
    endTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Batch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'batch_startTime' - Start time of batch to split ingestion.
--
-- 'endTime', 'batch_endTime' - End time of batch to split ingestion.
newBatch ::
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  Batch
newBatch pStartTime_ pEndTime_ =
  Batch'
    { startTime = Core._Time Lens.# pStartTime_,
      endTime = Core._Time Lens.# pEndTime_
    }

-- | Start time of batch to split ingestion.
batch_startTime :: Lens.Lens' Batch Prelude.UTCTime
batch_startTime = Lens.lens (\Batch' {startTime} -> startTime) (\s@Batch' {} a -> s {startTime = a} :: Batch) Prelude.. Core._Time

-- | End time of batch to split ingestion.
batch_endTime :: Lens.Lens' Batch Prelude.UTCTime
batch_endTime = Lens.lens (\Batch' {endTime} -> endTime) (\s@Batch' {} a -> s {endTime = a} :: Batch) Prelude.. Core._Time

instance Prelude.Hashable Batch where
  hashWithSalt _salt Batch' {..} =
    _salt `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData Batch where
  rnf Batch' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Core.ToJSON Batch where
  toJSON Batch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StartTime" Core..= startTime),
            Prelude.Just ("EndTime" Core..= endTime)
          ]
      )
