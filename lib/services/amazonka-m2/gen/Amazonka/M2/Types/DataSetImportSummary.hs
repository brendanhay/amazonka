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
-- Module      : Amazonka.M2.Types.DataSetImportSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.DataSetImportSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a summary of data set imports.
--
-- /See:/ 'newDataSetImportSummary' smart constructor.
data DataSetImportSummary = DataSetImportSummary'
  { -- | The number of data set imports that have failed.
    failed :: Prelude.Int,
    -- | The number of data set imports that are in progress.
    inProgress :: Prelude.Int,
    -- | The number of data set imports that are pending.
    pending :: Prelude.Int,
    -- | The number of data set imports that have succeeded.
    succeeded :: Prelude.Int,
    -- | The total number of data set imports.
    total :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetImportSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failed', 'dataSetImportSummary_failed' - The number of data set imports that have failed.
--
-- 'inProgress', 'dataSetImportSummary_inProgress' - The number of data set imports that are in progress.
--
-- 'pending', 'dataSetImportSummary_pending' - The number of data set imports that are pending.
--
-- 'succeeded', 'dataSetImportSummary_succeeded' - The number of data set imports that have succeeded.
--
-- 'total', 'dataSetImportSummary_total' - The total number of data set imports.
newDataSetImportSummary ::
  -- | 'failed'
  Prelude.Int ->
  -- | 'inProgress'
  Prelude.Int ->
  -- | 'pending'
  Prelude.Int ->
  -- | 'succeeded'
  Prelude.Int ->
  -- | 'total'
  Prelude.Int ->
  DataSetImportSummary
newDataSetImportSummary
  pFailed_
  pInProgress_
  pPending_
  pSucceeded_
  pTotal_ =
    DataSetImportSummary'
      { failed = pFailed_,
        inProgress = pInProgress_,
        pending = pPending_,
        succeeded = pSucceeded_,
        total = pTotal_
      }

-- | The number of data set imports that have failed.
dataSetImportSummary_failed :: Lens.Lens' DataSetImportSummary Prelude.Int
dataSetImportSummary_failed = Lens.lens (\DataSetImportSummary' {failed} -> failed) (\s@DataSetImportSummary' {} a -> s {failed = a} :: DataSetImportSummary)

-- | The number of data set imports that are in progress.
dataSetImportSummary_inProgress :: Lens.Lens' DataSetImportSummary Prelude.Int
dataSetImportSummary_inProgress = Lens.lens (\DataSetImportSummary' {inProgress} -> inProgress) (\s@DataSetImportSummary' {} a -> s {inProgress = a} :: DataSetImportSummary)

-- | The number of data set imports that are pending.
dataSetImportSummary_pending :: Lens.Lens' DataSetImportSummary Prelude.Int
dataSetImportSummary_pending = Lens.lens (\DataSetImportSummary' {pending} -> pending) (\s@DataSetImportSummary' {} a -> s {pending = a} :: DataSetImportSummary)

-- | The number of data set imports that have succeeded.
dataSetImportSummary_succeeded :: Lens.Lens' DataSetImportSummary Prelude.Int
dataSetImportSummary_succeeded = Lens.lens (\DataSetImportSummary' {succeeded} -> succeeded) (\s@DataSetImportSummary' {} a -> s {succeeded = a} :: DataSetImportSummary)

-- | The total number of data set imports.
dataSetImportSummary_total :: Lens.Lens' DataSetImportSummary Prelude.Int
dataSetImportSummary_total = Lens.lens (\DataSetImportSummary' {total} -> total) (\s@DataSetImportSummary' {} a -> s {total = a} :: DataSetImportSummary)

instance Data.FromJSON DataSetImportSummary where
  parseJSON =
    Data.withObject
      "DataSetImportSummary"
      ( \x ->
          DataSetImportSummary'
            Prelude.<$> (x Data..: "failed")
            Prelude.<*> (x Data..: "inProgress")
            Prelude.<*> (x Data..: "pending")
            Prelude.<*> (x Data..: "succeeded")
            Prelude.<*> (x Data..: "total")
      )

instance Prelude.Hashable DataSetImportSummary where
  hashWithSalt _salt DataSetImportSummary' {..} =
    _salt `Prelude.hashWithSalt` failed
      `Prelude.hashWithSalt` inProgress
      `Prelude.hashWithSalt` pending
      `Prelude.hashWithSalt` succeeded
      `Prelude.hashWithSalt` total

instance Prelude.NFData DataSetImportSummary where
  rnf DataSetImportSummary' {..} =
    Prelude.rnf failed
      `Prelude.seq` Prelude.rnf inProgress
      `Prelude.seq` Prelude.rnf pending
      `Prelude.seq` Prelude.rnf succeeded
      `Prelude.seq` Prelude.rnf total
