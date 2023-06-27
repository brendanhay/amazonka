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
-- Module      : Amazonka.Comprehend.Types.DatasetFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DatasetFilter where

import Amazonka.Comprehend.Types.DatasetStatus
import Amazonka.Comprehend.Types.DatasetType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filter the datasets based on creation time or dataset status.
--
-- /See:/ 'newDatasetFilter' smart constructor.
data DatasetFilter = DatasetFilter'
  { -- | Filter the datasets to include datasets created after the specified
    -- time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Filter the datasets to include datasets created before the specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Filter the datasets based on the dataset type.
    datasetType :: Prelude.Maybe DatasetType,
    -- | Filter the datasets based on the dataset status.
    status :: Prelude.Maybe DatasetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'datasetFilter_creationTimeAfter' - Filter the datasets to include datasets created after the specified
-- time.
--
-- 'creationTimeBefore', 'datasetFilter_creationTimeBefore' - Filter the datasets to include datasets created before the specified
-- time.
--
-- 'datasetType', 'datasetFilter_datasetType' - Filter the datasets based on the dataset type.
--
-- 'status', 'datasetFilter_status' - Filter the datasets based on the dataset status.
newDatasetFilter ::
  DatasetFilter
newDatasetFilter =
  DatasetFilter'
    { creationTimeAfter = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      datasetType = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Filter the datasets to include datasets created after the specified
-- time.
datasetFilter_creationTimeAfter :: Lens.Lens' DatasetFilter (Prelude.Maybe Prelude.UTCTime)
datasetFilter_creationTimeAfter = Lens.lens (\DatasetFilter' {creationTimeAfter} -> creationTimeAfter) (\s@DatasetFilter' {} a -> s {creationTimeAfter = a} :: DatasetFilter) Prelude.. Lens.mapping Data._Time

-- | Filter the datasets to include datasets created before the specified
-- time.
datasetFilter_creationTimeBefore :: Lens.Lens' DatasetFilter (Prelude.Maybe Prelude.UTCTime)
datasetFilter_creationTimeBefore = Lens.lens (\DatasetFilter' {creationTimeBefore} -> creationTimeBefore) (\s@DatasetFilter' {} a -> s {creationTimeBefore = a} :: DatasetFilter) Prelude.. Lens.mapping Data._Time

-- | Filter the datasets based on the dataset type.
datasetFilter_datasetType :: Lens.Lens' DatasetFilter (Prelude.Maybe DatasetType)
datasetFilter_datasetType = Lens.lens (\DatasetFilter' {datasetType} -> datasetType) (\s@DatasetFilter' {} a -> s {datasetType = a} :: DatasetFilter)

-- | Filter the datasets based on the dataset status.
datasetFilter_status :: Lens.Lens' DatasetFilter (Prelude.Maybe DatasetStatus)
datasetFilter_status = Lens.lens (\DatasetFilter' {status} -> status) (\s@DatasetFilter' {} a -> s {status = a} :: DatasetFilter)

instance Prelude.Hashable DatasetFilter where
  hashWithSalt _salt DatasetFilter' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` datasetType
      `Prelude.hashWithSalt` status

instance Prelude.NFData DatasetFilter where
  rnf DatasetFilter' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf datasetType
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON DatasetFilter where
  toJSON DatasetFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("DatasetType" Data..=) Prelude.<$> datasetType,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
