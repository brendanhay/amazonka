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
-- Module      : Amazonka.Rekognition.Types.DatasetStats
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DatasetStats where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides statistics about a dataset. For more information, see
-- DescribeDataset.
--
-- /See:/ 'newDatasetStats' smart constructor.
data DatasetStats = DatasetStats'
  { -- | The total number of entries that contain at least one error.
    errorEntries :: Prelude.Maybe Prelude.Natural,
    -- | The total number of images in the dataset that have labels.
    labeledEntries :: Prelude.Maybe Prelude.Natural,
    -- | The total number of images in the dataset.
    totalEntries :: Prelude.Maybe Prelude.Natural,
    -- | The total number of labels declared in the dataset.
    totalLabels :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorEntries', 'datasetStats_errorEntries' - The total number of entries that contain at least one error.
--
-- 'labeledEntries', 'datasetStats_labeledEntries' - The total number of images in the dataset that have labels.
--
-- 'totalEntries', 'datasetStats_totalEntries' - The total number of images in the dataset.
--
-- 'totalLabels', 'datasetStats_totalLabels' - The total number of labels declared in the dataset.
newDatasetStats ::
  DatasetStats
newDatasetStats =
  DatasetStats'
    { errorEntries = Prelude.Nothing,
      labeledEntries = Prelude.Nothing,
      totalEntries = Prelude.Nothing,
      totalLabels = Prelude.Nothing
    }

-- | The total number of entries that contain at least one error.
datasetStats_errorEntries :: Lens.Lens' DatasetStats (Prelude.Maybe Prelude.Natural)
datasetStats_errorEntries = Lens.lens (\DatasetStats' {errorEntries} -> errorEntries) (\s@DatasetStats' {} a -> s {errorEntries = a} :: DatasetStats)

-- | The total number of images in the dataset that have labels.
datasetStats_labeledEntries :: Lens.Lens' DatasetStats (Prelude.Maybe Prelude.Natural)
datasetStats_labeledEntries = Lens.lens (\DatasetStats' {labeledEntries} -> labeledEntries) (\s@DatasetStats' {} a -> s {labeledEntries = a} :: DatasetStats)

-- | The total number of images in the dataset.
datasetStats_totalEntries :: Lens.Lens' DatasetStats (Prelude.Maybe Prelude.Natural)
datasetStats_totalEntries = Lens.lens (\DatasetStats' {totalEntries} -> totalEntries) (\s@DatasetStats' {} a -> s {totalEntries = a} :: DatasetStats)

-- | The total number of labels declared in the dataset.
datasetStats_totalLabels :: Lens.Lens' DatasetStats (Prelude.Maybe Prelude.Natural)
datasetStats_totalLabels = Lens.lens (\DatasetStats' {totalLabels} -> totalLabels) (\s@DatasetStats' {} a -> s {totalLabels = a} :: DatasetStats)

instance Data.FromJSON DatasetStats where
  parseJSON =
    Data.withObject
      "DatasetStats"
      ( \x ->
          DatasetStats'
            Prelude.<$> (x Data..:? "ErrorEntries")
            Prelude.<*> (x Data..:? "LabeledEntries")
            Prelude.<*> (x Data..:? "TotalEntries")
            Prelude.<*> (x Data..:? "TotalLabels")
      )

instance Prelude.Hashable DatasetStats where
  hashWithSalt _salt DatasetStats' {..} =
    _salt `Prelude.hashWithSalt` errorEntries
      `Prelude.hashWithSalt` labeledEntries
      `Prelude.hashWithSalt` totalEntries
      `Prelude.hashWithSalt` totalLabels

instance Prelude.NFData DatasetStats where
  rnf DatasetStats' {..} =
    Prelude.rnf errorEntries
      `Prelude.seq` Prelude.rnf labeledEntries
      `Prelude.seq` Prelude.rnf totalEntries
      `Prelude.seq` Prelude.rnf totalLabels
