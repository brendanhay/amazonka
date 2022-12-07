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
-- Module      : Amazonka.LookoutVision.Types.DatasetImageStats
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.DatasetImageStats where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Statistics about the images in a dataset.
--
-- /See:/ 'newDatasetImageStats' smart constructor.
data DatasetImageStats = DatasetImageStats'
  { -- | The total number of images in the dataset.
    total :: Prelude.Maybe Prelude.Int,
    -- | The total number of labeled images.
    labeled :: Prelude.Maybe Prelude.Int,
    -- | the total number of images labeled as an anomaly.
    anomaly :: Prelude.Maybe Prelude.Int,
    -- | The total number of images labeled as normal.
    normal :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetImageStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'total', 'datasetImageStats_total' - The total number of images in the dataset.
--
-- 'labeled', 'datasetImageStats_labeled' - The total number of labeled images.
--
-- 'anomaly', 'datasetImageStats_anomaly' - the total number of images labeled as an anomaly.
--
-- 'normal', 'datasetImageStats_normal' - The total number of images labeled as normal.
newDatasetImageStats ::
  DatasetImageStats
newDatasetImageStats =
  DatasetImageStats'
    { total = Prelude.Nothing,
      labeled = Prelude.Nothing,
      anomaly = Prelude.Nothing,
      normal = Prelude.Nothing
    }

-- | The total number of images in the dataset.
datasetImageStats_total :: Lens.Lens' DatasetImageStats (Prelude.Maybe Prelude.Int)
datasetImageStats_total = Lens.lens (\DatasetImageStats' {total} -> total) (\s@DatasetImageStats' {} a -> s {total = a} :: DatasetImageStats)

-- | The total number of labeled images.
datasetImageStats_labeled :: Lens.Lens' DatasetImageStats (Prelude.Maybe Prelude.Int)
datasetImageStats_labeled = Lens.lens (\DatasetImageStats' {labeled} -> labeled) (\s@DatasetImageStats' {} a -> s {labeled = a} :: DatasetImageStats)

-- | the total number of images labeled as an anomaly.
datasetImageStats_anomaly :: Lens.Lens' DatasetImageStats (Prelude.Maybe Prelude.Int)
datasetImageStats_anomaly = Lens.lens (\DatasetImageStats' {anomaly} -> anomaly) (\s@DatasetImageStats' {} a -> s {anomaly = a} :: DatasetImageStats)

-- | The total number of images labeled as normal.
datasetImageStats_normal :: Lens.Lens' DatasetImageStats (Prelude.Maybe Prelude.Int)
datasetImageStats_normal = Lens.lens (\DatasetImageStats' {normal} -> normal) (\s@DatasetImageStats' {} a -> s {normal = a} :: DatasetImageStats)

instance Data.FromJSON DatasetImageStats where
  parseJSON =
    Data.withObject
      "DatasetImageStats"
      ( \x ->
          DatasetImageStats'
            Prelude.<$> (x Data..:? "Total")
            Prelude.<*> (x Data..:? "Labeled")
            Prelude.<*> (x Data..:? "Anomaly")
            Prelude.<*> (x Data..:? "Normal")
      )

instance Prelude.Hashable DatasetImageStats where
  hashWithSalt _salt DatasetImageStats' {..} =
    _salt `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` labeled
      `Prelude.hashWithSalt` anomaly
      `Prelude.hashWithSalt` normal

instance Prelude.NFData DatasetImageStats where
  rnf DatasetImageStats' {..} =
    Prelude.rnf total
      `Prelude.seq` Prelude.rnf labeled
      `Prelude.seq` Prelude.rnf anomaly
      `Prelude.seq` Prelude.rnf normal
