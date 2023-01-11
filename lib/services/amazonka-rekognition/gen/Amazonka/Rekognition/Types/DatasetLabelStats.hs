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
-- Module      : Amazonka.Rekognition.Types.DatasetLabelStats
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DatasetLabelStats where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Statistics about a label used in a dataset. For more information, see
-- DatasetLabelDescription.
--
-- /See:/ 'newDatasetLabelStats' smart constructor.
data DatasetLabelStats = DatasetLabelStats'
  { -- | The total number of images that have the label assigned to a bounding
    -- box.
    boundingBoxCount :: Prelude.Maybe Prelude.Natural,
    -- | The total number of images that use the label.
    entryCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetLabelStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boundingBoxCount', 'datasetLabelStats_boundingBoxCount' - The total number of images that have the label assigned to a bounding
-- box.
--
-- 'entryCount', 'datasetLabelStats_entryCount' - The total number of images that use the label.
newDatasetLabelStats ::
  DatasetLabelStats
newDatasetLabelStats =
  DatasetLabelStats'
    { boundingBoxCount =
        Prelude.Nothing,
      entryCount = Prelude.Nothing
    }

-- | The total number of images that have the label assigned to a bounding
-- box.
datasetLabelStats_boundingBoxCount :: Lens.Lens' DatasetLabelStats (Prelude.Maybe Prelude.Natural)
datasetLabelStats_boundingBoxCount = Lens.lens (\DatasetLabelStats' {boundingBoxCount} -> boundingBoxCount) (\s@DatasetLabelStats' {} a -> s {boundingBoxCount = a} :: DatasetLabelStats)

-- | The total number of images that use the label.
datasetLabelStats_entryCount :: Lens.Lens' DatasetLabelStats (Prelude.Maybe Prelude.Natural)
datasetLabelStats_entryCount = Lens.lens (\DatasetLabelStats' {entryCount} -> entryCount) (\s@DatasetLabelStats' {} a -> s {entryCount = a} :: DatasetLabelStats)

instance Data.FromJSON DatasetLabelStats where
  parseJSON =
    Data.withObject
      "DatasetLabelStats"
      ( \x ->
          DatasetLabelStats'
            Prelude.<$> (x Data..:? "BoundingBoxCount")
            Prelude.<*> (x Data..:? "EntryCount")
      )

instance Prelude.Hashable DatasetLabelStats where
  hashWithSalt _salt DatasetLabelStats' {..} =
    _salt `Prelude.hashWithSalt` boundingBoxCount
      `Prelude.hashWithSalt` entryCount

instance Prelude.NFData DatasetLabelStats where
  rnf DatasetLabelStats' {..} =
    Prelude.rnf boundingBoxCount
      `Prelude.seq` Prelude.rnf entryCount
