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
-- Module      : Amazonka.Rekognition.Types.DatasetLabelDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DatasetLabelDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.DatasetLabelStats

-- | Describes a dataset label. For more information, see ListDatasetLabels.
--
-- /See:/ 'newDatasetLabelDescription' smart constructor.
data DatasetLabelDescription = DatasetLabelDescription'
  { -- | The name of the label.
    labelName :: Prelude.Maybe Prelude.Text,
    -- | Statistics about the label.
    labelStats :: Prelude.Maybe DatasetLabelStats
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetLabelDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelName', 'datasetLabelDescription_labelName' - The name of the label.
--
-- 'labelStats', 'datasetLabelDescription_labelStats' - Statistics about the label.
newDatasetLabelDescription ::
  DatasetLabelDescription
newDatasetLabelDescription =
  DatasetLabelDescription'
    { labelName =
        Prelude.Nothing,
      labelStats = Prelude.Nothing
    }

-- | The name of the label.
datasetLabelDescription_labelName :: Lens.Lens' DatasetLabelDescription (Prelude.Maybe Prelude.Text)
datasetLabelDescription_labelName = Lens.lens (\DatasetLabelDescription' {labelName} -> labelName) (\s@DatasetLabelDescription' {} a -> s {labelName = a} :: DatasetLabelDescription)

-- | Statistics about the label.
datasetLabelDescription_labelStats :: Lens.Lens' DatasetLabelDescription (Prelude.Maybe DatasetLabelStats)
datasetLabelDescription_labelStats = Lens.lens (\DatasetLabelDescription' {labelStats} -> labelStats) (\s@DatasetLabelDescription' {} a -> s {labelStats = a} :: DatasetLabelDescription)

instance Data.FromJSON DatasetLabelDescription where
  parseJSON =
    Data.withObject
      "DatasetLabelDescription"
      ( \x ->
          DatasetLabelDescription'
            Prelude.<$> (x Data..:? "LabelName")
            Prelude.<*> (x Data..:? "LabelStats")
      )

instance Prelude.Hashable DatasetLabelDescription where
  hashWithSalt _salt DatasetLabelDescription' {..} =
    _salt
      `Prelude.hashWithSalt` labelName
      `Prelude.hashWithSalt` labelStats

instance Prelude.NFData DatasetLabelDescription where
  rnf DatasetLabelDescription' {..} =
    Prelude.rnf labelName `Prelude.seq`
      Prelude.rnf labelStats
