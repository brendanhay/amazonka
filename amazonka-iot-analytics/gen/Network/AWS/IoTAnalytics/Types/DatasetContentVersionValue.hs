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
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The dataset whose latest contents are used as input to the notebook or
-- application.
--
-- /See:/ 'newDatasetContentVersionValue' smart constructor.
data DatasetContentVersionValue = DatasetContentVersionValue'
  { -- | The name of the dataset whose latest contents are used as input to the
    -- notebook or application.
    datasetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DatasetContentVersionValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetName', 'datasetContentVersionValue_datasetName' - The name of the dataset whose latest contents are used as input to the
-- notebook or application.
newDatasetContentVersionValue ::
  -- | 'datasetName'
  Core.Text ->
  DatasetContentVersionValue
newDatasetContentVersionValue pDatasetName_ =
  DatasetContentVersionValue'
    { datasetName =
        pDatasetName_
    }

-- | The name of the dataset whose latest contents are used as input to the
-- notebook or application.
datasetContentVersionValue_datasetName :: Lens.Lens' DatasetContentVersionValue Core.Text
datasetContentVersionValue_datasetName = Lens.lens (\DatasetContentVersionValue' {datasetName} -> datasetName) (\s@DatasetContentVersionValue' {} a -> s {datasetName = a} :: DatasetContentVersionValue)

instance Core.FromJSON DatasetContentVersionValue where
  parseJSON =
    Core.withObject
      "DatasetContentVersionValue"
      ( \x ->
          DatasetContentVersionValue'
            Core.<$> (x Core..: "datasetName")
      )

instance Core.Hashable DatasetContentVersionValue

instance Core.NFData DatasetContentVersionValue

instance Core.ToJSON DatasetContentVersionValue where
  toJSON DatasetContentVersionValue' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("datasetName" Core..= datasetName)]
      )
