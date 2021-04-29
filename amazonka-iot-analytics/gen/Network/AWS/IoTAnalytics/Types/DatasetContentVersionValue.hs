{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The dataset whose latest contents are used as input to the notebook or
-- application.
--
-- /See:/ 'newDatasetContentVersionValue' smart constructor.
data DatasetContentVersionValue = DatasetContentVersionValue'
  { -- | The name of the dataset whose latest contents are used as input to the
    -- notebook or application.
    datasetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DatasetContentVersionValue
newDatasetContentVersionValue pDatasetName_ =
  DatasetContentVersionValue'
    { datasetName =
        pDatasetName_
    }

-- | The name of the dataset whose latest contents are used as input to the
-- notebook or application.
datasetContentVersionValue_datasetName :: Lens.Lens' DatasetContentVersionValue Prelude.Text
datasetContentVersionValue_datasetName = Lens.lens (\DatasetContentVersionValue' {datasetName} -> datasetName) (\s@DatasetContentVersionValue' {} a -> s {datasetName = a} :: DatasetContentVersionValue)

instance Prelude.FromJSON DatasetContentVersionValue where
  parseJSON =
    Prelude.withObject
      "DatasetContentVersionValue"
      ( \x ->
          DatasetContentVersionValue'
            Prelude.<$> (x Prelude..: "datasetName")
      )

instance Prelude.Hashable DatasetContentVersionValue

instance Prelude.NFData DatasetContentVersionValue

instance Prelude.ToJSON DatasetContentVersionValue where
  toJSON DatasetContentVersionValue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("datasetName" Prelude..= datasetName)
          ]
      )
