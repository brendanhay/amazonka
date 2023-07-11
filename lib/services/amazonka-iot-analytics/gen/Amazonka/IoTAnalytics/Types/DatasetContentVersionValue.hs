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
-- Module      : Amazonka.IoTAnalytics.Types.DatasetContentVersionValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatasetContentVersionValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The dataset whose latest contents are used as input to the notebook or
-- application.
--
-- /See:/ 'newDatasetContentVersionValue' smart constructor.
data DatasetContentVersionValue = DatasetContentVersionValue'
  { -- | The name of the dataset whose latest contents are used as input to the
    -- notebook or application.
    datasetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON DatasetContentVersionValue where
  parseJSON =
    Data.withObject
      "DatasetContentVersionValue"
      ( \x ->
          DatasetContentVersionValue'
            Prelude.<$> (x Data..: "datasetName")
      )

instance Prelude.Hashable DatasetContentVersionValue where
  hashWithSalt _salt DatasetContentVersionValue' {..} =
    _salt `Prelude.hashWithSalt` datasetName

instance Prelude.NFData DatasetContentVersionValue where
  rnf DatasetContentVersionValue' {..} =
    Prelude.rnf datasetName

instance Data.ToJSON DatasetContentVersionValue where
  toJSON DatasetContentVersionValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("datasetName" Data..= datasetName)]
      )
