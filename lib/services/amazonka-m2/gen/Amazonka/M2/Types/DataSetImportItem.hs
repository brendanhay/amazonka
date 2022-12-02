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
-- Module      : Amazonka.M2.Types.DataSetImportItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.DataSetImportItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types.DataSet
import Amazonka.M2.Types.ExternalLocation
import qualified Amazonka.Prelude as Prelude

-- | Identifies a specific data set to import from an external location.
--
-- /See:/ 'newDataSetImportItem' smart constructor.
data DataSetImportItem = DataSetImportItem'
  { -- | The data set.
    dataSet :: DataSet,
    -- | The location of the data set.
    externalLocation :: ExternalLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetImportItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSet', 'dataSetImportItem_dataSet' - The data set.
--
-- 'externalLocation', 'dataSetImportItem_externalLocation' - The location of the data set.
newDataSetImportItem ::
  -- | 'dataSet'
  DataSet ->
  -- | 'externalLocation'
  ExternalLocation ->
  DataSetImportItem
newDataSetImportItem pDataSet_ pExternalLocation_ =
  DataSetImportItem'
    { dataSet = pDataSet_,
      externalLocation = pExternalLocation_
    }

-- | The data set.
dataSetImportItem_dataSet :: Lens.Lens' DataSetImportItem DataSet
dataSetImportItem_dataSet = Lens.lens (\DataSetImportItem' {dataSet} -> dataSet) (\s@DataSetImportItem' {} a -> s {dataSet = a} :: DataSetImportItem)

-- | The location of the data set.
dataSetImportItem_externalLocation :: Lens.Lens' DataSetImportItem ExternalLocation
dataSetImportItem_externalLocation = Lens.lens (\DataSetImportItem' {externalLocation} -> externalLocation) (\s@DataSetImportItem' {} a -> s {externalLocation = a} :: DataSetImportItem)

instance Prelude.Hashable DataSetImportItem where
  hashWithSalt _salt DataSetImportItem' {..} =
    _salt `Prelude.hashWithSalt` dataSet
      `Prelude.hashWithSalt` externalLocation

instance Prelude.NFData DataSetImportItem where
  rnf DataSetImportItem' {..} =
    Prelude.rnf dataSet
      `Prelude.seq` Prelude.rnf externalLocation

instance Data.ToJSON DataSetImportItem where
  toJSON DataSetImportItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("dataSet" Data..= dataSet),
            Prelude.Just
              ("externalLocation" Data..= externalLocation)
          ]
      )
