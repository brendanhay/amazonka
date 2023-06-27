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
-- Module      : Amazonka.QuickSight.Types.MappedDataSetParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.MappedDataSetParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A dataset parameter that is mapped to an analysis parameter.
--
-- /See:/ 'newMappedDataSetParameter' smart constructor.
data MappedDataSetParameter = MappedDataSetParameter'
  { -- | A unique name that identifies a dataset within the analysis or
    -- dashboard.
    dataSetIdentifier :: Prelude.Text,
    -- | The name of the dataset parameter.
    dataSetParameterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MappedDataSetParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetIdentifier', 'mappedDataSetParameter_dataSetIdentifier' - A unique name that identifies a dataset within the analysis or
-- dashboard.
--
-- 'dataSetParameterName', 'mappedDataSetParameter_dataSetParameterName' - The name of the dataset parameter.
newMappedDataSetParameter ::
  -- | 'dataSetIdentifier'
  Prelude.Text ->
  -- | 'dataSetParameterName'
  Prelude.Text ->
  MappedDataSetParameter
newMappedDataSetParameter
  pDataSetIdentifier_
  pDataSetParameterName_ =
    MappedDataSetParameter'
      { dataSetIdentifier =
          pDataSetIdentifier_,
        dataSetParameterName = pDataSetParameterName_
      }

-- | A unique name that identifies a dataset within the analysis or
-- dashboard.
mappedDataSetParameter_dataSetIdentifier :: Lens.Lens' MappedDataSetParameter Prelude.Text
mappedDataSetParameter_dataSetIdentifier = Lens.lens (\MappedDataSetParameter' {dataSetIdentifier} -> dataSetIdentifier) (\s@MappedDataSetParameter' {} a -> s {dataSetIdentifier = a} :: MappedDataSetParameter)

-- | The name of the dataset parameter.
mappedDataSetParameter_dataSetParameterName :: Lens.Lens' MappedDataSetParameter Prelude.Text
mappedDataSetParameter_dataSetParameterName = Lens.lens (\MappedDataSetParameter' {dataSetParameterName} -> dataSetParameterName) (\s@MappedDataSetParameter' {} a -> s {dataSetParameterName = a} :: MappedDataSetParameter)

instance Data.FromJSON MappedDataSetParameter where
  parseJSON =
    Data.withObject
      "MappedDataSetParameter"
      ( \x ->
          MappedDataSetParameter'
            Prelude.<$> (x Data..: "DataSetIdentifier")
            Prelude.<*> (x Data..: "DataSetParameterName")
      )

instance Prelude.Hashable MappedDataSetParameter where
  hashWithSalt _salt MappedDataSetParameter' {..} =
    _salt
      `Prelude.hashWithSalt` dataSetIdentifier
      `Prelude.hashWithSalt` dataSetParameterName

instance Prelude.NFData MappedDataSetParameter where
  rnf MappedDataSetParameter' {..} =
    Prelude.rnf dataSetIdentifier
      `Prelude.seq` Prelude.rnf dataSetParameterName

instance Data.ToJSON MappedDataSetParameter where
  toJSON MappedDataSetParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DataSetIdentifier" Data..= dataSetIdentifier),
            Prelude.Just
              ( "DataSetParameterName"
                  Data..= dataSetParameterName
              )
          ]
      )
