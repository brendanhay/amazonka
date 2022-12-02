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
-- Module      : Amazonka.M2.Types.DataSetImportConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.DataSetImportConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types.DataSetImportItem
import qualified Amazonka.Prelude as Prelude

-- | Identifies one or more data sets you want to import with the
-- CreateDataSetImportTask operation.
--
-- /See:/ 'newDataSetImportConfig' smart constructor.
data DataSetImportConfig = DataSetImportConfig'
  { -- | The data sets.
    dataSets :: Prelude.Maybe (Prelude.NonEmpty DataSetImportItem),
    -- | The Amazon S3 location of the data sets.
    s3Location :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetImportConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSets', 'dataSetImportConfig_dataSets' - The data sets.
--
-- 's3Location', 'dataSetImportConfig_s3Location' - The Amazon S3 location of the data sets.
newDataSetImportConfig ::
  DataSetImportConfig
newDataSetImportConfig =
  DataSetImportConfig'
    { dataSets = Prelude.Nothing,
      s3Location = Prelude.Nothing
    }

-- | The data sets.
dataSetImportConfig_dataSets :: Lens.Lens' DataSetImportConfig (Prelude.Maybe (Prelude.NonEmpty DataSetImportItem))
dataSetImportConfig_dataSets = Lens.lens (\DataSetImportConfig' {dataSets} -> dataSets) (\s@DataSetImportConfig' {} a -> s {dataSets = a} :: DataSetImportConfig) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 location of the data sets.
dataSetImportConfig_s3Location :: Lens.Lens' DataSetImportConfig (Prelude.Maybe Prelude.Text)
dataSetImportConfig_s3Location = Lens.lens (\DataSetImportConfig' {s3Location} -> s3Location) (\s@DataSetImportConfig' {} a -> s {s3Location = a} :: DataSetImportConfig)

instance Prelude.Hashable DataSetImportConfig where
  hashWithSalt _salt DataSetImportConfig' {..} =
    _salt `Prelude.hashWithSalt` dataSets
      `Prelude.hashWithSalt` s3Location

instance Prelude.NFData DataSetImportConfig where
  rnf DataSetImportConfig' {..} =
    Prelude.rnf dataSets
      `Prelude.seq` Prelude.rnf s3Location

instance Data.ToJSON DataSetImportConfig where
  toJSON DataSetImportConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataSets" Data..=) Prelude.<$> dataSets,
            ("s3Location" Data..=) Prelude.<$> s3Location
          ]
      )
