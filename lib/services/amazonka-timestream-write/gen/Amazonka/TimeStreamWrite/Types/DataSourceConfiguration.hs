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
-- Module      : Amazonka.TimeStreamWrite.Types.DataSourceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.DataSourceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.BatchLoadDataFormat
import Amazonka.TimeStreamWrite.Types.CsvConfiguration
import Amazonka.TimeStreamWrite.Types.DataSourceS3Configuration

-- | Defines configuration details about the data source.
--
-- /See:/ 'newDataSourceConfiguration' smart constructor.
data DataSourceConfiguration = DataSourceConfiguration'
  { csvConfiguration :: Prelude.Maybe CsvConfiguration,
    -- | Configuration of an S3 location for a file which contains data to load.
    dataSourceS3Configuration :: DataSourceS3Configuration,
    -- | This is currently CSV.
    dataFormat :: BatchLoadDataFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csvConfiguration', 'dataSourceConfiguration_csvConfiguration' - Undocumented member.
--
-- 'dataSourceS3Configuration', 'dataSourceConfiguration_dataSourceS3Configuration' - Configuration of an S3 location for a file which contains data to load.
--
-- 'dataFormat', 'dataSourceConfiguration_dataFormat' - This is currently CSV.
newDataSourceConfiguration ::
  -- | 'dataSourceS3Configuration'
  DataSourceS3Configuration ->
  -- | 'dataFormat'
  BatchLoadDataFormat ->
  DataSourceConfiguration
newDataSourceConfiguration
  pDataSourceS3Configuration_
  pDataFormat_ =
    DataSourceConfiguration'
      { csvConfiguration =
          Prelude.Nothing,
        dataSourceS3Configuration =
          pDataSourceS3Configuration_,
        dataFormat = pDataFormat_
      }

-- | Undocumented member.
dataSourceConfiguration_csvConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe CsvConfiguration)
dataSourceConfiguration_csvConfiguration = Lens.lens (\DataSourceConfiguration' {csvConfiguration} -> csvConfiguration) (\s@DataSourceConfiguration' {} a -> s {csvConfiguration = a} :: DataSourceConfiguration)

-- | Configuration of an S3 location for a file which contains data to load.
dataSourceConfiguration_dataSourceS3Configuration :: Lens.Lens' DataSourceConfiguration DataSourceS3Configuration
dataSourceConfiguration_dataSourceS3Configuration = Lens.lens (\DataSourceConfiguration' {dataSourceS3Configuration} -> dataSourceS3Configuration) (\s@DataSourceConfiguration' {} a -> s {dataSourceS3Configuration = a} :: DataSourceConfiguration)

-- | This is currently CSV.
dataSourceConfiguration_dataFormat :: Lens.Lens' DataSourceConfiguration BatchLoadDataFormat
dataSourceConfiguration_dataFormat = Lens.lens (\DataSourceConfiguration' {dataFormat} -> dataFormat) (\s@DataSourceConfiguration' {} a -> s {dataFormat = a} :: DataSourceConfiguration)

instance Data.FromJSON DataSourceConfiguration where
  parseJSON =
    Data.withObject
      "DataSourceConfiguration"
      ( \x ->
          DataSourceConfiguration'
            Prelude.<$> (x Data..:? "CsvConfiguration")
            Prelude.<*> (x Data..: "DataSourceS3Configuration")
            Prelude.<*> (x Data..: "DataFormat")
      )

instance Prelude.Hashable DataSourceConfiguration where
  hashWithSalt _salt DataSourceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` csvConfiguration
      `Prelude.hashWithSalt` dataSourceS3Configuration
      `Prelude.hashWithSalt` dataFormat

instance Prelude.NFData DataSourceConfiguration where
  rnf DataSourceConfiguration' {..} =
    Prelude.rnf csvConfiguration
      `Prelude.seq` Prelude.rnf dataSourceS3Configuration
      `Prelude.seq` Prelude.rnf dataFormat

instance Data.ToJSON DataSourceConfiguration where
  toJSON DataSourceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CsvConfiguration" Data..=)
              Prelude.<$> csvConfiguration,
            Prelude.Just
              ( "DataSourceS3Configuration"
                  Data..= dataSourceS3Configuration
              ),
            Prelude.Just ("DataFormat" Data..= dataFormat)
          ]
      )
