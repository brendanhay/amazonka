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
-- Module      : Amazonka.LookoutMetrics.Types.AthenaSourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AthenaSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.BackTestConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Details about an Amazon Athena datasource.
--
-- /See:/ 'newAthenaSourceConfig' smart constructor.
data AthenaSourceConfig = AthenaSourceConfig'
  { -- | Settings for backtest mode.
    backTestConfiguration :: Prelude.Maybe BackTestConfiguration,
    -- | The database\'s data catalog.
    dataCatalog :: Prelude.Maybe Prelude.Text,
    -- | The database\'s name.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | An IAM role that gives Amazon Lookout for Metrics permission to access
    -- the data.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The database\'s results path.
    s3ResultsPath :: Prelude.Maybe Prelude.Text,
    -- | The database\'s table name.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The database\'s work group name.
    workGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AthenaSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backTestConfiguration', 'athenaSourceConfig_backTestConfiguration' - Settings for backtest mode.
--
-- 'dataCatalog', 'athenaSourceConfig_dataCatalog' - The database\'s data catalog.
--
-- 'databaseName', 'athenaSourceConfig_databaseName' - The database\'s name.
--
-- 'roleArn', 'athenaSourceConfig_roleArn' - An IAM role that gives Amazon Lookout for Metrics permission to access
-- the data.
--
-- 's3ResultsPath', 'athenaSourceConfig_s3ResultsPath' - The database\'s results path.
--
-- 'tableName', 'athenaSourceConfig_tableName' - The database\'s table name.
--
-- 'workGroupName', 'athenaSourceConfig_workGroupName' - The database\'s work group name.
newAthenaSourceConfig ::
  AthenaSourceConfig
newAthenaSourceConfig =
  AthenaSourceConfig'
    { backTestConfiguration =
        Prelude.Nothing,
      dataCatalog = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      s3ResultsPath = Prelude.Nothing,
      tableName = Prelude.Nothing,
      workGroupName = Prelude.Nothing
    }

-- | Settings for backtest mode.
athenaSourceConfig_backTestConfiguration :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe BackTestConfiguration)
athenaSourceConfig_backTestConfiguration = Lens.lens (\AthenaSourceConfig' {backTestConfiguration} -> backTestConfiguration) (\s@AthenaSourceConfig' {} a -> s {backTestConfiguration = a} :: AthenaSourceConfig)

-- | The database\'s data catalog.
athenaSourceConfig_dataCatalog :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe Prelude.Text)
athenaSourceConfig_dataCatalog = Lens.lens (\AthenaSourceConfig' {dataCatalog} -> dataCatalog) (\s@AthenaSourceConfig' {} a -> s {dataCatalog = a} :: AthenaSourceConfig)

-- | The database\'s name.
athenaSourceConfig_databaseName :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe Prelude.Text)
athenaSourceConfig_databaseName = Lens.lens (\AthenaSourceConfig' {databaseName} -> databaseName) (\s@AthenaSourceConfig' {} a -> s {databaseName = a} :: AthenaSourceConfig)

-- | An IAM role that gives Amazon Lookout for Metrics permission to access
-- the data.
athenaSourceConfig_roleArn :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe Prelude.Text)
athenaSourceConfig_roleArn = Lens.lens (\AthenaSourceConfig' {roleArn} -> roleArn) (\s@AthenaSourceConfig' {} a -> s {roleArn = a} :: AthenaSourceConfig)

-- | The database\'s results path.
athenaSourceConfig_s3ResultsPath :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe Prelude.Text)
athenaSourceConfig_s3ResultsPath = Lens.lens (\AthenaSourceConfig' {s3ResultsPath} -> s3ResultsPath) (\s@AthenaSourceConfig' {} a -> s {s3ResultsPath = a} :: AthenaSourceConfig)

-- | The database\'s table name.
athenaSourceConfig_tableName :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe Prelude.Text)
athenaSourceConfig_tableName = Lens.lens (\AthenaSourceConfig' {tableName} -> tableName) (\s@AthenaSourceConfig' {} a -> s {tableName = a} :: AthenaSourceConfig)

-- | The database\'s work group name.
athenaSourceConfig_workGroupName :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe Prelude.Text)
athenaSourceConfig_workGroupName = Lens.lens (\AthenaSourceConfig' {workGroupName} -> workGroupName) (\s@AthenaSourceConfig' {} a -> s {workGroupName = a} :: AthenaSourceConfig)

instance Data.FromJSON AthenaSourceConfig where
  parseJSON =
    Data.withObject
      "AthenaSourceConfig"
      ( \x ->
          AthenaSourceConfig'
            Prelude.<$> (x Data..:? "BackTestConfiguration")
            Prelude.<*> (x Data..:? "DataCatalog")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "S3ResultsPath")
            Prelude.<*> (x Data..:? "TableName")
            Prelude.<*> (x Data..:? "WorkGroupName")
      )

instance Prelude.Hashable AthenaSourceConfig where
  hashWithSalt _salt AthenaSourceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` backTestConfiguration
      `Prelude.hashWithSalt` dataCatalog
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` s3ResultsPath
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` workGroupName

instance Prelude.NFData AthenaSourceConfig where
  rnf AthenaSourceConfig' {..} =
    Prelude.rnf backTestConfiguration
      `Prelude.seq` Prelude.rnf dataCatalog
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf s3ResultsPath
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf workGroupName

instance Data.ToJSON AthenaSourceConfig where
  toJSON AthenaSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackTestConfiguration" Data..=)
              Prelude.<$> backTestConfiguration,
            ("DataCatalog" Data..=) Prelude.<$> dataCatalog,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("S3ResultsPath" Data..=) Prelude.<$> s3ResultsPath,
            ("TableName" Data..=) Prelude.<$> tableName,
            ("WorkGroupName" Data..=) Prelude.<$> workGroupName
          ]
      )
