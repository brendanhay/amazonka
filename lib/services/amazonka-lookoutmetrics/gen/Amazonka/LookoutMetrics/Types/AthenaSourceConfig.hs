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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AthenaSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types.BackTestConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Details about an Amazon Athena datasource.
--
-- /See:/ 'newAthenaSourceConfig' smart constructor.
data AthenaSourceConfig = AthenaSourceConfig'
  { -- | The database\'s table name.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | An IAM role that gives Amazon Lookout for Metrics permission to access
    -- the data.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The database\'s data catalog.
    dataCatalog :: Prelude.Maybe Prelude.Text,
    -- | The database\'s name.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Settings for backtest mode.
    backTestConfiguration :: Prelude.Maybe BackTestConfiguration,
    -- | The database\'s results path.
    s3ResultsPath :: Prelude.Maybe Prelude.Text,
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
-- 'tableName', 'athenaSourceConfig_tableName' - The database\'s table name.
--
-- 'roleArn', 'athenaSourceConfig_roleArn' - An IAM role that gives Amazon Lookout for Metrics permission to access
-- the data.
--
-- 'dataCatalog', 'athenaSourceConfig_dataCatalog' - The database\'s data catalog.
--
-- 'databaseName', 'athenaSourceConfig_databaseName' - The database\'s name.
--
-- 'backTestConfiguration', 'athenaSourceConfig_backTestConfiguration' - Settings for backtest mode.
--
-- 's3ResultsPath', 'athenaSourceConfig_s3ResultsPath' - The database\'s results path.
--
-- 'workGroupName', 'athenaSourceConfig_workGroupName' - The database\'s work group name.
newAthenaSourceConfig ::
  AthenaSourceConfig
newAthenaSourceConfig =
  AthenaSourceConfig'
    { tableName = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      dataCatalog = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      backTestConfiguration = Prelude.Nothing,
      s3ResultsPath = Prelude.Nothing,
      workGroupName = Prelude.Nothing
    }

-- | The database\'s table name.
athenaSourceConfig_tableName :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe Prelude.Text)
athenaSourceConfig_tableName = Lens.lens (\AthenaSourceConfig' {tableName} -> tableName) (\s@AthenaSourceConfig' {} a -> s {tableName = a} :: AthenaSourceConfig)

-- | An IAM role that gives Amazon Lookout for Metrics permission to access
-- the data.
athenaSourceConfig_roleArn :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe Prelude.Text)
athenaSourceConfig_roleArn = Lens.lens (\AthenaSourceConfig' {roleArn} -> roleArn) (\s@AthenaSourceConfig' {} a -> s {roleArn = a} :: AthenaSourceConfig)

-- | The database\'s data catalog.
athenaSourceConfig_dataCatalog :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe Prelude.Text)
athenaSourceConfig_dataCatalog = Lens.lens (\AthenaSourceConfig' {dataCatalog} -> dataCatalog) (\s@AthenaSourceConfig' {} a -> s {dataCatalog = a} :: AthenaSourceConfig)

-- | The database\'s name.
athenaSourceConfig_databaseName :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe Prelude.Text)
athenaSourceConfig_databaseName = Lens.lens (\AthenaSourceConfig' {databaseName} -> databaseName) (\s@AthenaSourceConfig' {} a -> s {databaseName = a} :: AthenaSourceConfig)

-- | Settings for backtest mode.
athenaSourceConfig_backTestConfiguration :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe BackTestConfiguration)
athenaSourceConfig_backTestConfiguration = Lens.lens (\AthenaSourceConfig' {backTestConfiguration} -> backTestConfiguration) (\s@AthenaSourceConfig' {} a -> s {backTestConfiguration = a} :: AthenaSourceConfig)

-- | The database\'s results path.
athenaSourceConfig_s3ResultsPath :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe Prelude.Text)
athenaSourceConfig_s3ResultsPath = Lens.lens (\AthenaSourceConfig' {s3ResultsPath} -> s3ResultsPath) (\s@AthenaSourceConfig' {} a -> s {s3ResultsPath = a} :: AthenaSourceConfig)

-- | The database\'s work group name.
athenaSourceConfig_workGroupName :: Lens.Lens' AthenaSourceConfig (Prelude.Maybe Prelude.Text)
athenaSourceConfig_workGroupName = Lens.lens (\AthenaSourceConfig' {workGroupName} -> workGroupName) (\s@AthenaSourceConfig' {} a -> s {workGroupName = a} :: AthenaSourceConfig)

instance Core.FromJSON AthenaSourceConfig where
  parseJSON =
    Core.withObject
      "AthenaSourceConfig"
      ( \x ->
          AthenaSourceConfig'
            Prelude.<$> (x Core..:? "TableName")
            Prelude.<*> (x Core..:? "RoleArn")
            Prelude.<*> (x Core..:? "DataCatalog")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "BackTestConfiguration")
            Prelude.<*> (x Core..:? "S3ResultsPath")
            Prelude.<*> (x Core..:? "WorkGroupName")
      )

instance Prelude.Hashable AthenaSourceConfig where
  hashWithSalt _salt AthenaSourceConfig' {..} =
    _salt `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` dataCatalog
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` backTestConfiguration
      `Prelude.hashWithSalt` s3ResultsPath
      `Prelude.hashWithSalt` workGroupName

instance Prelude.NFData AthenaSourceConfig where
  rnf AthenaSourceConfig' {..} =
    Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf dataCatalog
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf backTestConfiguration
      `Prelude.seq` Prelude.rnf s3ResultsPath
      `Prelude.seq` Prelude.rnf workGroupName

instance Core.ToJSON AthenaSourceConfig where
  toJSON AthenaSourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TableName" Core..=) Prelude.<$> tableName,
            ("RoleArn" Core..=) Prelude.<$> roleArn,
            ("DataCatalog" Core..=) Prelude.<$> dataCatalog,
            ("DatabaseName" Core..=) Prelude.<$> databaseName,
            ("BackTestConfiguration" Core..=)
              Prelude.<$> backTestConfiguration,
            ("S3ResultsPath" Core..=) Prelude.<$> s3ResultsPath,
            ("WorkGroupName" Core..=) Prelude.<$> workGroupName
          ]
      )
