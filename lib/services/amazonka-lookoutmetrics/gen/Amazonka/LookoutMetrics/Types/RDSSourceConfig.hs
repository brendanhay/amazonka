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
-- Module      : Amazonka.LookoutMetrics.Types.RDSSourceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.RDSSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types.VpcConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the Amazon Relational Database Service (RDS)
-- configuration.
--
-- /See:/ 'newRDSSourceConfig' smart constructor.
data RDSSourceConfig = RDSSourceConfig'
  { -- | An object containing information about the Amazon Virtual Private Cloud
    -- (VPC) configuration.
    vpcConfiguration :: Prelude.Maybe VpcConfiguration,
    -- | The name of the table in the database.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | A string identifying the database instance.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the RDS database.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The host name of the database.
    databaseHost :: Prelude.Maybe Prelude.Text,
    -- | The port number where the database can be accessed.
    databasePort :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
    secretManagerArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RDSSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfiguration', 'rDSSourceConfig_vpcConfiguration' - An object containing information about the Amazon Virtual Private Cloud
-- (VPC) configuration.
--
-- 'tableName', 'rDSSourceConfig_tableName' - The name of the table in the database.
--
-- 'roleArn', 'rDSSourceConfig_roleArn' - The Amazon Resource Name (ARN) of the role.
--
-- 'dbInstanceIdentifier', 'rDSSourceConfig_dbInstanceIdentifier' - A string identifying the database instance.
--
-- 'databaseName', 'rDSSourceConfig_databaseName' - The name of the RDS database.
--
-- 'databaseHost', 'rDSSourceConfig_databaseHost' - The host name of the database.
--
-- 'databasePort', 'rDSSourceConfig_databasePort' - The port number where the database can be accessed.
--
-- 'secretManagerArn', 'rDSSourceConfig_secretManagerArn' - The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
newRDSSourceConfig ::
  RDSSourceConfig
newRDSSourceConfig =
  RDSSourceConfig'
    { vpcConfiguration =
        Prelude.Nothing,
      tableName = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      databaseHost = Prelude.Nothing,
      databasePort = Prelude.Nothing,
      secretManagerArn = Prelude.Nothing
    }

-- | An object containing information about the Amazon Virtual Private Cloud
-- (VPC) configuration.
rDSSourceConfig_vpcConfiguration :: Lens.Lens' RDSSourceConfig (Prelude.Maybe VpcConfiguration)
rDSSourceConfig_vpcConfiguration = Lens.lens (\RDSSourceConfig' {vpcConfiguration} -> vpcConfiguration) (\s@RDSSourceConfig' {} a -> s {vpcConfiguration = a} :: RDSSourceConfig)

-- | The name of the table in the database.
rDSSourceConfig_tableName :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Text)
rDSSourceConfig_tableName = Lens.lens (\RDSSourceConfig' {tableName} -> tableName) (\s@RDSSourceConfig' {} a -> s {tableName = a} :: RDSSourceConfig)

-- | The Amazon Resource Name (ARN) of the role.
rDSSourceConfig_roleArn :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Text)
rDSSourceConfig_roleArn = Lens.lens (\RDSSourceConfig' {roleArn} -> roleArn) (\s@RDSSourceConfig' {} a -> s {roleArn = a} :: RDSSourceConfig)

-- | A string identifying the database instance.
rDSSourceConfig_dbInstanceIdentifier :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Text)
rDSSourceConfig_dbInstanceIdentifier = Lens.lens (\RDSSourceConfig' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RDSSourceConfig' {} a -> s {dbInstanceIdentifier = a} :: RDSSourceConfig)

-- | The name of the RDS database.
rDSSourceConfig_databaseName :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Text)
rDSSourceConfig_databaseName = Lens.lens (\RDSSourceConfig' {databaseName} -> databaseName) (\s@RDSSourceConfig' {} a -> s {databaseName = a} :: RDSSourceConfig)

-- | The host name of the database.
rDSSourceConfig_databaseHost :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Text)
rDSSourceConfig_databaseHost = Lens.lens (\RDSSourceConfig' {databaseHost} -> databaseHost) (\s@RDSSourceConfig' {} a -> s {databaseHost = a} :: RDSSourceConfig)

-- | The port number where the database can be accessed.
rDSSourceConfig_databasePort :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Natural)
rDSSourceConfig_databasePort = Lens.lens (\RDSSourceConfig' {databasePort} -> databasePort) (\s@RDSSourceConfig' {} a -> s {databasePort = a} :: RDSSourceConfig)

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
rDSSourceConfig_secretManagerArn :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Text)
rDSSourceConfig_secretManagerArn = Lens.lens (\RDSSourceConfig' {secretManagerArn} -> secretManagerArn) (\s@RDSSourceConfig' {} a -> s {secretManagerArn = a} :: RDSSourceConfig)

instance Core.FromJSON RDSSourceConfig where
  parseJSON =
    Core.withObject
      "RDSSourceConfig"
      ( \x ->
          RDSSourceConfig'
            Prelude.<$> (x Core..:? "VpcConfiguration")
            Prelude.<*> (x Core..:? "TableName")
            Prelude.<*> (x Core..:? "RoleArn")
            Prelude.<*> (x Core..:? "DBInstanceIdentifier")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "DatabaseHost")
            Prelude.<*> (x Core..:? "DatabasePort")
            Prelude.<*> (x Core..:? "SecretManagerArn")
      )

instance Prelude.Hashable RDSSourceConfig where
  hashWithSalt _salt RDSSourceConfig' {..} =
    _salt `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` databaseHost
      `Prelude.hashWithSalt` databasePort
      `Prelude.hashWithSalt` secretManagerArn

instance Prelude.NFData RDSSourceConfig where
  rnf RDSSourceConfig' {..} =
    Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf databaseHost
      `Prelude.seq` Prelude.rnf databasePort
      `Prelude.seq` Prelude.rnf secretManagerArn

instance Core.ToJSON RDSSourceConfig where
  toJSON RDSSourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VpcConfiguration" Core..=)
              Prelude.<$> vpcConfiguration,
            ("TableName" Core..=) Prelude.<$> tableName,
            ("RoleArn" Core..=) Prelude.<$> roleArn,
            ("DBInstanceIdentifier" Core..=)
              Prelude.<$> dbInstanceIdentifier,
            ("DatabaseName" Core..=) Prelude.<$> databaseName,
            ("DatabaseHost" Core..=) Prelude.<$> databaseHost,
            ("DatabasePort" Core..=) Prelude.<$> databasePort,
            ("SecretManagerArn" Core..=)
              Prelude.<$> secretManagerArn
          ]
      )
