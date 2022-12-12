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
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.VpcConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the Amazon Relational Database Service (RDS)
-- configuration.
--
-- /See:/ 'newRDSSourceConfig' smart constructor.
data RDSSourceConfig = RDSSourceConfig'
  { -- | A string identifying the database instance.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The host name of the database.
    databaseHost :: Prelude.Maybe Prelude.Text,
    -- | The name of the RDS database.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The port number where the database can be accessed.
    databasePort :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the role.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
    secretManagerArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the table in the database.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | An object containing information about the Amazon Virtual Private Cloud
    -- (VPC) configuration.
    vpcConfiguration :: Prelude.Maybe VpcConfiguration
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
-- 'dbInstanceIdentifier', 'rDSSourceConfig_dbInstanceIdentifier' - A string identifying the database instance.
--
-- 'databaseHost', 'rDSSourceConfig_databaseHost' - The host name of the database.
--
-- 'databaseName', 'rDSSourceConfig_databaseName' - The name of the RDS database.
--
-- 'databasePort', 'rDSSourceConfig_databasePort' - The port number where the database can be accessed.
--
-- 'roleArn', 'rDSSourceConfig_roleArn' - The Amazon Resource Name (ARN) of the role.
--
-- 'secretManagerArn', 'rDSSourceConfig_secretManagerArn' - The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
--
-- 'tableName', 'rDSSourceConfig_tableName' - The name of the table in the database.
--
-- 'vpcConfiguration', 'rDSSourceConfig_vpcConfiguration' - An object containing information about the Amazon Virtual Private Cloud
-- (VPC) configuration.
newRDSSourceConfig ::
  RDSSourceConfig
newRDSSourceConfig =
  RDSSourceConfig'
    { dbInstanceIdentifier =
        Prelude.Nothing,
      databaseHost = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      databasePort = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      secretManagerArn = Prelude.Nothing,
      tableName = Prelude.Nothing,
      vpcConfiguration = Prelude.Nothing
    }

-- | A string identifying the database instance.
rDSSourceConfig_dbInstanceIdentifier :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Text)
rDSSourceConfig_dbInstanceIdentifier = Lens.lens (\RDSSourceConfig' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RDSSourceConfig' {} a -> s {dbInstanceIdentifier = a} :: RDSSourceConfig)

-- | The host name of the database.
rDSSourceConfig_databaseHost :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Text)
rDSSourceConfig_databaseHost = Lens.lens (\RDSSourceConfig' {databaseHost} -> databaseHost) (\s@RDSSourceConfig' {} a -> s {databaseHost = a} :: RDSSourceConfig)

-- | The name of the RDS database.
rDSSourceConfig_databaseName :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Text)
rDSSourceConfig_databaseName = Lens.lens (\RDSSourceConfig' {databaseName} -> databaseName) (\s@RDSSourceConfig' {} a -> s {databaseName = a} :: RDSSourceConfig)

-- | The port number where the database can be accessed.
rDSSourceConfig_databasePort :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Natural)
rDSSourceConfig_databasePort = Lens.lens (\RDSSourceConfig' {databasePort} -> databasePort) (\s@RDSSourceConfig' {} a -> s {databasePort = a} :: RDSSourceConfig)

-- | The Amazon Resource Name (ARN) of the role.
rDSSourceConfig_roleArn :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Text)
rDSSourceConfig_roleArn = Lens.lens (\RDSSourceConfig' {roleArn} -> roleArn) (\s@RDSSourceConfig' {} a -> s {roleArn = a} :: RDSSourceConfig)

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
rDSSourceConfig_secretManagerArn :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Text)
rDSSourceConfig_secretManagerArn = Lens.lens (\RDSSourceConfig' {secretManagerArn} -> secretManagerArn) (\s@RDSSourceConfig' {} a -> s {secretManagerArn = a} :: RDSSourceConfig)

-- | The name of the table in the database.
rDSSourceConfig_tableName :: Lens.Lens' RDSSourceConfig (Prelude.Maybe Prelude.Text)
rDSSourceConfig_tableName = Lens.lens (\RDSSourceConfig' {tableName} -> tableName) (\s@RDSSourceConfig' {} a -> s {tableName = a} :: RDSSourceConfig)

-- | An object containing information about the Amazon Virtual Private Cloud
-- (VPC) configuration.
rDSSourceConfig_vpcConfiguration :: Lens.Lens' RDSSourceConfig (Prelude.Maybe VpcConfiguration)
rDSSourceConfig_vpcConfiguration = Lens.lens (\RDSSourceConfig' {vpcConfiguration} -> vpcConfiguration) (\s@RDSSourceConfig' {} a -> s {vpcConfiguration = a} :: RDSSourceConfig)

instance Data.FromJSON RDSSourceConfig where
  parseJSON =
    Data.withObject
      "RDSSourceConfig"
      ( \x ->
          RDSSourceConfig'
            Prelude.<$> (x Data..:? "DBInstanceIdentifier")
            Prelude.<*> (x Data..:? "DatabaseHost")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "DatabasePort")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "SecretManagerArn")
            Prelude.<*> (x Data..:? "TableName")
            Prelude.<*> (x Data..:? "VpcConfiguration")
      )

instance Prelude.Hashable RDSSourceConfig where
  hashWithSalt _salt RDSSourceConfig' {..} =
    _salt `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` databaseHost
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` databasePort
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` secretManagerArn
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` vpcConfiguration

instance Prelude.NFData RDSSourceConfig where
  rnf RDSSourceConfig' {..} =
    Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf databaseHost
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf databasePort
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf secretManagerArn
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf vpcConfiguration

instance Data.ToJSON RDSSourceConfig where
  toJSON RDSSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DBInstanceIdentifier" Data..=)
              Prelude.<$> dbInstanceIdentifier,
            ("DatabaseHost" Data..=) Prelude.<$> databaseHost,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("DatabasePort" Data..=) Prelude.<$> databasePort,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("SecretManagerArn" Data..=)
              Prelude.<$> secretManagerArn,
            ("TableName" Data..=) Prelude.<$> tableName,
            ("VpcConfiguration" Data..=)
              Prelude.<$> vpcConfiguration
          ]
      )
