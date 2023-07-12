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
-- Module      : Amazonka.LookoutMetrics.Types.RedshiftSourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.RedshiftSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.VpcConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the Amazon Redshift database configuration.
--
-- /See:/ 'newRedshiftSourceConfig' smart constructor.
data RedshiftSourceConfig = RedshiftSourceConfig'
  { -- | A string identifying the Redshift cluster.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the database host.
    databaseHost :: Prelude.Maybe Prelude.Text,
    -- | The Redshift database name.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The port number where the database can be accessed.
    databasePort :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the role providing access to the
    -- database.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
    secretManagerArn :: Prelude.Maybe Prelude.Text,
    -- | The table name of the Redshift database.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the Amazon Virtual Private Cloud (VPC)
    -- configuration.
    vpcConfiguration :: Prelude.Maybe VpcConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'redshiftSourceConfig_clusterIdentifier' - A string identifying the Redshift cluster.
--
-- 'databaseHost', 'redshiftSourceConfig_databaseHost' - The name of the database host.
--
-- 'databaseName', 'redshiftSourceConfig_databaseName' - The Redshift database name.
--
-- 'databasePort', 'redshiftSourceConfig_databasePort' - The port number where the database can be accessed.
--
-- 'roleArn', 'redshiftSourceConfig_roleArn' - The Amazon Resource Name (ARN) of the role providing access to the
-- database.
--
-- 'secretManagerArn', 'redshiftSourceConfig_secretManagerArn' - The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
--
-- 'tableName', 'redshiftSourceConfig_tableName' - The table name of the Redshift database.
--
-- 'vpcConfiguration', 'redshiftSourceConfig_vpcConfiguration' - Contains information about the Amazon Virtual Private Cloud (VPC)
-- configuration.
newRedshiftSourceConfig ::
  RedshiftSourceConfig
newRedshiftSourceConfig =
  RedshiftSourceConfig'
    { clusterIdentifier =
        Prelude.Nothing,
      databaseHost = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      databasePort = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      secretManagerArn = Prelude.Nothing,
      tableName = Prelude.Nothing,
      vpcConfiguration = Prelude.Nothing
    }

-- | A string identifying the Redshift cluster.
redshiftSourceConfig_clusterIdentifier :: Lens.Lens' RedshiftSourceConfig (Prelude.Maybe Prelude.Text)
redshiftSourceConfig_clusterIdentifier = Lens.lens (\RedshiftSourceConfig' {clusterIdentifier} -> clusterIdentifier) (\s@RedshiftSourceConfig' {} a -> s {clusterIdentifier = a} :: RedshiftSourceConfig)

-- | The name of the database host.
redshiftSourceConfig_databaseHost :: Lens.Lens' RedshiftSourceConfig (Prelude.Maybe Prelude.Text)
redshiftSourceConfig_databaseHost = Lens.lens (\RedshiftSourceConfig' {databaseHost} -> databaseHost) (\s@RedshiftSourceConfig' {} a -> s {databaseHost = a} :: RedshiftSourceConfig)

-- | The Redshift database name.
redshiftSourceConfig_databaseName :: Lens.Lens' RedshiftSourceConfig (Prelude.Maybe Prelude.Text)
redshiftSourceConfig_databaseName = Lens.lens (\RedshiftSourceConfig' {databaseName} -> databaseName) (\s@RedshiftSourceConfig' {} a -> s {databaseName = a} :: RedshiftSourceConfig)

-- | The port number where the database can be accessed.
redshiftSourceConfig_databasePort :: Lens.Lens' RedshiftSourceConfig (Prelude.Maybe Prelude.Natural)
redshiftSourceConfig_databasePort = Lens.lens (\RedshiftSourceConfig' {databasePort} -> databasePort) (\s@RedshiftSourceConfig' {} a -> s {databasePort = a} :: RedshiftSourceConfig)

-- | The Amazon Resource Name (ARN) of the role providing access to the
-- database.
redshiftSourceConfig_roleArn :: Lens.Lens' RedshiftSourceConfig (Prelude.Maybe Prelude.Text)
redshiftSourceConfig_roleArn = Lens.lens (\RedshiftSourceConfig' {roleArn} -> roleArn) (\s@RedshiftSourceConfig' {} a -> s {roleArn = a} :: RedshiftSourceConfig)

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
redshiftSourceConfig_secretManagerArn :: Lens.Lens' RedshiftSourceConfig (Prelude.Maybe Prelude.Text)
redshiftSourceConfig_secretManagerArn = Lens.lens (\RedshiftSourceConfig' {secretManagerArn} -> secretManagerArn) (\s@RedshiftSourceConfig' {} a -> s {secretManagerArn = a} :: RedshiftSourceConfig)

-- | The table name of the Redshift database.
redshiftSourceConfig_tableName :: Lens.Lens' RedshiftSourceConfig (Prelude.Maybe Prelude.Text)
redshiftSourceConfig_tableName = Lens.lens (\RedshiftSourceConfig' {tableName} -> tableName) (\s@RedshiftSourceConfig' {} a -> s {tableName = a} :: RedshiftSourceConfig)

-- | Contains information about the Amazon Virtual Private Cloud (VPC)
-- configuration.
redshiftSourceConfig_vpcConfiguration :: Lens.Lens' RedshiftSourceConfig (Prelude.Maybe VpcConfiguration)
redshiftSourceConfig_vpcConfiguration = Lens.lens (\RedshiftSourceConfig' {vpcConfiguration} -> vpcConfiguration) (\s@RedshiftSourceConfig' {} a -> s {vpcConfiguration = a} :: RedshiftSourceConfig)

instance Data.FromJSON RedshiftSourceConfig where
  parseJSON =
    Data.withObject
      "RedshiftSourceConfig"
      ( \x ->
          RedshiftSourceConfig'
            Prelude.<$> (x Data..:? "ClusterIdentifier")
            Prelude.<*> (x Data..:? "DatabaseHost")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "DatabasePort")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "SecretManagerArn")
            Prelude.<*> (x Data..:? "TableName")
            Prelude.<*> (x Data..:? "VpcConfiguration")
      )

instance Prelude.Hashable RedshiftSourceConfig where
  hashWithSalt _salt RedshiftSourceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` databaseHost
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` databasePort
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` secretManagerArn
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` vpcConfiguration

instance Prelude.NFData RedshiftSourceConfig where
  rnf RedshiftSourceConfig' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf databaseHost
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf databasePort
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf secretManagerArn
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf vpcConfiguration

instance Data.ToJSON RedshiftSourceConfig where
  toJSON RedshiftSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterIdentifier" Data..=)
              Prelude.<$> clusterIdentifier,
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
