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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.RDSSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutMetrics.Types.VpcConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the Amazon Relational Database Service (RDS)
-- configuration.
--
-- /See:/ 'newRDSSourceConfig' smart constructor.
data RDSSourceConfig = RDSSourceConfig'
  { -- | A string identifying the database instance.
    dbInstanceIdentifier :: Prelude.Text,
    -- | The host name of the database.
    databaseHost :: Prelude.Text,
    -- | The port number where the database can be accessed.
    databasePort :: Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
    secretManagerArn :: Prelude.Text,
    -- | The name of the RDS database.
    databaseName :: Prelude.Text,
    -- | The name of the table in the database.
    tableName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role.
    roleArn :: Prelude.Text,
    -- | An object containing information about the Amazon Virtual Private Cloud
    -- (VPC) configuration.
    vpcConfiguration :: VpcConfiguration
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
-- 'databasePort', 'rDSSourceConfig_databasePort' - The port number where the database can be accessed.
--
-- 'secretManagerArn', 'rDSSourceConfig_secretManagerArn' - The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
--
-- 'databaseName', 'rDSSourceConfig_databaseName' - The name of the RDS database.
--
-- 'tableName', 'rDSSourceConfig_tableName' - The name of the table in the database.
--
-- 'roleArn', 'rDSSourceConfig_roleArn' - The Amazon Resource Name (ARN) of the role.
--
-- 'vpcConfiguration', 'rDSSourceConfig_vpcConfiguration' - An object containing information about the Amazon Virtual Private Cloud
-- (VPC) configuration.
newRDSSourceConfig ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  -- | 'databaseHost'
  Prelude.Text ->
  -- | 'databasePort'
  Prelude.Natural ->
  -- | 'secretManagerArn'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'vpcConfiguration'
  VpcConfiguration ->
  RDSSourceConfig
newRDSSourceConfig
  pDBInstanceIdentifier_
  pDatabaseHost_
  pDatabasePort_
  pSecretManagerArn_
  pDatabaseName_
  pTableName_
  pRoleArn_
  pVpcConfiguration_ =
    RDSSourceConfig'
      { dbInstanceIdentifier =
          pDBInstanceIdentifier_,
        databaseHost = pDatabaseHost_,
        databasePort = pDatabasePort_,
        secretManagerArn = pSecretManagerArn_,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        roleArn = pRoleArn_,
        vpcConfiguration = pVpcConfiguration_
      }

-- | A string identifying the database instance.
rDSSourceConfig_dbInstanceIdentifier :: Lens.Lens' RDSSourceConfig Prelude.Text
rDSSourceConfig_dbInstanceIdentifier = Lens.lens (\RDSSourceConfig' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RDSSourceConfig' {} a -> s {dbInstanceIdentifier = a} :: RDSSourceConfig)

-- | The host name of the database.
rDSSourceConfig_databaseHost :: Lens.Lens' RDSSourceConfig Prelude.Text
rDSSourceConfig_databaseHost = Lens.lens (\RDSSourceConfig' {databaseHost} -> databaseHost) (\s@RDSSourceConfig' {} a -> s {databaseHost = a} :: RDSSourceConfig)

-- | The port number where the database can be accessed.
rDSSourceConfig_databasePort :: Lens.Lens' RDSSourceConfig Prelude.Natural
rDSSourceConfig_databasePort = Lens.lens (\RDSSourceConfig' {databasePort} -> databasePort) (\s@RDSSourceConfig' {} a -> s {databasePort = a} :: RDSSourceConfig)

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
rDSSourceConfig_secretManagerArn :: Lens.Lens' RDSSourceConfig Prelude.Text
rDSSourceConfig_secretManagerArn = Lens.lens (\RDSSourceConfig' {secretManagerArn} -> secretManagerArn) (\s@RDSSourceConfig' {} a -> s {secretManagerArn = a} :: RDSSourceConfig)

-- | The name of the RDS database.
rDSSourceConfig_databaseName :: Lens.Lens' RDSSourceConfig Prelude.Text
rDSSourceConfig_databaseName = Lens.lens (\RDSSourceConfig' {databaseName} -> databaseName) (\s@RDSSourceConfig' {} a -> s {databaseName = a} :: RDSSourceConfig)

-- | The name of the table in the database.
rDSSourceConfig_tableName :: Lens.Lens' RDSSourceConfig Prelude.Text
rDSSourceConfig_tableName = Lens.lens (\RDSSourceConfig' {tableName} -> tableName) (\s@RDSSourceConfig' {} a -> s {tableName = a} :: RDSSourceConfig)

-- | The Amazon Resource Name (ARN) of the role.
rDSSourceConfig_roleArn :: Lens.Lens' RDSSourceConfig Prelude.Text
rDSSourceConfig_roleArn = Lens.lens (\RDSSourceConfig' {roleArn} -> roleArn) (\s@RDSSourceConfig' {} a -> s {roleArn = a} :: RDSSourceConfig)

-- | An object containing information about the Amazon Virtual Private Cloud
-- (VPC) configuration.
rDSSourceConfig_vpcConfiguration :: Lens.Lens' RDSSourceConfig VpcConfiguration
rDSSourceConfig_vpcConfiguration = Lens.lens (\RDSSourceConfig' {vpcConfiguration} -> vpcConfiguration) (\s@RDSSourceConfig' {} a -> s {vpcConfiguration = a} :: RDSSourceConfig)

instance Core.FromJSON RDSSourceConfig where
  parseJSON =
    Core.withObject
      "RDSSourceConfig"
      ( \x ->
          RDSSourceConfig'
            Prelude.<$> (x Core..: "DBInstanceIdentifier")
            Prelude.<*> (x Core..: "DatabaseHost")
            Prelude.<*> (x Core..: "DatabasePort")
            Prelude.<*> (x Core..: "SecretManagerArn")
            Prelude.<*> (x Core..: "DatabaseName")
            Prelude.<*> (x Core..: "TableName")
            Prelude.<*> (x Core..: "RoleArn")
            Prelude.<*> (x Core..: "VpcConfiguration")
      )

instance Prelude.Hashable RDSSourceConfig where
  hashWithSalt _salt RDSSourceConfig' {..} =
    _salt `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` databaseHost
      `Prelude.hashWithSalt` databasePort
      `Prelude.hashWithSalt` secretManagerArn
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` vpcConfiguration

instance Prelude.NFData RDSSourceConfig where
  rnf RDSSourceConfig' {..} =
    Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf databaseHost
      `Prelude.seq` Prelude.rnf databasePort
      `Prelude.seq` Prelude.rnf secretManagerArn
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf vpcConfiguration

instance Core.ToJSON RDSSourceConfig where
  toJSON RDSSourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DBInstanceIdentifier"
                  Core..= dbInstanceIdentifier
              ),
            Prelude.Just ("DatabaseHost" Core..= databaseHost),
            Prelude.Just ("DatabasePort" Core..= databasePort),
            Prelude.Just
              ("SecretManagerArn" Core..= secretManagerArn),
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("TableName" Core..= tableName),
            Prelude.Just ("RoleArn" Core..= roleArn),
            Prelude.Just
              ("VpcConfiguration" Core..= vpcConfiguration)
          ]
      )
