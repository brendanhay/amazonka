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
-- Module      : Network.AWS.AppSync.Types.RdsHttpEndpointConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.RdsHttpEndpointConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Amazon RDS HTTP endpoint configuration.
--
-- /See:/ 'newRdsHttpEndpointConfig' smart constructor.
data RdsHttpEndpointConfig = RdsHttpEndpointConfig'
  { -- | AWS secret store ARN for database credentials.
    awsSecretStoreArn :: Core.Maybe Core.Text,
    -- | Logical schema name.
    schema :: Core.Maybe Core.Text,
    -- | Amazon RDS cluster ARN.
    dbClusterIdentifier :: Core.Maybe Core.Text,
    -- | AWS Region for RDS HTTP endpoint.
    awsRegion :: Core.Maybe Core.Text,
    -- | Logical database name.
    databaseName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RdsHttpEndpointConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsSecretStoreArn', 'rdsHttpEndpointConfig_awsSecretStoreArn' - AWS secret store ARN for database credentials.
--
-- 'schema', 'rdsHttpEndpointConfig_schema' - Logical schema name.
--
-- 'dbClusterIdentifier', 'rdsHttpEndpointConfig_dbClusterIdentifier' - Amazon RDS cluster ARN.
--
-- 'awsRegion', 'rdsHttpEndpointConfig_awsRegion' - AWS Region for RDS HTTP endpoint.
--
-- 'databaseName', 'rdsHttpEndpointConfig_databaseName' - Logical database name.
newRdsHttpEndpointConfig ::
  RdsHttpEndpointConfig
newRdsHttpEndpointConfig =
  RdsHttpEndpointConfig'
    { awsSecretStoreArn =
        Core.Nothing,
      schema = Core.Nothing,
      dbClusterIdentifier = Core.Nothing,
      awsRegion = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | AWS secret store ARN for database credentials.
rdsHttpEndpointConfig_awsSecretStoreArn :: Lens.Lens' RdsHttpEndpointConfig (Core.Maybe Core.Text)
rdsHttpEndpointConfig_awsSecretStoreArn = Lens.lens (\RdsHttpEndpointConfig' {awsSecretStoreArn} -> awsSecretStoreArn) (\s@RdsHttpEndpointConfig' {} a -> s {awsSecretStoreArn = a} :: RdsHttpEndpointConfig)

-- | Logical schema name.
rdsHttpEndpointConfig_schema :: Lens.Lens' RdsHttpEndpointConfig (Core.Maybe Core.Text)
rdsHttpEndpointConfig_schema = Lens.lens (\RdsHttpEndpointConfig' {schema} -> schema) (\s@RdsHttpEndpointConfig' {} a -> s {schema = a} :: RdsHttpEndpointConfig)

-- | Amazon RDS cluster ARN.
rdsHttpEndpointConfig_dbClusterIdentifier :: Lens.Lens' RdsHttpEndpointConfig (Core.Maybe Core.Text)
rdsHttpEndpointConfig_dbClusterIdentifier = Lens.lens (\RdsHttpEndpointConfig' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RdsHttpEndpointConfig' {} a -> s {dbClusterIdentifier = a} :: RdsHttpEndpointConfig)

-- | AWS Region for RDS HTTP endpoint.
rdsHttpEndpointConfig_awsRegion :: Lens.Lens' RdsHttpEndpointConfig (Core.Maybe Core.Text)
rdsHttpEndpointConfig_awsRegion = Lens.lens (\RdsHttpEndpointConfig' {awsRegion} -> awsRegion) (\s@RdsHttpEndpointConfig' {} a -> s {awsRegion = a} :: RdsHttpEndpointConfig)

-- | Logical database name.
rdsHttpEndpointConfig_databaseName :: Lens.Lens' RdsHttpEndpointConfig (Core.Maybe Core.Text)
rdsHttpEndpointConfig_databaseName = Lens.lens (\RdsHttpEndpointConfig' {databaseName} -> databaseName) (\s@RdsHttpEndpointConfig' {} a -> s {databaseName = a} :: RdsHttpEndpointConfig)

instance Core.FromJSON RdsHttpEndpointConfig where
  parseJSON =
    Core.withObject
      "RdsHttpEndpointConfig"
      ( \x ->
          RdsHttpEndpointConfig'
            Core.<$> (x Core..:? "awsSecretStoreArn")
            Core.<*> (x Core..:? "schema")
            Core.<*> (x Core..:? "dbClusterIdentifier")
            Core.<*> (x Core..:? "awsRegion")
            Core.<*> (x Core..:? "databaseName")
      )

instance Core.Hashable RdsHttpEndpointConfig

instance Core.NFData RdsHttpEndpointConfig

instance Core.ToJSON RdsHttpEndpointConfig where
  toJSON RdsHttpEndpointConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("awsSecretStoreArn" Core..=)
              Core.<$> awsSecretStoreArn,
            ("schema" Core..=) Core.<$> schema,
            ("dbClusterIdentifier" Core..=)
              Core.<$> dbClusterIdentifier,
            ("awsRegion" Core..=) Core.<$> awsRegion,
            ("databaseName" Core..=) Core.<$> databaseName
          ]
      )
