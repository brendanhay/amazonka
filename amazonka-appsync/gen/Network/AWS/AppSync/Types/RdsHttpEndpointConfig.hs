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
import qualified Network.AWS.Prelude as Prelude

-- | The Amazon RDS HTTP endpoint configuration.
--
-- /See:/ 'newRdsHttpEndpointConfig' smart constructor.
data RdsHttpEndpointConfig = RdsHttpEndpointConfig'
  { -- | AWS secret store ARN for database credentials.
    awsSecretStoreArn :: Prelude.Maybe Prelude.Text,
    -- | Logical schema name.
    schema :: Prelude.Maybe Prelude.Text,
    -- | Amazon RDS cluster ARN.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | AWS Region for RDS HTTP endpoint.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | Logical database name.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      schema = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      databaseName = Prelude.Nothing
    }

-- | AWS secret store ARN for database credentials.
rdsHttpEndpointConfig_awsSecretStoreArn :: Lens.Lens' RdsHttpEndpointConfig (Prelude.Maybe Prelude.Text)
rdsHttpEndpointConfig_awsSecretStoreArn = Lens.lens (\RdsHttpEndpointConfig' {awsSecretStoreArn} -> awsSecretStoreArn) (\s@RdsHttpEndpointConfig' {} a -> s {awsSecretStoreArn = a} :: RdsHttpEndpointConfig)

-- | Logical schema name.
rdsHttpEndpointConfig_schema :: Lens.Lens' RdsHttpEndpointConfig (Prelude.Maybe Prelude.Text)
rdsHttpEndpointConfig_schema = Lens.lens (\RdsHttpEndpointConfig' {schema} -> schema) (\s@RdsHttpEndpointConfig' {} a -> s {schema = a} :: RdsHttpEndpointConfig)

-- | Amazon RDS cluster ARN.
rdsHttpEndpointConfig_dbClusterIdentifier :: Lens.Lens' RdsHttpEndpointConfig (Prelude.Maybe Prelude.Text)
rdsHttpEndpointConfig_dbClusterIdentifier = Lens.lens (\RdsHttpEndpointConfig' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RdsHttpEndpointConfig' {} a -> s {dbClusterIdentifier = a} :: RdsHttpEndpointConfig)

-- | AWS Region for RDS HTTP endpoint.
rdsHttpEndpointConfig_awsRegion :: Lens.Lens' RdsHttpEndpointConfig (Prelude.Maybe Prelude.Text)
rdsHttpEndpointConfig_awsRegion = Lens.lens (\RdsHttpEndpointConfig' {awsRegion} -> awsRegion) (\s@RdsHttpEndpointConfig' {} a -> s {awsRegion = a} :: RdsHttpEndpointConfig)

-- | Logical database name.
rdsHttpEndpointConfig_databaseName :: Lens.Lens' RdsHttpEndpointConfig (Prelude.Maybe Prelude.Text)
rdsHttpEndpointConfig_databaseName = Lens.lens (\RdsHttpEndpointConfig' {databaseName} -> databaseName) (\s@RdsHttpEndpointConfig' {} a -> s {databaseName = a} :: RdsHttpEndpointConfig)

instance Core.FromJSON RdsHttpEndpointConfig where
  parseJSON =
    Core.withObject
      "RdsHttpEndpointConfig"
      ( \x ->
          RdsHttpEndpointConfig'
            Prelude.<$> (x Core..:? "awsSecretStoreArn")
            Prelude.<*> (x Core..:? "schema")
            Prelude.<*> (x Core..:? "dbClusterIdentifier")
            Prelude.<*> (x Core..:? "awsRegion")
            Prelude.<*> (x Core..:? "databaseName")
      )

instance Prelude.Hashable RdsHttpEndpointConfig

instance Prelude.NFData RdsHttpEndpointConfig

instance Core.ToJSON RdsHttpEndpointConfig where
  toJSON RdsHttpEndpointConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("awsSecretStoreArn" Core..=)
              Prelude.<$> awsSecretStoreArn,
            ("schema" Core..=) Prelude.<$> schema,
            ("dbClusterIdentifier" Core..=)
              Prelude.<$> dbClusterIdentifier,
            ("awsRegion" Core..=) Prelude.<$> awsRegion,
            ("databaseName" Core..=) Prelude.<$> databaseName
          ]
      )
