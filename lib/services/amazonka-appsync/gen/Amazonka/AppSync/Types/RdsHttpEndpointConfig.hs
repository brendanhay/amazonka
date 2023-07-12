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
-- Module      : Amazonka.AppSync.Types.RdsHttpEndpointConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.RdsHttpEndpointConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Relational Database Service (Amazon RDS) HTTP endpoint
-- configuration.
--
-- /See:/ 'newRdsHttpEndpointConfig' smart constructor.
data RdsHttpEndpointConfig = RdsHttpEndpointConfig'
  { -- | Amazon Web Services Region for Amazon RDS HTTP endpoint.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | Amazon Web Services secret store Amazon Resource Name (ARN) for database
    -- credentials.
    awsSecretStoreArn :: Prelude.Maybe Prelude.Text,
    -- | Logical database name.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Amazon RDS cluster Amazon Resource Name (ARN).
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Logical schema name.
    schema :: Prelude.Maybe Prelude.Text
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
-- 'awsRegion', 'rdsHttpEndpointConfig_awsRegion' - Amazon Web Services Region for Amazon RDS HTTP endpoint.
--
-- 'awsSecretStoreArn', 'rdsHttpEndpointConfig_awsSecretStoreArn' - Amazon Web Services secret store Amazon Resource Name (ARN) for database
-- credentials.
--
-- 'databaseName', 'rdsHttpEndpointConfig_databaseName' - Logical database name.
--
-- 'dbClusterIdentifier', 'rdsHttpEndpointConfig_dbClusterIdentifier' - Amazon RDS cluster Amazon Resource Name (ARN).
--
-- 'schema', 'rdsHttpEndpointConfig_schema' - Logical schema name.
newRdsHttpEndpointConfig ::
  RdsHttpEndpointConfig
newRdsHttpEndpointConfig =
  RdsHttpEndpointConfig'
    { awsRegion = Prelude.Nothing,
      awsSecretStoreArn = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      schema = Prelude.Nothing
    }

-- | Amazon Web Services Region for Amazon RDS HTTP endpoint.
rdsHttpEndpointConfig_awsRegion :: Lens.Lens' RdsHttpEndpointConfig (Prelude.Maybe Prelude.Text)
rdsHttpEndpointConfig_awsRegion = Lens.lens (\RdsHttpEndpointConfig' {awsRegion} -> awsRegion) (\s@RdsHttpEndpointConfig' {} a -> s {awsRegion = a} :: RdsHttpEndpointConfig)

-- | Amazon Web Services secret store Amazon Resource Name (ARN) for database
-- credentials.
rdsHttpEndpointConfig_awsSecretStoreArn :: Lens.Lens' RdsHttpEndpointConfig (Prelude.Maybe Prelude.Text)
rdsHttpEndpointConfig_awsSecretStoreArn = Lens.lens (\RdsHttpEndpointConfig' {awsSecretStoreArn} -> awsSecretStoreArn) (\s@RdsHttpEndpointConfig' {} a -> s {awsSecretStoreArn = a} :: RdsHttpEndpointConfig)

-- | Logical database name.
rdsHttpEndpointConfig_databaseName :: Lens.Lens' RdsHttpEndpointConfig (Prelude.Maybe Prelude.Text)
rdsHttpEndpointConfig_databaseName = Lens.lens (\RdsHttpEndpointConfig' {databaseName} -> databaseName) (\s@RdsHttpEndpointConfig' {} a -> s {databaseName = a} :: RdsHttpEndpointConfig)

-- | Amazon RDS cluster Amazon Resource Name (ARN).
rdsHttpEndpointConfig_dbClusterIdentifier :: Lens.Lens' RdsHttpEndpointConfig (Prelude.Maybe Prelude.Text)
rdsHttpEndpointConfig_dbClusterIdentifier = Lens.lens (\RdsHttpEndpointConfig' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RdsHttpEndpointConfig' {} a -> s {dbClusterIdentifier = a} :: RdsHttpEndpointConfig)

-- | Logical schema name.
rdsHttpEndpointConfig_schema :: Lens.Lens' RdsHttpEndpointConfig (Prelude.Maybe Prelude.Text)
rdsHttpEndpointConfig_schema = Lens.lens (\RdsHttpEndpointConfig' {schema} -> schema) (\s@RdsHttpEndpointConfig' {} a -> s {schema = a} :: RdsHttpEndpointConfig)

instance Data.FromJSON RdsHttpEndpointConfig where
  parseJSON =
    Data.withObject
      "RdsHttpEndpointConfig"
      ( \x ->
          RdsHttpEndpointConfig'
            Prelude.<$> (x Data..:? "awsRegion")
            Prelude.<*> (x Data..:? "awsSecretStoreArn")
            Prelude.<*> (x Data..:? "databaseName")
            Prelude.<*> (x Data..:? "dbClusterIdentifier")
            Prelude.<*> (x Data..:? "schema")
      )

instance Prelude.Hashable RdsHttpEndpointConfig where
  hashWithSalt _salt RdsHttpEndpointConfig' {..} =
    _salt
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` awsSecretStoreArn
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` schema

instance Prelude.NFData RdsHttpEndpointConfig where
  rnf RdsHttpEndpointConfig' {..} =
    Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf awsSecretStoreArn
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf schema

instance Data.ToJSON RdsHttpEndpointConfig where
  toJSON RdsHttpEndpointConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsRegion" Data..=) Prelude.<$> awsRegion,
            ("awsSecretStoreArn" Data..=)
              Prelude.<$> awsSecretStoreArn,
            ("databaseName" Data..=) Prelude.<$> databaseName,
            ("dbClusterIdentifier" Data..=)
              Prelude.<$> dbClusterIdentifier,
            ("schema" Data..=) Prelude.<$> schema
          ]
      )
