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
-- Module      : Amazonka.AppSync.Types.DynamodbDataSourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.DynamodbDataSourceConfig where

import Amazonka.AppSync.Types.DeltaSyncConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon DynamoDB data source configuration.
--
-- /See:/ 'newDynamodbDataSourceConfig' smart constructor.
data DynamodbDataSourceConfig = DynamodbDataSourceConfig'
  { -- | The @DeltaSyncConfig@ for a versioned data source.
    deltaSyncConfig :: Prelude.Maybe DeltaSyncConfig,
    -- | Set to TRUE to use Amazon Cognito credentials with this data source.
    useCallerCredentials :: Prelude.Maybe Prelude.Bool,
    -- | Set to TRUE to use Conflict Detection and Resolution with this data
    -- source.
    versioned :: Prelude.Maybe Prelude.Bool,
    -- | The table name.
    tableName :: Prelude.Text,
    -- | The Amazon Web Services Region.
    awsRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynamodbDataSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deltaSyncConfig', 'dynamodbDataSourceConfig_deltaSyncConfig' - The @DeltaSyncConfig@ for a versioned data source.
--
-- 'useCallerCredentials', 'dynamodbDataSourceConfig_useCallerCredentials' - Set to TRUE to use Amazon Cognito credentials with this data source.
--
-- 'versioned', 'dynamodbDataSourceConfig_versioned' - Set to TRUE to use Conflict Detection and Resolution with this data
-- source.
--
-- 'tableName', 'dynamodbDataSourceConfig_tableName' - The table name.
--
-- 'awsRegion', 'dynamodbDataSourceConfig_awsRegion' - The Amazon Web Services Region.
newDynamodbDataSourceConfig ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'awsRegion'
  Prelude.Text ->
  DynamodbDataSourceConfig
newDynamodbDataSourceConfig pTableName_ pAwsRegion_ =
  DynamodbDataSourceConfig'
    { deltaSyncConfig =
        Prelude.Nothing,
      useCallerCredentials = Prelude.Nothing,
      versioned = Prelude.Nothing,
      tableName = pTableName_,
      awsRegion = pAwsRegion_
    }

-- | The @DeltaSyncConfig@ for a versioned data source.
dynamodbDataSourceConfig_deltaSyncConfig :: Lens.Lens' DynamodbDataSourceConfig (Prelude.Maybe DeltaSyncConfig)
dynamodbDataSourceConfig_deltaSyncConfig = Lens.lens (\DynamodbDataSourceConfig' {deltaSyncConfig} -> deltaSyncConfig) (\s@DynamodbDataSourceConfig' {} a -> s {deltaSyncConfig = a} :: DynamodbDataSourceConfig)

-- | Set to TRUE to use Amazon Cognito credentials with this data source.
dynamodbDataSourceConfig_useCallerCredentials :: Lens.Lens' DynamodbDataSourceConfig (Prelude.Maybe Prelude.Bool)
dynamodbDataSourceConfig_useCallerCredentials = Lens.lens (\DynamodbDataSourceConfig' {useCallerCredentials} -> useCallerCredentials) (\s@DynamodbDataSourceConfig' {} a -> s {useCallerCredentials = a} :: DynamodbDataSourceConfig)

-- | Set to TRUE to use Conflict Detection and Resolution with this data
-- source.
dynamodbDataSourceConfig_versioned :: Lens.Lens' DynamodbDataSourceConfig (Prelude.Maybe Prelude.Bool)
dynamodbDataSourceConfig_versioned = Lens.lens (\DynamodbDataSourceConfig' {versioned} -> versioned) (\s@DynamodbDataSourceConfig' {} a -> s {versioned = a} :: DynamodbDataSourceConfig)

-- | The table name.
dynamodbDataSourceConfig_tableName :: Lens.Lens' DynamodbDataSourceConfig Prelude.Text
dynamodbDataSourceConfig_tableName = Lens.lens (\DynamodbDataSourceConfig' {tableName} -> tableName) (\s@DynamodbDataSourceConfig' {} a -> s {tableName = a} :: DynamodbDataSourceConfig)

-- | The Amazon Web Services Region.
dynamodbDataSourceConfig_awsRegion :: Lens.Lens' DynamodbDataSourceConfig Prelude.Text
dynamodbDataSourceConfig_awsRegion = Lens.lens (\DynamodbDataSourceConfig' {awsRegion} -> awsRegion) (\s@DynamodbDataSourceConfig' {} a -> s {awsRegion = a} :: DynamodbDataSourceConfig)

instance Data.FromJSON DynamodbDataSourceConfig where
  parseJSON =
    Data.withObject
      "DynamodbDataSourceConfig"
      ( \x ->
          DynamodbDataSourceConfig'
            Prelude.<$> (x Data..:? "deltaSyncConfig")
            Prelude.<*> (x Data..:? "useCallerCredentials")
            Prelude.<*> (x Data..:? "versioned")
            Prelude.<*> (x Data..: "tableName")
            Prelude.<*> (x Data..: "awsRegion")
      )

instance Prelude.Hashable DynamodbDataSourceConfig where
  hashWithSalt _salt DynamodbDataSourceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` deltaSyncConfig
      `Prelude.hashWithSalt` useCallerCredentials
      `Prelude.hashWithSalt` versioned
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` awsRegion

instance Prelude.NFData DynamodbDataSourceConfig where
  rnf DynamodbDataSourceConfig' {..} =
    Prelude.rnf deltaSyncConfig `Prelude.seq`
      Prelude.rnf useCallerCredentials `Prelude.seq`
        Prelude.rnf versioned `Prelude.seq`
          Prelude.rnf tableName `Prelude.seq`
            Prelude.rnf awsRegion

instance Data.ToJSON DynamodbDataSourceConfig where
  toJSON DynamodbDataSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deltaSyncConfig" Data..=)
              Prelude.<$> deltaSyncConfig,
            ("useCallerCredentials" Data..=)
              Prelude.<$> useCallerCredentials,
            ("versioned" Data..=) Prelude.<$> versioned,
            Prelude.Just ("tableName" Data..= tableName),
            Prelude.Just ("awsRegion" Data..= awsRegion)
          ]
      )
