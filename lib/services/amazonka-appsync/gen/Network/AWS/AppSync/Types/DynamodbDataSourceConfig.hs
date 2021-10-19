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
-- Module      : Network.AWS.AppSync.Types.DynamodbDataSourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.DynamodbDataSourceConfig where

import Network.AWS.AppSync.Types.DeltaSyncConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Amazon DynamoDB data source configuration.
--
-- /See:/ 'newDynamodbDataSourceConfig' smart constructor.
data DynamodbDataSourceConfig = DynamodbDataSourceConfig'
  { -- | Set to TRUE to use Conflict Detection and Resolution with this data
    -- source.
    versioned :: Prelude.Maybe Prelude.Bool,
    -- | Set to TRUE to use Amazon Cognito credentials with this data source.
    useCallerCredentials :: Prelude.Maybe Prelude.Bool,
    -- | The @DeltaSyncConfig@ for a versioned datasource.
    deltaSyncConfig :: Prelude.Maybe DeltaSyncConfig,
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
-- 'versioned', 'dynamodbDataSourceConfig_versioned' - Set to TRUE to use Conflict Detection and Resolution with this data
-- source.
--
-- 'useCallerCredentials', 'dynamodbDataSourceConfig_useCallerCredentials' - Set to TRUE to use Amazon Cognito credentials with this data source.
--
-- 'deltaSyncConfig', 'dynamodbDataSourceConfig_deltaSyncConfig' - The @DeltaSyncConfig@ for a versioned datasource.
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
    { versioned =
        Prelude.Nothing,
      useCallerCredentials = Prelude.Nothing,
      deltaSyncConfig = Prelude.Nothing,
      tableName = pTableName_,
      awsRegion = pAwsRegion_
    }

-- | Set to TRUE to use Conflict Detection and Resolution with this data
-- source.
dynamodbDataSourceConfig_versioned :: Lens.Lens' DynamodbDataSourceConfig (Prelude.Maybe Prelude.Bool)
dynamodbDataSourceConfig_versioned = Lens.lens (\DynamodbDataSourceConfig' {versioned} -> versioned) (\s@DynamodbDataSourceConfig' {} a -> s {versioned = a} :: DynamodbDataSourceConfig)

-- | Set to TRUE to use Amazon Cognito credentials with this data source.
dynamodbDataSourceConfig_useCallerCredentials :: Lens.Lens' DynamodbDataSourceConfig (Prelude.Maybe Prelude.Bool)
dynamodbDataSourceConfig_useCallerCredentials = Lens.lens (\DynamodbDataSourceConfig' {useCallerCredentials} -> useCallerCredentials) (\s@DynamodbDataSourceConfig' {} a -> s {useCallerCredentials = a} :: DynamodbDataSourceConfig)

-- | The @DeltaSyncConfig@ for a versioned datasource.
dynamodbDataSourceConfig_deltaSyncConfig :: Lens.Lens' DynamodbDataSourceConfig (Prelude.Maybe DeltaSyncConfig)
dynamodbDataSourceConfig_deltaSyncConfig = Lens.lens (\DynamodbDataSourceConfig' {deltaSyncConfig} -> deltaSyncConfig) (\s@DynamodbDataSourceConfig' {} a -> s {deltaSyncConfig = a} :: DynamodbDataSourceConfig)

-- | The table name.
dynamodbDataSourceConfig_tableName :: Lens.Lens' DynamodbDataSourceConfig Prelude.Text
dynamodbDataSourceConfig_tableName = Lens.lens (\DynamodbDataSourceConfig' {tableName} -> tableName) (\s@DynamodbDataSourceConfig' {} a -> s {tableName = a} :: DynamodbDataSourceConfig)

-- | The Amazon Web Services Region.
dynamodbDataSourceConfig_awsRegion :: Lens.Lens' DynamodbDataSourceConfig Prelude.Text
dynamodbDataSourceConfig_awsRegion = Lens.lens (\DynamodbDataSourceConfig' {awsRegion} -> awsRegion) (\s@DynamodbDataSourceConfig' {} a -> s {awsRegion = a} :: DynamodbDataSourceConfig)

instance Core.FromJSON DynamodbDataSourceConfig where
  parseJSON =
    Core.withObject
      "DynamodbDataSourceConfig"
      ( \x ->
          DynamodbDataSourceConfig'
            Prelude.<$> (x Core..:? "versioned")
            Prelude.<*> (x Core..:? "useCallerCredentials")
            Prelude.<*> (x Core..:? "deltaSyncConfig")
            Prelude.<*> (x Core..: "tableName")
            Prelude.<*> (x Core..: "awsRegion")
      )

instance Prelude.Hashable DynamodbDataSourceConfig

instance Prelude.NFData DynamodbDataSourceConfig

instance Core.ToJSON DynamodbDataSourceConfig where
  toJSON DynamodbDataSourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("versioned" Core..=) Prelude.<$> versioned,
            ("useCallerCredentials" Core..=)
              Prelude.<$> useCallerCredentials,
            ("deltaSyncConfig" Core..=)
              Prelude.<$> deltaSyncConfig,
            Prelude.Just ("tableName" Core..= tableName),
            Prelude.Just ("awsRegion" Core..= awsRegion)
          ]
      )
