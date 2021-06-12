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

-- | Describes an Amazon DynamoDB data source configuration.
--
-- /See:/ 'newDynamodbDataSourceConfig' smart constructor.
data DynamodbDataSourceConfig = DynamodbDataSourceConfig'
  { -- | Set to TRUE to use Amazon Cognito credentials with this data source.
    useCallerCredentials :: Core.Maybe Core.Bool,
    -- | Set to TRUE to use Conflict Detection and Resolution with this data
    -- source.
    versioned :: Core.Maybe Core.Bool,
    -- | The @DeltaSyncConfig@ for a versioned datasource.
    deltaSyncConfig :: Core.Maybe DeltaSyncConfig,
    -- | The table name.
    tableName :: Core.Text,
    -- | The AWS Region.
    awsRegion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DynamodbDataSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useCallerCredentials', 'dynamodbDataSourceConfig_useCallerCredentials' - Set to TRUE to use Amazon Cognito credentials with this data source.
--
-- 'versioned', 'dynamodbDataSourceConfig_versioned' - Set to TRUE to use Conflict Detection and Resolution with this data
-- source.
--
-- 'deltaSyncConfig', 'dynamodbDataSourceConfig_deltaSyncConfig' - The @DeltaSyncConfig@ for a versioned datasource.
--
-- 'tableName', 'dynamodbDataSourceConfig_tableName' - The table name.
--
-- 'awsRegion', 'dynamodbDataSourceConfig_awsRegion' - The AWS Region.
newDynamodbDataSourceConfig ::
  -- | 'tableName'
  Core.Text ->
  -- | 'awsRegion'
  Core.Text ->
  DynamodbDataSourceConfig
newDynamodbDataSourceConfig pTableName_ pAwsRegion_ =
  DynamodbDataSourceConfig'
    { useCallerCredentials =
        Core.Nothing,
      versioned = Core.Nothing,
      deltaSyncConfig = Core.Nothing,
      tableName = pTableName_,
      awsRegion = pAwsRegion_
    }

-- | Set to TRUE to use Amazon Cognito credentials with this data source.
dynamodbDataSourceConfig_useCallerCredentials :: Lens.Lens' DynamodbDataSourceConfig (Core.Maybe Core.Bool)
dynamodbDataSourceConfig_useCallerCredentials = Lens.lens (\DynamodbDataSourceConfig' {useCallerCredentials} -> useCallerCredentials) (\s@DynamodbDataSourceConfig' {} a -> s {useCallerCredentials = a} :: DynamodbDataSourceConfig)

-- | Set to TRUE to use Conflict Detection and Resolution with this data
-- source.
dynamodbDataSourceConfig_versioned :: Lens.Lens' DynamodbDataSourceConfig (Core.Maybe Core.Bool)
dynamodbDataSourceConfig_versioned = Lens.lens (\DynamodbDataSourceConfig' {versioned} -> versioned) (\s@DynamodbDataSourceConfig' {} a -> s {versioned = a} :: DynamodbDataSourceConfig)

-- | The @DeltaSyncConfig@ for a versioned datasource.
dynamodbDataSourceConfig_deltaSyncConfig :: Lens.Lens' DynamodbDataSourceConfig (Core.Maybe DeltaSyncConfig)
dynamodbDataSourceConfig_deltaSyncConfig = Lens.lens (\DynamodbDataSourceConfig' {deltaSyncConfig} -> deltaSyncConfig) (\s@DynamodbDataSourceConfig' {} a -> s {deltaSyncConfig = a} :: DynamodbDataSourceConfig)

-- | The table name.
dynamodbDataSourceConfig_tableName :: Lens.Lens' DynamodbDataSourceConfig Core.Text
dynamodbDataSourceConfig_tableName = Lens.lens (\DynamodbDataSourceConfig' {tableName} -> tableName) (\s@DynamodbDataSourceConfig' {} a -> s {tableName = a} :: DynamodbDataSourceConfig)

-- | The AWS Region.
dynamodbDataSourceConfig_awsRegion :: Lens.Lens' DynamodbDataSourceConfig Core.Text
dynamodbDataSourceConfig_awsRegion = Lens.lens (\DynamodbDataSourceConfig' {awsRegion} -> awsRegion) (\s@DynamodbDataSourceConfig' {} a -> s {awsRegion = a} :: DynamodbDataSourceConfig)

instance Core.FromJSON DynamodbDataSourceConfig where
  parseJSON =
    Core.withObject
      "DynamodbDataSourceConfig"
      ( \x ->
          DynamodbDataSourceConfig'
            Core.<$> (x Core..:? "useCallerCredentials")
            Core.<*> (x Core..:? "versioned")
            Core.<*> (x Core..:? "deltaSyncConfig")
            Core.<*> (x Core..: "tableName")
            Core.<*> (x Core..: "awsRegion")
      )

instance Core.Hashable DynamodbDataSourceConfig

instance Core.NFData DynamodbDataSourceConfig

instance Core.ToJSON DynamodbDataSourceConfig where
  toJSON DynamodbDataSourceConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("useCallerCredentials" Core..=)
              Core.<$> useCallerCredentials,
            ("versioned" Core..=) Core.<$> versioned,
            ("deltaSyncConfig" Core..=) Core.<$> deltaSyncConfig,
            Core.Just ("tableName" Core..= tableName),
            Core.Just ("awsRegion" Core..= awsRegion)
          ]
      )
