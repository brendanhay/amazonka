{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DynamodbDataSourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.DynamodbDataSourceConfig
  ( DynamodbDataSourceConfig (..),

    -- * Smart constructor
    mkDynamodbDataSourceConfig,

    -- * Lenses
    ddscTableName,
    ddscAwsRegion,
    ddscDeltaSyncConfig,
    ddscUseCallerCredentials,
    ddscVersioned,
  )
where

import qualified Network.AWS.AppSync.Types.DeltaSyncConfig as Types
import qualified Network.AWS.AppSync.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon DynamoDB data source configuration.
--
-- /See:/ 'mkDynamodbDataSourceConfig' smart constructor.
data DynamodbDataSourceConfig = DynamodbDataSourceConfig'
  { -- | The table name.
    tableName :: Types.String,
    -- | The AWS Region.
    awsRegion :: Types.String,
    -- | The @DeltaSyncConfig@ for a versioned datasource.
    deltaSyncConfig :: Core.Maybe Types.DeltaSyncConfig,
    -- | Set to TRUE to use Amazon Cognito credentials with this data source.
    useCallerCredentials :: Core.Maybe Core.Bool,
    -- | Set to TRUE to use Conflict Detection and Resolution with this data source.
    versioned :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DynamodbDataSourceConfig' value with any optional fields omitted.
mkDynamodbDataSourceConfig ::
  -- | 'tableName'
  Types.String ->
  -- | 'awsRegion'
  Types.String ->
  DynamodbDataSourceConfig
mkDynamodbDataSourceConfig tableName awsRegion =
  DynamodbDataSourceConfig'
    { tableName,
      awsRegion,
      deltaSyncConfig = Core.Nothing,
      useCallerCredentials = Core.Nothing,
      versioned = Core.Nothing
    }

-- | The table name.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscTableName :: Lens.Lens' DynamodbDataSourceConfig Types.String
ddscTableName = Lens.field @"tableName"
{-# DEPRECATED ddscTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The AWS Region.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscAwsRegion :: Lens.Lens' DynamodbDataSourceConfig Types.String
ddscAwsRegion = Lens.field @"awsRegion"
{-# DEPRECATED ddscAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

-- | The @DeltaSyncConfig@ for a versioned datasource.
--
-- /Note:/ Consider using 'deltaSyncConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscDeltaSyncConfig :: Lens.Lens' DynamodbDataSourceConfig (Core.Maybe Types.DeltaSyncConfig)
ddscDeltaSyncConfig = Lens.field @"deltaSyncConfig"
{-# DEPRECATED ddscDeltaSyncConfig "Use generic-lens or generic-optics with 'deltaSyncConfig' instead." #-}

-- | Set to TRUE to use Amazon Cognito credentials with this data source.
--
-- /Note:/ Consider using 'useCallerCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscUseCallerCredentials :: Lens.Lens' DynamodbDataSourceConfig (Core.Maybe Core.Bool)
ddscUseCallerCredentials = Lens.field @"useCallerCredentials"
{-# DEPRECATED ddscUseCallerCredentials "Use generic-lens or generic-optics with 'useCallerCredentials' instead." #-}

-- | Set to TRUE to use Conflict Detection and Resolution with this data source.
--
-- /Note:/ Consider using 'versioned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscVersioned :: Lens.Lens' DynamodbDataSourceConfig (Core.Maybe Core.Bool)
ddscVersioned = Lens.field @"versioned"
{-# DEPRECATED ddscVersioned "Use generic-lens or generic-optics with 'versioned' instead." #-}

instance Core.FromJSON DynamodbDataSourceConfig where
  toJSON DynamodbDataSourceConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("tableName" Core..= tableName),
            Core.Just ("awsRegion" Core..= awsRegion),
            ("deltaSyncConfig" Core..=) Core.<$> deltaSyncConfig,
            ("useCallerCredentials" Core..=) Core.<$> useCallerCredentials,
            ("versioned" Core..=) Core.<$> versioned
          ]
      )

instance Core.FromJSON DynamodbDataSourceConfig where
  parseJSON =
    Core.withObject "DynamodbDataSourceConfig" Core.$
      \x ->
        DynamodbDataSourceConfig'
          Core.<$> (x Core..: "tableName")
          Core.<*> (x Core..: "awsRegion")
          Core.<*> (x Core..:? "deltaSyncConfig")
          Core.<*> (x Core..:? "useCallerCredentials")
          Core.<*> (x Core..:? "versioned")
