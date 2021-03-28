{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DynamodbDataSourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.DynamodbDataSourceConfig
  ( DynamodbDataSourceConfig (..)
  -- * Smart constructor
  , mkDynamodbDataSourceConfig
  -- * Lenses
  , ddscTableName
  , ddscAwsRegion
  , ddscDeltaSyncConfig
  , ddscUseCallerCredentials
  , ddscVersioned
  ) where

import qualified Network.AWS.AppSync.Types.DeltaSyncConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon DynamoDB data source configuration.
--
-- /See:/ 'mkDynamodbDataSourceConfig' smart constructor.
data DynamodbDataSourceConfig = DynamodbDataSourceConfig'
  { tableName :: Core.Text
    -- ^ The table name.
  , awsRegion :: Core.Text
    -- ^ The AWS Region.
  , deltaSyncConfig :: Core.Maybe Types.DeltaSyncConfig
    -- ^ The @DeltaSyncConfig@ for a versioned datasource.
  , useCallerCredentials :: Core.Maybe Core.Bool
    -- ^ Set to TRUE to use Amazon Cognito credentials with this data source.
  , versioned :: Core.Maybe Core.Bool
    -- ^ Set to TRUE to use Conflict Detection and Resolution with this data source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DynamodbDataSourceConfig' value with any optional fields omitted.
mkDynamodbDataSourceConfig
    :: Core.Text -- ^ 'tableName'
    -> Core.Text -- ^ 'awsRegion'
    -> DynamodbDataSourceConfig
mkDynamodbDataSourceConfig tableName awsRegion
  = DynamodbDataSourceConfig'{tableName, awsRegion,
                              deltaSyncConfig = Core.Nothing,
                              useCallerCredentials = Core.Nothing, versioned = Core.Nothing}

-- | The table name.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscTableName :: Lens.Lens' DynamodbDataSourceConfig Core.Text
ddscTableName = Lens.field @"tableName"
{-# INLINEABLE ddscTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | The AWS Region.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscAwsRegion :: Lens.Lens' DynamodbDataSourceConfig Core.Text
ddscAwsRegion = Lens.field @"awsRegion"
{-# INLINEABLE ddscAwsRegion #-}
{-# DEPRECATED awsRegion "Use generic-lens or generic-optics with 'awsRegion' instead"  #-}

-- | The @DeltaSyncConfig@ for a versioned datasource.
--
-- /Note:/ Consider using 'deltaSyncConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscDeltaSyncConfig :: Lens.Lens' DynamodbDataSourceConfig (Core.Maybe Types.DeltaSyncConfig)
ddscDeltaSyncConfig = Lens.field @"deltaSyncConfig"
{-# INLINEABLE ddscDeltaSyncConfig #-}
{-# DEPRECATED deltaSyncConfig "Use generic-lens or generic-optics with 'deltaSyncConfig' instead"  #-}

-- | Set to TRUE to use Amazon Cognito credentials with this data source.
--
-- /Note:/ Consider using 'useCallerCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscUseCallerCredentials :: Lens.Lens' DynamodbDataSourceConfig (Core.Maybe Core.Bool)
ddscUseCallerCredentials = Lens.field @"useCallerCredentials"
{-# INLINEABLE ddscUseCallerCredentials #-}
{-# DEPRECATED useCallerCredentials "Use generic-lens or generic-optics with 'useCallerCredentials' instead"  #-}

-- | Set to TRUE to use Conflict Detection and Resolution with this data source.
--
-- /Note:/ Consider using 'versioned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddscVersioned :: Lens.Lens' DynamodbDataSourceConfig (Core.Maybe Core.Bool)
ddscVersioned = Lens.field @"versioned"
{-# INLINEABLE ddscVersioned #-}
{-# DEPRECATED versioned "Use generic-lens or generic-optics with 'versioned' instead"  #-}

instance Core.FromJSON DynamodbDataSourceConfig where
        toJSON DynamodbDataSourceConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("tableName" Core..= tableName),
                  Core.Just ("awsRegion" Core..= awsRegion),
                  ("deltaSyncConfig" Core..=) Core.<$> deltaSyncConfig,
                  ("useCallerCredentials" Core..=) Core.<$> useCallerCredentials,
                  ("versioned" Core..=) Core.<$> versioned])

instance Core.FromJSON DynamodbDataSourceConfig where
        parseJSON
          = Core.withObject "DynamodbDataSourceConfig" Core.$
              \ x ->
                DynamodbDataSourceConfig' Core.<$>
                  (x Core..: "tableName") Core.<*> x Core..: "awsRegion" Core.<*>
                    x Core..:? "deltaSyncConfig"
                    Core.<*> x Core..:? "useCallerCredentials"
                    Core.<*> x Core..:? "versioned"
