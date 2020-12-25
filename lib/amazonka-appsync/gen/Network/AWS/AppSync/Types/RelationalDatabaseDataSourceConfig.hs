{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
  ( RelationalDatabaseDataSourceConfig (..),

    -- * Smart constructor
    mkRelationalDatabaseDataSourceConfig,

    -- * Lenses
    rddscRdsHttpEndpointConfig,
    rddscRelationalDatabaseSourceType,
  )
where

import qualified Network.AWS.AppSync.Types.RdsHttpEndpointConfig as Types
import qualified Network.AWS.AppSync.Types.RelationalDatabaseSourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a relational database data source configuration.
--
-- /See:/ 'mkRelationalDatabaseDataSourceConfig' smart constructor.
data RelationalDatabaseDataSourceConfig = RelationalDatabaseDataSourceConfig'
  { -- | Amazon RDS HTTP endpoint settings.
    rdsHttpEndpointConfig :: Core.Maybe Types.RdsHttpEndpointConfig,
    -- | Source type for the relational database.
    --
    --
    --     * __RDS_HTTP_ENDPOINT__ : The relational database source type is an Amazon RDS HTTP endpoint.
    relationalDatabaseSourceType :: Core.Maybe Types.RelationalDatabaseSourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RelationalDatabaseDataSourceConfig' value with any optional fields omitted.
mkRelationalDatabaseDataSourceConfig ::
  RelationalDatabaseDataSourceConfig
mkRelationalDatabaseDataSourceConfig =
  RelationalDatabaseDataSourceConfig'
    { rdsHttpEndpointConfig =
        Core.Nothing,
      relationalDatabaseSourceType = Core.Nothing
    }

-- | Amazon RDS HTTP endpoint settings.
--
-- /Note:/ Consider using 'rdsHttpEndpointConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddscRdsHttpEndpointConfig :: Lens.Lens' RelationalDatabaseDataSourceConfig (Core.Maybe Types.RdsHttpEndpointConfig)
rddscRdsHttpEndpointConfig = Lens.field @"rdsHttpEndpointConfig"
{-# DEPRECATED rddscRdsHttpEndpointConfig "Use generic-lens or generic-optics with 'rdsHttpEndpointConfig' instead." #-}

-- | Source type for the relational database.
--
--
--     * __RDS_HTTP_ENDPOINT__ : The relational database source type is an Amazon RDS HTTP endpoint.
--
--
--
-- /Note:/ Consider using 'relationalDatabaseSourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddscRelationalDatabaseSourceType :: Lens.Lens' RelationalDatabaseDataSourceConfig (Core.Maybe Types.RelationalDatabaseSourceType)
rddscRelationalDatabaseSourceType = Lens.field @"relationalDatabaseSourceType"
{-# DEPRECATED rddscRelationalDatabaseSourceType "Use generic-lens or generic-optics with 'relationalDatabaseSourceType' instead." #-}

instance Core.FromJSON RelationalDatabaseDataSourceConfig where
  toJSON RelationalDatabaseDataSourceConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("rdsHttpEndpointConfig" Core..=) Core.<$> rdsHttpEndpointConfig,
            ("relationalDatabaseSourceType" Core..=)
              Core.<$> relationalDatabaseSourceType
          ]
      )

instance Core.FromJSON RelationalDatabaseDataSourceConfig where
  parseJSON =
    Core.withObject "RelationalDatabaseDataSourceConfig" Core.$
      \x ->
        RelationalDatabaseDataSourceConfig'
          Core.<$> (x Core..:? "rdsHttpEndpointConfig")
          Core.<*> (x Core..:? "relationalDatabaseSourceType")
