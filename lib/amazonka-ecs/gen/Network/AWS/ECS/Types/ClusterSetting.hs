{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ClusterSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ClusterSetting
  ( ClusterSetting (..)
  -- * Smart constructor
  , mkClusterSetting
  -- * Lenses
  , csName
  , csValue
  ) where

import qualified Network.AWS.ECS.Types.ClusterSettingName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The settings to use when creating a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster.
--
-- /See:/ 'mkClusterSetting' smart constructor.
data ClusterSetting = ClusterSetting'
  { name :: Core.Maybe Types.ClusterSettingName
    -- ^ The name of the cluster setting. The only supported value is @containerInsights@ .
  , value :: Core.Maybe Core.Text
    -- ^ The value to set for the cluster setting. The supported values are @enabled@ and @disabled@ . If @enabled@ is specified, CloudWatch Container Insights will be enabled for the cluster, otherwise it will be disabled unless the @containerInsights@ account setting is enabled. If a cluster value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterSetting' value with any optional fields omitted.
mkClusterSetting
    :: ClusterSetting
mkClusterSetting
  = ClusterSetting'{name = Core.Nothing, value = Core.Nothing}

-- | The name of the cluster setting. The only supported value is @containerInsights@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' ClusterSetting (Core.Maybe Types.ClusterSettingName)
csName = Lens.field @"name"
{-# INLINEABLE csName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value to set for the cluster setting. The supported values are @enabled@ and @disabled@ . If @enabled@ is specified, CloudWatch Container Insights will be enabled for the cluster, otherwise it will be disabled unless the @containerInsights@ account setting is enabled. If a cluster value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csValue :: Lens.Lens' ClusterSetting (Core.Maybe Core.Text)
csValue = Lens.field @"value"
{-# INLINEABLE csValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON ClusterSetting where
        toJSON ClusterSetting{..}
          = Core.object
              (Core.catMaybes
                 [("name" Core..=) Core.<$> name, ("value" Core..=) Core.<$> value])

instance Core.FromJSON ClusterSetting where
        parseJSON
          = Core.withObject "ClusterSetting" Core.$
              \ x ->
                ClusterSetting' Core.<$>
                  (x Core..:? "name") Core.<*> x Core..:? "value"
