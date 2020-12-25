{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateClusterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings to use for a cluster.
module Network.AWS.ECS.UpdateClusterSettings
  ( -- * Creating a request
    UpdateClusterSettings (..),
    mkUpdateClusterSettings,

    -- ** Request lenses
    ucsCluster,
    ucsSettings,

    -- * Destructuring the response
    UpdateClusterSettingsResponse (..),
    mkUpdateClusterSettingsResponse,

    -- ** Response lenses
    ucsrrsCluster,
    ucsrrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateClusterSettings' smart constructor.
data UpdateClusterSettings = UpdateClusterSettings'
  { -- | The name of the cluster to modify the settings for.
    cluster :: Types.String,
    -- | The setting to use by default for a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster. If this value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
    settings :: [Types.ClusterSetting]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateClusterSettings' value with any optional fields omitted.
mkUpdateClusterSettings ::
  -- | 'cluster'
  Types.String ->
  UpdateClusterSettings
mkUpdateClusterSettings cluster =
  UpdateClusterSettings' {cluster, settings = Core.mempty}

-- | The name of the cluster to modify the settings for.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsCluster :: Lens.Lens' UpdateClusterSettings Types.String
ucsCluster = Lens.field @"cluster"
{-# DEPRECATED ucsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The setting to use by default for a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster. If this value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsSettings :: Lens.Lens' UpdateClusterSettings [Types.ClusterSetting]
ucsSettings = Lens.field @"settings"
{-# DEPRECATED ucsSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

instance Core.FromJSON UpdateClusterSettings where
  toJSON UpdateClusterSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("cluster" Core..= cluster),
            Core.Just ("settings" Core..= settings)
          ]
      )

instance Core.AWSRequest UpdateClusterSettings where
  type Rs UpdateClusterSettings = UpdateClusterSettingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.UpdateClusterSettings"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateClusterSettingsResponse'
            Core.<$> (x Core..:? "cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateClusterSettingsResponse' smart constructor.
data UpdateClusterSettingsResponse = UpdateClusterSettingsResponse'
  { cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateClusterSettingsResponse' value with any optional fields omitted.
mkUpdateClusterSettingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateClusterSettingsResponse
mkUpdateClusterSettingsResponse responseStatus =
  UpdateClusterSettingsResponse'
    { cluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrrsCluster :: Lens.Lens' UpdateClusterSettingsResponse (Core.Maybe Types.Cluster)
ucsrrsCluster = Lens.field @"cluster"
{-# DEPRECATED ucsrrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrrsResponseStatus :: Lens.Lens' UpdateClusterSettingsResponse Core.Int
ucsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
