{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateIndexingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the search configuration.
module Network.AWS.IoT.UpdateIndexingConfiguration
  ( -- * Creating a request
    UpdateIndexingConfiguration (..),
    mkUpdateIndexingConfiguration,

    -- ** Request lenses
    uicThingGroupIndexingConfiguration,
    uicThingIndexingConfiguration,

    -- * Destructuring the response
    UpdateIndexingConfigurationResponse (..),
    mkUpdateIndexingConfigurationResponse,

    -- ** Response lenses
    uicrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateIndexingConfiguration' smart constructor.
data UpdateIndexingConfiguration = UpdateIndexingConfiguration'
  { -- | Thing group indexing configuration.
    thingGroupIndexingConfiguration :: Core.Maybe Types.ThingGroupIndexingConfiguration,
    -- | Thing indexing configuration.
    thingIndexingConfiguration :: Core.Maybe Types.ThingIndexingConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateIndexingConfiguration' value with any optional fields omitted.
mkUpdateIndexingConfiguration ::
  UpdateIndexingConfiguration
mkUpdateIndexingConfiguration =
  UpdateIndexingConfiguration'
    { thingGroupIndexingConfiguration =
        Core.Nothing,
      thingIndexingConfiguration = Core.Nothing
    }

-- | Thing group indexing configuration.
--
-- /Note:/ Consider using 'thingGroupIndexingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicThingGroupIndexingConfiguration :: Lens.Lens' UpdateIndexingConfiguration (Core.Maybe Types.ThingGroupIndexingConfiguration)
uicThingGroupIndexingConfiguration = Lens.field @"thingGroupIndexingConfiguration"
{-# DEPRECATED uicThingGroupIndexingConfiguration "Use generic-lens or generic-optics with 'thingGroupIndexingConfiguration' instead." #-}

-- | Thing indexing configuration.
--
-- /Note:/ Consider using 'thingIndexingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicThingIndexingConfiguration :: Lens.Lens' UpdateIndexingConfiguration (Core.Maybe Types.ThingIndexingConfiguration)
uicThingIndexingConfiguration = Lens.field @"thingIndexingConfiguration"
{-# DEPRECATED uicThingIndexingConfiguration "Use generic-lens or generic-optics with 'thingIndexingConfiguration' instead." #-}

instance Core.FromJSON UpdateIndexingConfiguration where
  toJSON UpdateIndexingConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("thingGroupIndexingConfiguration" Core..=)
              Core.<$> thingGroupIndexingConfiguration,
            ("thingIndexingConfiguration" Core..=)
              Core.<$> thingIndexingConfiguration
          ]
      )

instance Core.AWSRequest UpdateIndexingConfiguration where
  type
    Rs UpdateIndexingConfiguration =
      UpdateIndexingConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/indexing/config",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateIndexingConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateIndexingConfigurationResponse' smart constructor.
newtype UpdateIndexingConfigurationResponse = UpdateIndexingConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateIndexingConfigurationResponse' value with any optional fields omitted.
mkUpdateIndexingConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateIndexingConfigurationResponse
mkUpdateIndexingConfigurationResponse responseStatus =
  UpdateIndexingConfigurationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicrrsResponseStatus :: Lens.Lens' UpdateIndexingConfigurationResponse Core.Int
uicrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uicrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
