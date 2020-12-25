{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the state of a specific instance. Works on one instance at a time.
module Network.AWS.Lightsail.GetInstanceState
  ( -- * Creating a request
    GetInstanceState (..),
    mkGetInstanceState,

    -- ** Request lenses
    gisInstanceName,

    -- * Destructuring the response
    GetInstanceStateResponse (..),
    mkGetInstanceStateResponse,

    -- ** Response lenses
    gisrgrsState,
    gisrgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInstanceState' smart constructor.
newtype GetInstanceState = GetInstanceState'
  { -- | The name of the instance to get state information about.
    instanceName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceState' value with any optional fields omitted.
mkGetInstanceState ::
  -- | 'instanceName'
  Types.ResourceName ->
  GetInstanceState
mkGetInstanceState instanceName = GetInstanceState' {instanceName}

-- | The name of the instance to get state information about.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisInstanceName :: Lens.Lens' GetInstanceState Types.ResourceName
gisInstanceName = Lens.field @"instanceName"
{-# DEPRECATED gisInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Core.FromJSON GetInstanceState where
  toJSON GetInstanceState {..} =
    Core.object
      (Core.catMaybes [Core.Just ("instanceName" Core..= instanceName)])

instance Core.AWSRequest GetInstanceState where
  type Rs GetInstanceState = GetInstanceStateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetInstanceState")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceStateResponse'
            Core.<$> (x Core..:? "state") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetInstanceStateResponse' smart constructor.
data GetInstanceStateResponse = GetInstanceStateResponse'
  { -- | The state of the instance.
    state :: Core.Maybe Types.InstanceState,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceStateResponse' value with any optional fields omitted.
mkGetInstanceStateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInstanceStateResponse
mkGetInstanceStateResponse responseStatus =
  GetInstanceStateResponse' {state = Core.Nothing, responseStatus}

-- | The state of the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrgrsState :: Lens.Lens' GetInstanceStateResponse (Core.Maybe Types.InstanceState)
gisrgrsState = Lens.field @"state"
{-# DEPRECATED gisrgrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrgrsResponseStatus :: Lens.Lens' GetInstanceStateResponse Core.Int
gisrgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gisrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
