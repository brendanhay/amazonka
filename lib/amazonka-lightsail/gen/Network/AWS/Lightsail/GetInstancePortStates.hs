{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstancePortStates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the firewall port states for a specific Amazon Lightsail instance, the IP addresses allowed to connect to the instance through the ports, and the protocol.
module Network.AWS.Lightsail.GetInstancePortStates
  ( -- * Creating a request
    GetInstancePortStates (..),
    mkGetInstancePortStates,

    -- ** Request lenses
    gipsInstanceName,

    -- * Destructuring the response
    GetInstancePortStatesResponse (..),
    mkGetInstancePortStatesResponse,

    -- ** Response lenses
    gipsrrsPortStates,
    gipsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInstancePortStates' smart constructor.
newtype GetInstancePortStates = GetInstancePortStates'
  { -- | The name of the instance for which to return firewall port states.
    instanceName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstancePortStates' value with any optional fields omitted.
mkGetInstancePortStates ::
  -- | 'instanceName'
  Types.ResourceName ->
  GetInstancePortStates
mkGetInstancePortStates instanceName =
  GetInstancePortStates' {instanceName}

-- | The name of the instance for which to return firewall port states.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsInstanceName :: Lens.Lens' GetInstancePortStates Types.ResourceName
gipsInstanceName = Lens.field @"instanceName"
{-# DEPRECATED gipsInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Core.FromJSON GetInstancePortStates where
  toJSON GetInstancePortStates {..} =
    Core.object
      (Core.catMaybes [Core.Just ("instanceName" Core..= instanceName)])

instance Core.AWSRequest GetInstancePortStates where
  type Rs GetInstancePortStates = GetInstancePortStatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.GetInstancePortStates")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstancePortStatesResponse'
            Core.<$> (x Core..:? "portStates") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetInstancePortStatesResponse' smart constructor.
data GetInstancePortStatesResponse = GetInstancePortStatesResponse'
  { -- | An array of objects that describe the firewall port states for the specified instance.
    portStates :: Core.Maybe [Types.InstancePortState],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstancePortStatesResponse' value with any optional fields omitted.
mkGetInstancePortStatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInstancePortStatesResponse
mkGetInstancePortStatesResponse responseStatus =
  GetInstancePortStatesResponse'
    { portStates = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the firewall port states for the specified instance.
--
-- /Note:/ Consider using 'portStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsrrsPortStates :: Lens.Lens' GetInstancePortStatesResponse (Core.Maybe [Types.InstancePortState])
gipsrrsPortStates = Lens.field @"portStates"
{-# DEPRECATED gipsrrsPortStates "Use generic-lens or generic-optics with 'portStates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsrrsResponseStatus :: Lens.Lens' GetInstancePortStatesResponse Core.Int
gipsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gipsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
