{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteDevEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified development endpoint.
module Network.AWS.Glue.DeleteDevEndpoint
  ( -- * Creating a request
    DeleteDevEndpoint (..),
    mkDeleteDevEndpoint,

    -- ** Request lenses
    ddeEndpointName,

    -- * Destructuring the response
    DeleteDevEndpointResponse (..),
    mkDeleteDevEndpointResponse,

    -- ** Response lenses
    dderrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDevEndpoint' smart constructor.
newtype DeleteDevEndpoint = DeleteDevEndpoint'
  { -- | The name of the @DevEndpoint@ .
    endpointName :: Types.EndpointName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDevEndpoint' value with any optional fields omitted.
mkDeleteDevEndpoint ::
  -- | 'endpointName'
  Types.EndpointName ->
  DeleteDevEndpoint
mkDeleteDevEndpoint endpointName = DeleteDevEndpoint' {endpointName}

-- | The name of the @DevEndpoint@ .
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeEndpointName :: Lens.Lens' DeleteDevEndpoint Types.EndpointName
ddeEndpointName = Lens.field @"endpointName"
{-# DEPRECATED ddeEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

instance Core.FromJSON DeleteDevEndpoint where
  toJSON DeleteDevEndpoint {..} =
    Core.object
      (Core.catMaybes [Core.Just ("EndpointName" Core..= endpointName)])

instance Core.AWSRequest DeleteDevEndpoint where
  type Rs DeleteDevEndpoint = DeleteDevEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.DeleteDevEndpoint")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDevEndpointResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDevEndpointResponse' smart constructor.
newtype DeleteDevEndpointResponse = DeleteDevEndpointResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDevEndpointResponse' value with any optional fields omitted.
mkDeleteDevEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDevEndpointResponse
mkDeleteDevEndpointResponse responseStatus =
  DeleteDevEndpointResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dderrsResponseStatus :: Lens.Lens' DeleteDevEndpointResponse Core.Int
dderrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dderrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
