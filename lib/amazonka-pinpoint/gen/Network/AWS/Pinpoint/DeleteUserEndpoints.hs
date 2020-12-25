{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteUserEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all the endpoints that are associated with a specific user ID.
module Network.AWS.Pinpoint.DeleteUserEndpoints
  ( -- * Creating a request
    DeleteUserEndpoints (..),
    mkDeleteUserEndpoints,

    -- ** Request lenses
    dueApplicationId,
    dueUserId,

    -- * Destructuring the response
    DeleteUserEndpointsResponse (..),
    mkDeleteUserEndpointsResponse,

    -- ** Response lenses
    duerrsEndpointsResponse,
    duerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUserEndpoints' smart constructor.
data DeleteUserEndpoints = DeleteUserEndpoints'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The unique identifier for the user.
    userId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserEndpoints' value with any optional fields omitted.
mkDeleteUserEndpoints ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'userId'
  Core.Text ->
  DeleteUserEndpoints
mkDeleteUserEndpoints applicationId userId =
  DeleteUserEndpoints' {applicationId, userId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dueApplicationId :: Lens.Lens' DeleteUserEndpoints Core.Text
dueApplicationId = Lens.field @"applicationId"
{-# DEPRECATED dueApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dueUserId :: Lens.Lens' DeleteUserEndpoints Core.Text
dueUserId = Lens.field @"userId"
{-# DEPRECATED dueUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Core.AWSRequest DeleteUserEndpoints where
  type Rs DeleteUserEndpoints = DeleteUserEndpointsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/users/")
                Core.<> (Core.toText userId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteUserEndpointsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteUserEndpointsResponse' smart constructor.
data DeleteUserEndpointsResponse = DeleteUserEndpointsResponse'
  { endpointsResponse :: Types.EndpointsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserEndpointsResponse' value with any optional fields omitted.
mkDeleteUserEndpointsResponse ::
  -- | 'endpointsResponse'
  Types.EndpointsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteUserEndpointsResponse
mkDeleteUserEndpointsResponse endpointsResponse responseStatus =
  DeleteUserEndpointsResponse' {endpointsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpointsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duerrsEndpointsResponse :: Lens.Lens' DeleteUserEndpointsResponse Types.EndpointsResponse
duerrsEndpointsResponse = Lens.field @"endpointsResponse"
{-# DEPRECATED duerrsEndpointsResponse "Use generic-lens or generic-optics with 'endpointsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duerrsResponseStatus :: Lens.Lens' DeleteUserEndpointsResponse Core.Int
duerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED duerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
