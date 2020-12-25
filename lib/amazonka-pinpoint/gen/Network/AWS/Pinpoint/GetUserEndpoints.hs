{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetUserEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the endpoints that are associated with a specific user ID.
module Network.AWS.Pinpoint.GetUserEndpoints
  ( -- * Creating a request
    GetUserEndpoints (..),
    mkGetUserEndpoints,

    -- ** Request lenses
    gueApplicationId,
    gueUserId,

    -- * Destructuring the response
    GetUserEndpointsResponse (..),
    mkGetUserEndpointsResponse,

    -- ** Response lenses
    guerrsEndpointsResponse,
    guerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetUserEndpoints' smart constructor.
data GetUserEndpoints = GetUserEndpoints'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The unique identifier for the user.
    userId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUserEndpoints' value with any optional fields omitted.
mkGetUserEndpoints ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'userId'
  Core.Text ->
  GetUserEndpoints
mkGetUserEndpoints applicationId userId =
  GetUserEndpoints' {applicationId, userId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gueApplicationId :: Lens.Lens' GetUserEndpoints Core.Text
gueApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gueApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gueUserId :: Lens.Lens' GetUserEndpoints Core.Text
gueUserId = Lens.field @"userId"
{-# DEPRECATED gueUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Core.AWSRequest GetUserEndpoints where
  type Rs GetUserEndpoints = GetUserEndpointsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
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
          GetUserEndpointsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetUserEndpointsResponse' smart constructor.
data GetUserEndpointsResponse = GetUserEndpointsResponse'
  { endpointsResponse :: Types.EndpointsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUserEndpointsResponse' value with any optional fields omitted.
mkGetUserEndpointsResponse ::
  -- | 'endpointsResponse'
  Types.EndpointsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetUserEndpointsResponse
mkGetUserEndpointsResponse endpointsResponse responseStatus =
  GetUserEndpointsResponse' {endpointsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpointsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guerrsEndpointsResponse :: Lens.Lens' GetUserEndpointsResponse Types.EndpointsResponse
guerrsEndpointsResponse = Lens.field @"endpointsResponse"
{-# DEPRECATED guerrsEndpointsResponse "Use generic-lens or generic-optics with 'endpointsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guerrsResponseStatus :: Lens.Lens' GetUserEndpointsResponse Core.Int
guerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED guerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
