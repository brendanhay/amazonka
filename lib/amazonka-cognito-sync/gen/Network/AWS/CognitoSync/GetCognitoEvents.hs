{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.GetCognitoEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the events and the corresponding Lambda functions associated with an identity pool.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.GetCognitoEvents
  ( -- * Creating a request
    GetCognitoEvents (..),
    mkGetCognitoEvents,

    -- ** Request lenses
    gceIdentityPoolId,

    -- * Destructuring the response
    GetCognitoEventsResponse (..),
    mkGetCognitoEventsResponse,

    -- ** Response lenses
    gcerrsEvents,
    gcerrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request for a list of the configured Cognito Events
--
-- /See:/ 'mkGetCognitoEvents' smart constructor.
newtype GetCognitoEvents = GetCognitoEvents'
  { -- | The Cognito Identity Pool ID for the request
    identityPoolId :: Types.IdentityPoolId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCognitoEvents' value with any optional fields omitted.
mkGetCognitoEvents ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  GetCognitoEvents
mkGetCognitoEvents identityPoolId =
  GetCognitoEvents' {identityPoolId}

-- | The Cognito Identity Pool ID for the request
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gceIdentityPoolId :: Lens.Lens' GetCognitoEvents Types.IdentityPoolId
gceIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED gceIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Core.AWSRequest GetCognitoEvents where
  type Rs GetCognitoEvents = GetCognitoEventsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/identitypools/" Core.<> (Core.toText identityPoolId)
                Core.<> ("/events")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCognitoEventsResponse'
            Core.<$> (x Core..:? "Events") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The response from the GetCognitoEvents request
--
-- /See:/ 'mkGetCognitoEventsResponse' smart constructor.
data GetCognitoEventsResponse = GetCognitoEventsResponse'
  { -- | The Cognito Events returned from the GetCognitoEvents request
    events :: Core.Maybe (Core.HashMap Types.CognitoEventType Types.LambdaFunctionArn),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCognitoEventsResponse' value with any optional fields omitted.
mkGetCognitoEventsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCognitoEventsResponse
mkGetCognitoEventsResponse responseStatus =
  GetCognitoEventsResponse' {events = Core.Nothing, responseStatus}

-- | The Cognito Events returned from the GetCognitoEvents request
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcerrsEvents :: Lens.Lens' GetCognitoEventsResponse (Core.Maybe (Core.HashMap Types.CognitoEventType Types.LambdaFunctionArn))
gcerrsEvents = Lens.field @"events"
{-# DEPRECATED gcerrsEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcerrsResponseStatus :: Lens.Lens' GetCognitoEventsResponse Core.Int
gcerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
