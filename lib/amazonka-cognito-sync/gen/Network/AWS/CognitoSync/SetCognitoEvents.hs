{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.SetCognitoEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the AWS Lambda function for a given event type for an identity pool. This request only updates the key/value pair specified. Other key/values pairs are not updated. To remove a key value pair, pass a empty value for the particular key.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.SetCognitoEvents
  ( -- * Creating a request
    SetCognitoEvents (..),
    mkSetCognitoEvents,

    -- ** Request lenses
    sceIdentityPoolId,
    sceEvents,

    -- * Destructuring the response
    SetCognitoEventsResponse (..),
    mkSetCognitoEventsResponse,
  )
where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to configure Cognito Events"
--
-- /See:/ 'mkSetCognitoEvents' smart constructor.
data SetCognitoEvents = SetCognitoEvents'
  { -- | The Cognito Identity Pool to use when configuring Cognito Events
    identityPoolId :: Types.IdentityPoolId,
    -- | The events to configure
    events :: Core.HashMap Types.CognitoEventType Types.LambdaFunctionArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetCognitoEvents' value with any optional fields omitted.
mkSetCognitoEvents ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  SetCognitoEvents
mkSetCognitoEvents identityPoolId =
  SetCognitoEvents' {identityPoolId, events = Core.mempty}

-- | The Cognito Identity Pool to use when configuring Cognito Events
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sceIdentityPoolId :: Lens.Lens' SetCognitoEvents Types.IdentityPoolId
sceIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED sceIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | The events to configure
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sceEvents :: Lens.Lens' SetCognitoEvents (Core.HashMap Types.CognitoEventType Types.LambdaFunctionArn)
sceEvents = Lens.field @"events"
{-# DEPRECATED sceEvents "Use generic-lens or generic-optics with 'events' instead." #-}

instance Core.FromJSON SetCognitoEvents where
  toJSON SetCognitoEvents {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Events" Core..= events)])

instance Core.AWSRequest SetCognitoEvents where
  type Rs SetCognitoEvents = SetCognitoEventsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/identitypools/" Core.<> (Core.toText identityPoolId)
                Core.<> ("/events")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull SetCognitoEventsResponse'

-- | /See:/ 'mkSetCognitoEventsResponse' smart constructor.
data SetCognitoEventsResponse = SetCognitoEventsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetCognitoEventsResponse' value with any optional fields omitted.
mkSetCognitoEventsResponse ::
  SetCognitoEventsResponse
mkSetCognitoEventsResponse = SetCognitoEventsResponse'
