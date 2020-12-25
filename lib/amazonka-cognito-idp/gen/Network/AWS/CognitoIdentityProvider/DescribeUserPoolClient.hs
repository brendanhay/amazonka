{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Client method for returning the configuration information and metadata of the specified user pool app client.
module Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
  ( -- * Creating a request
    DescribeUserPoolClient (..),
    mkDescribeUserPoolClient,

    -- ** Request lenses
    dupcfUserPoolId,
    dupcfClientId,

    -- * Destructuring the response
    DescribeUserPoolClientResponse (..),
    mkDescribeUserPoolClientResponse,

    -- ** Response lenses
    dupcrrsUserPoolClient,
    dupcrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to describe a user pool client.
--
-- /See:/ 'mkDescribeUserPoolClient' smart constructor.
data DescribeUserPoolClient = DescribeUserPoolClient'
  { -- | The user pool ID for the user pool you want to describe.
    userPoolId :: Types.UserPoolId,
    -- | The app client ID of the app associated with the user pool.
    clientId :: Types.ClientIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserPoolClient' value with any optional fields omitted.
mkDescribeUserPoolClient ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'clientId'
  Types.ClientIdType ->
  DescribeUserPoolClient
mkDescribeUserPoolClient userPoolId clientId =
  DescribeUserPoolClient' {userPoolId, clientId}

-- | The user pool ID for the user pool you want to describe.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcfUserPoolId :: Lens.Lens' DescribeUserPoolClient Types.UserPoolId
dupcfUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED dupcfUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The app client ID of the app associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcfClientId :: Lens.Lens' DescribeUserPoolClient Types.ClientIdType
dupcfClientId = Lens.field @"clientId"
{-# DEPRECATED dupcfClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

instance Core.FromJSON DescribeUserPoolClient where
  toJSON DescribeUserPoolClient {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ClientId" Core..= clientId)
          ]
      )

instance Core.AWSRequest DescribeUserPoolClient where
  type Rs DescribeUserPoolClient = DescribeUserPoolClientResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.DescribeUserPoolClient"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserPoolClientResponse'
            Core.<$> (x Core..:? "UserPoolClient")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the response from the server from a request to describe the user pool client.
--
-- /See:/ 'mkDescribeUserPoolClientResponse' smart constructor.
data DescribeUserPoolClientResponse = DescribeUserPoolClientResponse'
  { -- | The user pool client from a server response to describe the user pool client.
    userPoolClient :: Core.Maybe Types.UserPoolClientType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeUserPoolClientResponse' value with any optional fields omitted.
mkDescribeUserPoolClientResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUserPoolClientResponse
mkDescribeUserPoolClientResponse responseStatus =
  DescribeUserPoolClientResponse'
    { userPoolClient = Core.Nothing,
      responseStatus
    }

-- | The user pool client from a server response to describe the user pool client.
--
-- /Note:/ Consider using 'userPoolClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcrrsUserPoolClient :: Lens.Lens' DescribeUserPoolClientResponse (Core.Maybe Types.UserPoolClientType)
dupcrrsUserPoolClient = Lens.field @"userPoolClient"
{-# DEPRECATED dupcrrsUserPoolClient "Use generic-lens or generic-optics with 'userPoolClient' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcrrsResponseStatus :: Lens.Lens' DescribeUserPoolClientResponse Core.Int
dupcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dupcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
