{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeResourceServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a resource server.
module Network.AWS.CognitoIdentityProvider.DescribeResourceServer
  ( -- * Creating a request
    DescribeResourceServer (..),
    mkDescribeResourceServer,

    -- ** Request lenses
    drsfUserPoolId,
    drsfIdentifier,

    -- * Destructuring the response
    DescribeResourceServerResponse (..),
    mkDescribeResourceServerResponse,

    -- ** Response lenses
    drsrrsResourceServer,
    drsrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeResourceServer' smart constructor.
data DescribeResourceServer = DescribeResourceServer'
  { -- | The user pool ID for the user pool that hosts the resource server.
    userPoolId :: Types.UserPoolId,
    -- | The identifier for the resource server
    identifier :: Types.Identifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeResourceServer' value with any optional fields omitted.
mkDescribeResourceServer ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'identifier'
  Types.Identifier ->
  DescribeResourceServer
mkDescribeResourceServer userPoolId identifier =
  DescribeResourceServer' {userPoolId, identifier}

-- | The user pool ID for the user pool that hosts the resource server.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsfUserPoolId :: Lens.Lens' DescribeResourceServer Types.UserPoolId
drsfUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED drsfUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identifier for the resource server
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsfIdentifier :: Lens.Lens' DescribeResourceServer Types.Identifier
drsfIdentifier = Lens.field @"identifier"
{-# DEPRECATED drsfIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

instance Core.FromJSON DescribeResourceServer where
  toJSON DescribeResourceServer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Identifier" Core..= identifier)
          ]
      )

instance Core.AWSRequest DescribeResourceServer where
  type Rs DescribeResourceServer = DescribeResourceServerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.DescribeResourceServer"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourceServerResponse'
            Core.<$> (x Core..: "ResourceServer")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeResourceServerResponse' smart constructor.
data DescribeResourceServerResponse = DescribeResourceServerResponse'
  { -- | The resource server.
    resourceServer :: Types.ResourceServerType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeResourceServerResponse' value with any optional fields omitted.
mkDescribeResourceServerResponse ::
  -- | 'resourceServer'
  Types.ResourceServerType ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeResourceServerResponse
mkDescribeResourceServerResponse resourceServer responseStatus =
  DescribeResourceServerResponse' {resourceServer, responseStatus}

-- | The resource server.
--
-- /Note:/ Consider using 'resourceServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsrrsResourceServer :: Lens.Lens' DescribeResourceServerResponse Types.ResourceServerType
drsrrsResourceServer = Lens.field @"resourceServer"
{-# DEPRECATED drsrrsResourceServer "Use generic-lens or generic-optics with 'resourceServer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsrrsResponseStatus :: Lens.Lens' DescribeResourceServerResponse Core.Int
drsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
