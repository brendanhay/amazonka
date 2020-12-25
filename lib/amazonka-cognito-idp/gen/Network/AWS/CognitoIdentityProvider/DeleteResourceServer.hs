{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteResourceServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource server.
module Network.AWS.CognitoIdentityProvider.DeleteResourceServer
  ( -- * Creating a request
    DeleteResourceServer (..),
    mkDeleteResourceServer,

    -- ** Request lenses
    drsUserPoolId,
    drsIdentifier,

    -- * Destructuring the response
    DeleteResourceServerResponse (..),
    mkDeleteResourceServerResponse,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteResourceServer' smart constructor.
data DeleteResourceServer = DeleteResourceServer'
  { -- | The user pool ID for the user pool that hosts the resource server.
    userPoolId :: Types.UserPoolId,
    -- | The identifier for the resource server.
    identifier :: Types.Identifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceServer' value with any optional fields omitted.
mkDeleteResourceServer ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'identifier'
  Types.Identifier ->
  DeleteResourceServer
mkDeleteResourceServer userPoolId identifier =
  DeleteResourceServer' {userPoolId, identifier}

-- | The user pool ID for the user pool that hosts the resource server.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsUserPoolId :: Lens.Lens' DeleteResourceServer Types.UserPoolId
drsUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED drsUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identifier for the resource server.
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsIdentifier :: Lens.Lens' DeleteResourceServer Types.Identifier
drsIdentifier = Lens.field @"identifier"
{-# DEPRECATED drsIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

instance Core.FromJSON DeleteResourceServer where
  toJSON DeleteResourceServer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Identifier" Core..= identifier)
          ]
      )

instance Core.AWSRequest DeleteResourceServer where
  type Rs DeleteResourceServer = DeleteResourceServerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.DeleteResourceServer"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteResourceServerResponse'

-- | /See:/ 'mkDeleteResourceServerResponse' smart constructor.
data DeleteResourceServerResponse = DeleteResourceServerResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceServerResponse' value with any optional fields omitted.
mkDeleteResourceServerResponse ::
  DeleteResourceServerResponse
mkDeleteResourceServerResponse = DeleteResourceServerResponse'
