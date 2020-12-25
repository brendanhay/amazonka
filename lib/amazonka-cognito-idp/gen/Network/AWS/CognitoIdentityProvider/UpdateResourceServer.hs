{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateResourceServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and scopes of resource server. All other fields are read-only.
--
-- /Important:/ If you don't provide a value for an attribute, it will be set to the default value.
module Network.AWS.CognitoIdentityProvider.UpdateResourceServer
  ( -- * Creating a request
    UpdateResourceServer (..),
    mkUpdateResourceServer,

    -- ** Request lenses
    ursUserPoolId,
    ursIdentifier,
    ursName,
    ursScopes,

    -- * Destructuring the response
    UpdateResourceServerResponse (..),
    mkUpdateResourceServerResponse,

    -- ** Response lenses
    ursrrsResourceServer,
    ursrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateResourceServer' smart constructor.
data UpdateResourceServer = UpdateResourceServer'
  { -- | The user pool ID for the user pool.
    userPoolId :: Types.UserPoolId,
    -- | The identifier for the resource server.
    identifier :: Types.Identifier,
    -- | The name of the resource server.
    name :: Types.Name,
    -- | The scope values to be set for the resource server.
    scopes :: Core.Maybe [Types.ResourceServerScopeType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResourceServer' value with any optional fields omitted.
mkUpdateResourceServer ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'identifier'
  Types.Identifier ->
  -- | 'name'
  Types.Name ->
  UpdateResourceServer
mkUpdateResourceServer userPoolId identifier name =
  UpdateResourceServer'
    { userPoolId,
      identifier,
      name,
      scopes = Core.Nothing
    }

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursUserPoolId :: Lens.Lens' UpdateResourceServer Types.UserPoolId
ursUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED ursUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identifier for the resource server.
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursIdentifier :: Lens.Lens' UpdateResourceServer Types.Identifier
ursIdentifier = Lens.field @"identifier"
{-# DEPRECATED ursIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

-- | The name of the resource server.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursName :: Lens.Lens' UpdateResourceServer Types.Name
ursName = Lens.field @"name"
{-# DEPRECATED ursName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The scope values to be set for the resource server.
--
-- /Note:/ Consider using 'scopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursScopes :: Lens.Lens' UpdateResourceServer (Core.Maybe [Types.ResourceServerScopeType])
ursScopes = Lens.field @"scopes"
{-# DEPRECATED ursScopes "Use generic-lens or generic-optics with 'scopes' instead." #-}

instance Core.FromJSON UpdateResourceServer where
  toJSON UpdateResourceServer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Identifier" Core..= identifier),
            Core.Just ("Name" Core..= name),
            ("Scopes" Core..=) Core.<$> scopes
          ]
      )

instance Core.AWSRequest UpdateResourceServer where
  type Rs UpdateResourceServer = UpdateResourceServerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.UpdateResourceServer"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResourceServerResponse'
            Core.<$> (x Core..: "ResourceServer")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateResourceServerResponse' smart constructor.
data UpdateResourceServerResponse = UpdateResourceServerResponse'
  { -- | The resource server.
    resourceServer :: Types.ResourceServerType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResourceServerResponse' value with any optional fields omitted.
mkUpdateResourceServerResponse ::
  -- | 'resourceServer'
  Types.ResourceServerType ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateResourceServerResponse
mkUpdateResourceServerResponse resourceServer responseStatus =
  UpdateResourceServerResponse' {resourceServer, responseStatus}

-- | The resource server.
--
-- /Note:/ Consider using 'resourceServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursrrsResourceServer :: Lens.Lens' UpdateResourceServerResponse Types.ResourceServerType
ursrrsResourceServer = Lens.field @"resourceServer"
{-# DEPRECATED ursrrsResourceServer "Use generic-lens or generic-optics with 'resourceServer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursrrsResponseStatus :: Lens.Lens' UpdateResourceServerResponse Core.Int
ursrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ursrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
