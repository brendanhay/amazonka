{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.CreateResourceServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new OAuth2.0 resource server and defines custom scopes in it.
module Network.AWS.CognitoIdentityProvider.CreateResourceServer
  ( -- * Creating a request
    CreateResourceServer (..),
    mkCreateResourceServer,

    -- ** Request lenses
    crsUserPoolId,
    crsIdentifier,
    crsName,
    crsScopes,

    -- * Destructuring the response
    CreateResourceServerResponse (..),
    mkCreateResourceServerResponse,

    -- ** Response lenses
    crsrrsResourceServer,
    crsrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateResourceServer' smart constructor.
data CreateResourceServer = CreateResourceServer'
  { -- | The user pool ID for the user pool.
    userPoolId :: Types.UserPoolId,
    -- | A unique resource server identifier for the resource server. This could be an HTTPS endpoint where the resource server is located. For example, @https://my-weather-api.example.com@ .
    identifier :: Types.ResourceServerIdentifierType,
    -- | A friendly name for the resource server.
    name :: Types.ResourceServerNameType,
    -- | A list of scopes. Each scope is map, where the keys are @name@ and @description@ .
    scopes :: Core.Maybe [Types.ResourceServerScopeType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResourceServer' value with any optional fields omitted.
mkCreateResourceServer ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'identifier'
  Types.ResourceServerIdentifierType ->
  -- | 'name'
  Types.ResourceServerNameType ->
  CreateResourceServer
mkCreateResourceServer userPoolId identifier name =
  CreateResourceServer'
    { userPoolId,
      identifier,
      name,
      scopes = Core.Nothing
    }

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsUserPoolId :: Lens.Lens' CreateResourceServer Types.UserPoolId
crsUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED crsUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | A unique resource server identifier for the resource server. This could be an HTTPS endpoint where the resource server is located. For example, @https://my-weather-api.example.com@ .
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsIdentifier :: Lens.Lens' CreateResourceServer Types.ResourceServerIdentifierType
crsIdentifier = Lens.field @"identifier"
{-# DEPRECATED crsIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

-- | A friendly name for the resource server.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsName :: Lens.Lens' CreateResourceServer Types.ResourceServerNameType
crsName = Lens.field @"name"
{-# DEPRECATED crsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of scopes. Each scope is map, where the keys are @name@ and @description@ .
--
-- /Note:/ Consider using 'scopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsScopes :: Lens.Lens' CreateResourceServer (Core.Maybe [Types.ResourceServerScopeType])
crsScopes = Lens.field @"scopes"
{-# DEPRECATED crsScopes "Use generic-lens or generic-optics with 'scopes' instead." #-}

instance Core.FromJSON CreateResourceServer where
  toJSON CreateResourceServer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Identifier" Core..= identifier),
            Core.Just ("Name" Core..= name),
            ("Scopes" Core..=) Core.<$> scopes
          ]
      )

instance Core.AWSRequest CreateResourceServer where
  type Rs CreateResourceServer = CreateResourceServerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.CreateResourceServer"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceServerResponse'
            Core.<$> (x Core..: "ResourceServer")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateResourceServerResponse' smart constructor.
data CreateResourceServerResponse = CreateResourceServerResponse'
  { -- | The newly created resource server.
    resourceServer :: Types.ResourceServerType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResourceServerResponse' value with any optional fields omitted.
mkCreateResourceServerResponse ::
  -- | 'resourceServer'
  Types.ResourceServerType ->
  -- | 'responseStatus'
  Core.Int ->
  CreateResourceServerResponse
mkCreateResourceServerResponse resourceServer responseStatus =
  CreateResourceServerResponse' {resourceServer, responseStatus}

-- | The newly created resource server.
--
-- /Note:/ Consider using 'resourceServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrrsResourceServer :: Lens.Lens' CreateResourceServerResponse Types.ResourceServerType
crsrrsResourceServer = Lens.field @"resourceServer"
{-# DEPRECATED crsrrsResourceServer "Use generic-lens or generic-optics with 'resourceServer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrrsResponseStatus :: Lens.Lens' CreateResourceServerResponse Core.Int
crsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
