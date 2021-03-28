{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateResourceServer (..)
    , mkUpdateResourceServer
    -- ** Request lenses
    , ursUserPoolId
    , ursIdentifier
    , ursName
    , ursScopes

    -- * Destructuring the response
    , UpdateResourceServerResponse (..)
    , mkUpdateResourceServerResponse
    -- ** Response lenses
    , ursrrsResourceServer
    , ursrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateResourceServer' smart constructor.
data UpdateResourceServer = UpdateResourceServer'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool.
  , identifier :: Types.Identifier
    -- ^ The identifier for the resource server.
  , name :: Types.Name
    -- ^ The name of the resource server.
  , scopes :: Core.Maybe [Types.ResourceServerScopeType]
    -- ^ The scope values to be set for the resource server.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResourceServer' value with any optional fields omitted.
mkUpdateResourceServer
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.Identifier -- ^ 'identifier'
    -> Types.Name -- ^ 'name'
    -> UpdateResourceServer
mkUpdateResourceServer userPoolId identifier name
  = UpdateResourceServer'{userPoolId, identifier, name,
                          scopes = Core.Nothing}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursUserPoolId :: Lens.Lens' UpdateResourceServer Types.UserPoolId
ursUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE ursUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The identifier for the resource server.
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursIdentifier :: Lens.Lens' UpdateResourceServer Types.Identifier
ursIdentifier = Lens.field @"identifier"
{-# INLINEABLE ursIdentifier #-}
{-# DEPRECATED identifier "Use generic-lens or generic-optics with 'identifier' instead"  #-}

-- | The name of the resource server.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursName :: Lens.Lens' UpdateResourceServer Types.Name
ursName = Lens.field @"name"
{-# INLINEABLE ursName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The scope values to be set for the resource server.
--
-- /Note:/ Consider using 'scopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursScopes :: Lens.Lens' UpdateResourceServer (Core.Maybe [Types.ResourceServerScopeType])
ursScopes = Lens.field @"scopes"
{-# INLINEABLE ursScopes #-}
{-# DEPRECATED scopes "Use generic-lens or generic-optics with 'scopes' instead"  #-}

instance Core.ToQuery UpdateResourceServer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateResourceServer where
        toHeaders UpdateResourceServer{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.UpdateResourceServer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateResourceServer where
        toJSON UpdateResourceServer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Identifier" Core..= identifier),
                  Core.Just ("Name" Core..= name),
                  ("Scopes" Core..=) Core.<$> scopes])

instance Core.AWSRequest UpdateResourceServer where
        type Rs UpdateResourceServer = UpdateResourceServerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateResourceServerResponse' Core.<$>
                   (x Core..: "ResourceServer") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateResourceServerResponse' smart constructor.
data UpdateResourceServerResponse = UpdateResourceServerResponse'
  { resourceServer :: Types.ResourceServerType
    -- ^ The resource server.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResourceServerResponse' value with any optional fields omitted.
mkUpdateResourceServerResponse
    :: Types.ResourceServerType -- ^ 'resourceServer'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateResourceServerResponse
mkUpdateResourceServerResponse resourceServer responseStatus
  = UpdateResourceServerResponse'{resourceServer, responseStatus}

-- | The resource server.
--
-- /Note:/ Consider using 'resourceServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursrrsResourceServer :: Lens.Lens' UpdateResourceServerResponse Types.ResourceServerType
ursrrsResourceServer = Lens.field @"resourceServer"
{-# INLINEABLE ursrrsResourceServer #-}
{-# DEPRECATED resourceServer "Use generic-lens or generic-optics with 'resourceServer' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursrrsResponseStatus :: Lens.Lens' UpdateResourceServerResponse Core.Int
ursrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ursrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
