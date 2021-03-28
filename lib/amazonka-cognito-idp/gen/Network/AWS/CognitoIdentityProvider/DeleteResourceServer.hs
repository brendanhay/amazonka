{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteResourceServer (..)
    , mkDeleteResourceServer
    -- ** Request lenses
    , drsUserPoolId
    , drsIdentifier

    -- * Destructuring the response
    , DeleteResourceServerResponse (..)
    , mkDeleteResourceServerResponse
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteResourceServer' smart constructor.
data DeleteResourceServer = DeleteResourceServer'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool that hosts the resource server.
  , identifier :: Types.Identifier
    -- ^ The identifier for the resource server.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceServer' value with any optional fields omitted.
mkDeleteResourceServer
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.Identifier -- ^ 'identifier'
    -> DeleteResourceServer
mkDeleteResourceServer userPoolId identifier
  = DeleteResourceServer'{userPoolId, identifier}

-- | The user pool ID for the user pool that hosts the resource server.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsUserPoolId :: Lens.Lens' DeleteResourceServer Types.UserPoolId
drsUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE drsUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The identifier for the resource server.
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsIdentifier :: Lens.Lens' DeleteResourceServer Types.Identifier
drsIdentifier = Lens.field @"identifier"
{-# INLINEABLE drsIdentifier #-}
{-# DEPRECATED identifier "Use generic-lens or generic-optics with 'identifier' instead"  #-}

instance Core.ToQuery DeleteResourceServer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteResourceServer where
        toHeaders DeleteResourceServer{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.DeleteResourceServer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteResourceServer where
        toJSON DeleteResourceServer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Identifier" Core..= identifier)])

instance Core.AWSRequest DeleteResourceServer where
        type Rs DeleteResourceServer = DeleteResourceServerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteResourceServerResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteResourceServerResponse' smart constructor.
data DeleteResourceServerResponse = DeleteResourceServerResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceServerResponse' value with any optional fields omitted.
mkDeleteResourceServerResponse
    :: DeleteResourceServerResponse
mkDeleteResourceServerResponse = DeleteResourceServerResponse'
