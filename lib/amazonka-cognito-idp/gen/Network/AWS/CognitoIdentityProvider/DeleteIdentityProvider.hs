{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an identity provider for a user pool.
module Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider
    (
    -- * Creating a request
      DeleteIdentityProvider (..)
    , mkDeleteIdentityProvider
    -- ** Request lenses
    , dipfUserPoolId
    , dipfProviderName

    -- * Destructuring the response
    , DeleteIdentityProviderResponse (..)
    , mkDeleteIdentityProviderResponse
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteIdentityProvider' smart constructor.
data DeleteIdentityProvider = DeleteIdentityProvider'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  , providerName :: Types.ProviderNameType
    -- ^ The identity provider name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentityProvider' value with any optional fields omitted.
mkDeleteIdentityProvider
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.ProviderNameType -- ^ 'providerName'
    -> DeleteIdentityProvider
mkDeleteIdentityProvider userPoolId providerName
  = DeleteIdentityProvider'{userPoolId, providerName}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipfUserPoolId :: Lens.Lens' DeleteIdentityProvider Types.UserPoolId
dipfUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE dipfUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipfProviderName :: Lens.Lens' DeleteIdentityProvider Types.ProviderNameType
dipfProviderName = Lens.field @"providerName"
{-# INLINEABLE dipfProviderName #-}
{-# DEPRECATED providerName "Use generic-lens or generic-optics with 'providerName' instead"  #-}

instance Core.ToQuery DeleteIdentityProvider where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteIdentityProvider where
        toHeaders DeleteIdentityProvider{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.DeleteIdentityProvider")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteIdentityProvider where
        toJSON DeleteIdentityProvider{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("ProviderName" Core..= providerName)])

instance Core.AWSRequest DeleteIdentityProvider where
        type Rs DeleteIdentityProvider = DeleteIdentityProviderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteIdentityProviderResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteIdentityProviderResponse' smart constructor.
data DeleteIdentityProviderResponse = DeleteIdentityProviderResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentityProviderResponse' value with any optional fields omitted.
mkDeleteIdentityProviderResponse
    :: DeleteIdentityProviderResponse
mkDeleteIdentityProviderResponse = DeleteIdentityProviderResponse'
