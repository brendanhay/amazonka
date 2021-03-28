{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserPoolDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a domain for a user pool.
module Network.AWS.CognitoIdentityProvider.DeleteUserPoolDomain
    (
    -- * Creating a request
      DeleteUserPoolDomain (..)
    , mkDeleteUserPoolDomain
    -- ** Request lenses
    , dupdDomain
    , dupdUserPoolId

    -- * Destructuring the response
    , DeleteUserPoolDomainResponse (..)
    , mkDeleteUserPoolDomainResponse
    -- ** Response lenses
    , dupdrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUserPoolDomain' smart constructor.
data DeleteUserPoolDomain = DeleteUserPoolDomain'
  { domain :: Types.Domain
    -- ^ The domain string.
  , userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserPoolDomain' value with any optional fields omitted.
mkDeleteUserPoolDomain
    :: Types.Domain -- ^ 'domain'
    -> Types.UserPoolId -- ^ 'userPoolId'
    -> DeleteUserPoolDomain
mkDeleteUserPoolDomain domain userPoolId
  = DeleteUserPoolDomain'{domain, userPoolId}

-- | The domain string.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupdDomain :: Lens.Lens' DeleteUserPoolDomain Types.Domain
dupdDomain = Lens.field @"domain"
{-# INLINEABLE dupdDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupdUserPoolId :: Lens.Lens' DeleteUserPoolDomain Types.UserPoolId
dupdUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE dupdUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

instance Core.ToQuery DeleteUserPoolDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteUserPoolDomain where
        toHeaders DeleteUserPoolDomain{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.DeleteUserPoolDomain")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteUserPoolDomain where
        toJSON DeleteUserPoolDomain{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Domain" Core..= domain),
                  Core.Just ("UserPoolId" Core..= userPoolId)])

instance Core.AWSRequest DeleteUserPoolDomain where
        type Rs DeleteUserPoolDomain = DeleteUserPoolDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteUserPoolDomainResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteUserPoolDomainResponse' smart constructor.
newtype DeleteUserPoolDomainResponse = DeleteUserPoolDomainResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserPoolDomainResponse' value with any optional fields omitted.
mkDeleteUserPoolDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteUserPoolDomainResponse
mkDeleteUserPoolDomainResponse responseStatus
  = DeleteUserPoolDomainResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupdrrsResponseStatus :: Lens.Lens' DeleteUserPoolDomainResponse Core.Int
dupdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dupdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
