{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to delete a domain. If you onboarded with IAM mode, you will need to delete your domain to onboard again using SSO. Use with caution. All of the members of the domain will lose access to their EFS volume, including data, notebooks, and other artifacts. 
module Network.AWS.SageMaker.DeleteDomain
    (
    -- * Creating a request
      DeleteDomain (..)
    , mkDeleteDomain
    -- ** Request lenses
    , ddgDomainId
    , ddgRetentionPolicy

    -- * Destructuring the response
    , DeleteDomainResponse (..)
    , mkDeleteDomainResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteDomain' smart constructor.
data DeleteDomain = DeleteDomain'
  { domainId :: Types.DomainId
    -- ^ The domain ID.
  , retentionPolicy :: Core.Maybe Types.RetentionPolicy
    -- ^ The retention policy for this domain, which specifies whether resources will be retained after the Domain is deleted. By default, all resources are retained (not automatically deleted). 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomain' value with any optional fields omitted.
mkDeleteDomain
    :: Types.DomainId -- ^ 'domainId'
    -> DeleteDomain
mkDeleteDomain domainId
  = DeleteDomain'{domainId, retentionPolicy = Core.Nothing}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgDomainId :: Lens.Lens' DeleteDomain Types.DomainId
ddgDomainId = Lens.field @"domainId"
{-# INLINEABLE ddgDomainId #-}
{-# DEPRECATED domainId "Use generic-lens or generic-optics with 'domainId' instead"  #-}

-- | The retention policy for this domain, which specifies whether resources will be retained after the Domain is deleted. By default, all resources are retained (not automatically deleted). 
--
-- /Note:/ Consider using 'retentionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgRetentionPolicy :: Lens.Lens' DeleteDomain (Core.Maybe Types.RetentionPolicy)
ddgRetentionPolicy = Lens.field @"retentionPolicy"
{-# INLINEABLE ddgRetentionPolicy #-}
{-# DEPRECATED retentionPolicy "Use generic-lens or generic-optics with 'retentionPolicy' instead"  #-}

instance Core.ToQuery DeleteDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDomain where
        toHeaders DeleteDomain{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DeleteDomain") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDomain where
        toJSON DeleteDomain{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainId" Core..= domainId),
                  ("RetentionPolicy" Core..=) Core.<$> retentionPolicy])

instance Core.AWSRequest DeleteDomain where
        type Rs DeleteDomain = DeleteDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteDomainResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomainResponse' value with any optional fields omitted.
mkDeleteDomainResponse
    :: DeleteDomainResponse
mkDeleteDomainResponse = DeleteDomainResponse'
