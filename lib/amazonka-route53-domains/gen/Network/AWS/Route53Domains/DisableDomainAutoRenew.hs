{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.DisableDomainAutoRenew
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation disables automatic renewal of domain registration for the specified domain.
module Network.AWS.Route53Domains.DisableDomainAutoRenew
    (
    -- * Creating a request
      DisableDomainAutoRenew (..)
    , mkDisableDomainAutoRenew
    -- ** Request lenses
    , ddarDomainName

    -- * Destructuring the response
    , DisableDomainAutoRenewResponse (..)
    , mkDisableDomainAutoRenewResponse
    -- ** Response lenses
    , ddarrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | /See:/ 'mkDisableDomainAutoRenew' smart constructor.
newtype DisableDomainAutoRenew = DisableDomainAutoRenew'
  { domainName :: Types.DomainName
    -- ^ The name of the domain that you want to disable automatic renewal for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableDomainAutoRenew' value with any optional fields omitted.
mkDisableDomainAutoRenew
    :: Types.DomainName -- ^ 'domainName'
    -> DisableDomainAutoRenew
mkDisableDomainAutoRenew domainName
  = DisableDomainAutoRenew'{domainName}

-- | The name of the domain that you want to disable automatic renewal for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddarDomainName :: Lens.Lens' DisableDomainAutoRenew Types.DomainName
ddarDomainName = Lens.field @"domainName"
{-# INLINEABLE ddarDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery DisableDomainAutoRenew where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisableDomainAutoRenew where
        toHeaders DisableDomainAutoRenew{..}
          = Core.pure
              ("X-Amz-Target", "Route53Domains_v20140515.DisableDomainAutoRenew")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisableDomainAutoRenew where
        toJSON DisableDomainAutoRenew{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DomainName" Core..= domainName)])

instance Core.AWSRequest DisableDomainAutoRenew where
        type Rs DisableDomainAutoRenew = DisableDomainAutoRenewResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisableDomainAutoRenewResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableDomainAutoRenewResponse' smart constructor.
newtype DisableDomainAutoRenewResponse = DisableDomainAutoRenewResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableDomainAutoRenewResponse' value with any optional fields omitted.
mkDisableDomainAutoRenewResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisableDomainAutoRenewResponse
mkDisableDomainAutoRenewResponse responseStatus
  = DisableDomainAutoRenewResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddarrrsResponseStatus :: Lens.Lens' DisableDomainAutoRenewResponse Core.Int
ddarrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddarrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
