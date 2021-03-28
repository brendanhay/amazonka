{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.RenewDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation renews a domain for the specified number of years. The cost of renewing your domain is billed to your AWS account.
--
-- We recommend that you renew your domain several weeks before the expiration date. Some TLD registries delete domains before the expiration date if you haven't renewed far enough in advance. For more information about renewing domain registration, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-renew.html Renewing Registration for a Domain> in the /Amazon Route 53 Developer Guide/ .
module Network.AWS.Route53Domains.RenewDomain
    (
    -- * Creating a request
      RenewDomain (..)
    , mkRenewDomain
    -- ** Request lenses
    , rdDomainName
    , rdCurrentExpiryYear
    , rdDurationInYears

    -- * Destructuring the response
    , RenewDomainResponse (..)
    , mkRenewDomainResponse
    -- ** Response lenses
    , rrsOperationId
    , rrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | A @RenewDomain@ request includes the number of years that you want to renew for and the current expiration year.
--
-- /See:/ 'mkRenewDomain' smart constructor.
data RenewDomain = RenewDomain'
  { domainName :: Types.DomainName
    -- ^ The name of the domain that you want to renew.
  , currentExpiryYear :: Core.Int
    -- ^ The year when the registration for the domain is set to expire. This value must match the current expiration date for the domain.
  , durationInYears :: Core.Maybe Core.Natural
    -- ^ The number of years that you want to renew the domain for. The maximum number of years depends on the top-level domain. For the range of valid values for your domain, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- Default: 1
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RenewDomain' value with any optional fields omitted.
mkRenewDomain
    :: Types.DomainName -- ^ 'domainName'
    -> Core.Int -- ^ 'currentExpiryYear'
    -> RenewDomain
mkRenewDomain domainName currentExpiryYear
  = RenewDomain'{domainName, currentExpiryYear,
                 durationInYears = Core.Nothing}

-- | The name of the domain that you want to renew.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDomainName :: Lens.Lens' RenewDomain Types.DomainName
rdDomainName = Lens.field @"domainName"
{-# INLINEABLE rdDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The year when the registration for the domain is set to expire. This value must match the current expiration date for the domain.
--
-- /Note:/ Consider using 'currentExpiryYear' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCurrentExpiryYear :: Lens.Lens' RenewDomain Core.Int
rdCurrentExpiryYear = Lens.field @"currentExpiryYear"
{-# INLINEABLE rdCurrentExpiryYear #-}
{-# DEPRECATED currentExpiryYear "Use generic-lens or generic-optics with 'currentExpiryYear' instead"  #-}

-- | The number of years that you want to renew the domain for. The maximum number of years depends on the top-level domain. For the range of valid values for your domain, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- Default: 1
--
-- /Note:/ Consider using 'durationInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDurationInYears :: Lens.Lens' RenewDomain (Core.Maybe Core.Natural)
rdDurationInYears = Lens.field @"durationInYears"
{-# INLINEABLE rdDurationInYears #-}
{-# DEPRECATED durationInYears "Use generic-lens or generic-optics with 'durationInYears' instead"  #-}

instance Core.ToQuery RenewDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RenewDomain where
        toHeaders RenewDomain{..}
          = Core.pure
              ("X-Amz-Target", "Route53Domains_v20140515.RenewDomain")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RenewDomain where
        toJSON RenewDomain{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainName" Core..= domainName),
                  Core.Just ("CurrentExpiryYear" Core..= currentExpiryYear),
                  ("DurationInYears" Core..=) Core.<$> durationInYears])

instance Core.AWSRequest RenewDomain where
        type Rs RenewDomain = RenewDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RenewDomainResponse' Core.<$>
                   (x Core..: "OperationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRenewDomainResponse' smart constructor.
data RenewDomainResponse = RenewDomainResponse'
  { operationId :: Types.OperationId
    -- ^ Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RenewDomainResponse' value with any optional fields omitted.
mkRenewDomainResponse
    :: Types.OperationId -- ^ 'operationId'
    -> Core.Int -- ^ 'responseStatus'
    -> RenewDomainResponse
mkRenewDomainResponse operationId responseStatus
  = RenewDomainResponse'{operationId, responseStatus}

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsOperationId :: Lens.Lens' RenewDomainResponse Types.OperationId
rrsOperationId = Lens.field @"operationId"
{-# INLINEABLE rrsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RenewDomainResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
