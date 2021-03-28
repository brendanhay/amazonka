{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.EnableDomainAutoRenew
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation configures Amazon Route 53 to automatically renew the specified domain before the domain registration expires. The cost of renewing your domain registration is billed to your AWS account.
--
-- The period during which you can renew a domain name varies by TLD. For a list of TLDs and their renewal policies, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains That You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ . Route 53 requires that you renew before the end of the renewal period so we can complete processing before the deadline.
module Network.AWS.Route53Domains.EnableDomainAutoRenew
    (
    -- * Creating a request
      EnableDomainAutoRenew (..)
    , mkEnableDomainAutoRenew
    -- ** Request lenses
    , edarDomainName

    -- * Destructuring the response
    , EnableDomainAutoRenewResponse (..)
    , mkEnableDomainAutoRenewResponse
    -- ** Response lenses
    , edarrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | /See:/ 'mkEnableDomainAutoRenew' smart constructor.
newtype EnableDomainAutoRenew = EnableDomainAutoRenew'
  { domainName :: Types.DomainName
    -- ^ The name of the domain that you want to enable automatic renewal for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableDomainAutoRenew' value with any optional fields omitted.
mkEnableDomainAutoRenew
    :: Types.DomainName -- ^ 'domainName'
    -> EnableDomainAutoRenew
mkEnableDomainAutoRenew domainName
  = EnableDomainAutoRenew'{domainName}

-- | The name of the domain that you want to enable automatic renewal for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edarDomainName :: Lens.Lens' EnableDomainAutoRenew Types.DomainName
edarDomainName = Lens.field @"domainName"
{-# INLINEABLE edarDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery EnableDomainAutoRenew where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders EnableDomainAutoRenew where
        toHeaders EnableDomainAutoRenew{..}
          = Core.pure
              ("X-Amz-Target", "Route53Domains_v20140515.EnableDomainAutoRenew")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON EnableDomainAutoRenew where
        toJSON EnableDomainAutoRenew{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DomainName" Core..= domainName)])

instance Core.AWSRequest EnableDomainAutoRenew where
        type Rs EnableDomainAutoRenew = EnableDomainAutoRenewResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 EnableDomainAutoRenewResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableDomainAutoRenewResponse' smart constructor.
newtype EnableDomainAutoRenewResponse = EnableDomainAutoRenewResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableDomainAutoRenewResponse' value with any optional fields omitted.
mkEnableDomainAutoRenewResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnableDomainAutoRenewResponse
mkEnableDomainAutoRenewResponse responseStatus
  = EnableDomainAutoRenewResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edarrrsResponseStatus :: Lens.Lens' EnableDomainAutoRenewResponse Core.Int
edarrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE edarrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
