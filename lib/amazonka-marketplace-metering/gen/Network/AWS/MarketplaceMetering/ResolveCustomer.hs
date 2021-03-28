{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.ResolveCustomer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- ResolveCustomer is called by a SaaS application during the registration process. When a buyer visits your website during the registration process, the buyer submits a registration token through their browser. The registration token is resolved through this API to obtain a CustomerIdentifier and product code.
module Network.AWS.MarketplaceMetering.ResolveCustomer
    (
    -- * Creating a request
      ResolveCustomer (..)
    , mkResolveCustomer
    -- ** Request lenses
    , rcRegistrationToken

    -- * Destructuring the response
    , ResolveCustomerResponse (..)
    , mkResolveCustomerResponse
    -- ** Response lenses
    , rcrrsCustomerIdentifier
    , rcrrsProductCode
    , rcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MarketplaceMetering.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains input to the ResolveCustomer operation.
--
-- /See:/ 'mkResolveCustomer' smart constructor.
newtype ResolveCustomer = ResolveCustomer'
  { registrationToken :: Types.NonEmptyString
    -- ^ When a buyer visits your website during the registration process, the buyer submits a registration token through the browser. The registration token is resolved to obtain a CustomerIdentifier and product code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResolveCustomer' value with any optional fields omitted.
mkResolveCustomer
    :: Types.NonEmptyString -- ^ 'registrationToken'
    -> ResolveCustomer
mkResolveCustomer registrationToken
  = ResolveCustomer'{registrationToken}

-- | When a buyer visits your website during the registration process, the buyer submits a registration token through the browser. The registration token is resolved to obtain a CustomerIdentifier and product code.
--
-- /Note:/ Consider using 'registrationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRegistrationToken :: Lens.Lens' ResolveCustomer Types.NonEmptyString
rcRegistrationToken = Lens.field @"registrationToken"
{-# INLINEABLE rcRegistrationToken #-}
{-# DEPRECATED registrationToken "Use generic-lens or generic-optics with 'registrationToken' instead"  #-}

instance Core.ToQuery ResolveCustomer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ResolveCustomer where
        toHeaders ResolveCustomer{..}
          = Core.pure
              ("X-Amz-Target", "AWSMPMeteringService.ResolveCustomer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ResolveCustomer where
        toJSON ResolveCustomer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RegistrationToken" Core..= registrationToken)])

instance Core.AWSRequest ResolveCustomer where
        type Rs ResolveCustomer = ResolveCustomerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ResolveCustomerResponse' Core.<$>
                   (x Core..:? "CustomerIdentifier") Core.<*> x Core..:? "ProductCode"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of the ResolveCustomer operation. Contains the CustomerIdentifier and product code.
--
-- /See:/ 'mkResolveCustomerResponse' smart constructor.
data ResolveCustomerResponse = ResolveCustomerResponse'
  { customerIdentifier :: Core.Maybe Types.CustomerIdentifier
    -- ^ The CustomerIdentifier is used to identify an individual customer in your application. Calls to BatchMeterUsage require CustomerIdentifiers for each UsageRecord.
  , productCode :: Core.Maybe Types.ProductCode
    -- ^ The product code is returned to confirm that the buyer is registering for your product. Subsequent BatchMeterUsage calls should be made using this product code.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResolveCustomerResponse' value with any optional fields omitted.
mkResolveCustomerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResolveCustomerResponse
mkResolveCustomerResponse responseStatus
  = ResolveCustomerResponse'{customerIdentifier = Core.Nothing,
                             productCode = Core.Nothing, responseStatus}

-- | The CustomerIdentifier is used to identify an individual customer in your application. Calls to BatchMeterUsage require CustomerIdentifiers for each UsageRecord.
--
-- /Note:/ Consider using 'customerIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsCustomerIdentifier :: Lens.Lens' ResolveCustomerResponse (Core.Maybe Types.CustomerIdentifier)
rcrrsCustomerIdentifier = Lens.field @"customerIdentifier"
{-# INLINEABLE rcrrsCustomerIdentifier #-}
{-# DEPRECATED customerIdentifier "Use generic-lens or generic-optics with 'customerIdentifier' instead"  #-}

-- | The product code is returned to confirm that the buyer is registering for your product. Subsequent BatchMeterUsage calls should be made using this product code.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsProductCode :: Lens.Lens' ResolveCustomerResponse (Core.Maybe Types.ProductCode)
rcrrsProductCode = Lens.field @"productCode"
{-# INLINEABLE rcrrsProductCode #-}
{-# DEPRECATED productCode "Use generic-lens or generic-optics with 'productCode' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsResponseStatus :: Lens.Lens' ResolveCustomerResponse Core.Int
rcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
