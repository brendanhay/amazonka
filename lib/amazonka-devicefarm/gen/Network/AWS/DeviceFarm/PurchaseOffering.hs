{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.PurchaseOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Immediately purchases offerings for an AWS account. Offerings renew with the latest total purchased quantity for an offering, unless the renewal was overridden. The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. If you must be able to invoke this operation, contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> .
module Network.AWS.DeviceFarm.PurchaseOffering
    (
    -- * Creating a request
      PurchaseOffering (..)
    , mkPurchaseOffering
    -- ** Request lenses
    , poOfferingId
    , poOfferingPromotionId
    , poQuantity

    -- * Destructuring the response
    , PurchaseOfferingResponse (..)
    , mkPurchaseOfferingResponse
    -- ** Response lenses
    , porrsOfferingTransaction
    , porrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request for a purchase offering.
--
-- /See:/ 'mkPurchaseOffering' smart constructor.
data PurchaseOffering = PurchaseOffering'
  { offeringId :: Core.Maybe Types.OfferingIdentifier
    -- ^ The ID of the offering.
  , offeringPromotionId :: Core.Maybe Types.OfferingPromotionId
    -- ^ The ID of the offering promotion to be applied to the purchase.
  , quantity :: Core.Maybe Core.Int
    -- ^ The number of device slots to purchase in an offering request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseOffering' value with any optional fields omitted.
mkPurchaseOffering
    :: PurchaseOffering
mkPurchaseOffering
  = PurchaseOffering'{offeringId = Core.Nothing,
                      offeringPromotionId = Core.Nothing, quantity = Core.Nothing}

-- | The ID of the offering.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poOfferingId :: Lens.Lens' PurchaseOffering (Core.Maybe Types.OfferingIdentifier)
poOfferingId = Lens.field @"offeringId"
{-# INLINEABLE poOfferingId #-}
{-# DEPRECATED offeringId "Use generic-lens or generic-optics with 'offeringId' instead"  #-}

-- | The ID of the offering promotion to be applied to the purchase.
--
-- /Note:/ Consider using 'offeringPromotionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poOfferingPromotionId :: Lens.Lens' PurchaseOffering (Core.Maybe Types.OfferingPromotionId)
poOfferingPromotionId = Lens.field @"offeringPromotionId"
{-# INLINEABLE poOfferingPromotionId #-}
{-# DEPRECATED offeringPromotionId "Use generic-lens or generic-optics with 'offeringPromotionId' instead"  #-}

-- | The number of device slots to purchase in an offering request.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poQuantity :: Lens.Lens' PurchaseOffering (Core.Maybe Core.Int)
poQuantity = Lens.field @"quantity"
{-# INLINEABLE poQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

instance Core.ToQuery PurchaseOffering where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PurchaseOffering where
        toHeaders PurchaseOffering{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.PurchaseOffering")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PurchaseOffering where
        toJSON PurchaseOffering{..}
          = Core.object
              (Core.catMaybes
                 [("offeringId" Core..=) Core.<$> offeringId,
                  ("offeringPromotionId" Core..=) Core.<$> offeringPromotionId,
                  ("quantity" Core..=) Core.<$> quantity])

instance Core.AWSRequest PurchaseOffering where
        type Rs PurchaseOffering = PurchaseOfferingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PurchaseOfferingResponse' Core.<$>
                   (x Core..:? "offeringTransaction") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of the purchase offering (for example, success or failure).
--
-- /See:/ 'mkPurchaseOfferingResponse' smart constructor.
data PurchaseOfferingResponse = PurchaseOfferingResponse'
  { offeringTransaction :: Core.Maybe Types.OfferingTransaction
    -- ^ Represents the offering transaction for the purchase result.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PurchaseOfferingResponse' value with any optional fields omitted.
mkPurchaseOfferingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PurchaseOfferingResponse
mkPurchaseOfferingResponse responseStatus
  = PurchaseOfferingResponse'{offeringTransaction = Core.Nothing,
                              responseStatus}

-- | Represents the offering transaction for the purchase result.
--
-- /Note:/ Consider using 'offeringTransaction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsOfferingTransaction :: Lens.Lens' PurchaseOfferingResponse (Core.Maybe Types.OfferingTransaction)
porrsOfferingTransaction = Lens.field @"offeringTransaction"
{-# INLINEABLE porrsOfferingTransaction #-}
{-# DEPRECATED offeringTransaction "Use generic-lens or generic-optics with 'offeringTransaction' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsResponseStatus :: Lens.Lens' PurchaseOfferingResponse Core.Int
porrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE porrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
