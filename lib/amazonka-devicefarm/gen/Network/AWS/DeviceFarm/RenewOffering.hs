{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.RenewOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Explicitly sets the quantity of devices to renew for an offering, starting from the @effectiveDate@ of the next period. The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. If you must be able to invoke this operation, contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> .
module Network.AWS.DeviceFarm.RenewOffering
    (
    -- * Creating a request
      RenewOffering (..)
    , mkRenewOffering
    -- ** Request lenses
    , roOfferingId
    , roQuantity

    -- * Destructuring the response
    , RenewOfferingResponse (..)
    , mkRenewOfferingResponse
    -- ** Response lenses
    , rorrsOfferingTransaction
    , rorrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request that represents an offering renewal.
--
-- /See:/ 'mkRenewOffering' smart constructor.
data RenewOffering = RenewOffering'
  { offeringId :: Core.Maybe Types.OfferingId
    -- ^ The ID of a request to renew an offering.
  , quantity :: Core.Maybe Core.Int
    -- ^ The quantity requested in an offering renewal.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RenewOffering' value with any optional fields omitted.
mkRenewOffering
    :: RenewOffering
mkRenewOffering
  = RenewOffering'{offeringId = Core.Nothing,
                   quantity = Core.Nothing}

-- | The ID of a request to renew an offering.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roOfferingId :: Lens.Lens' RenewOffering (Core.Maybe Types.OfferingId)
roOfferingId = Lens.field @"offeringId"
{-# INLINEABLE roOfferingId #-}
{-# DEPRECATED offeringId "Use generic-lens or generic-optics with 'offeringId' instead"  #-}

-- | The quantity requested in an offering renewal.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roQuantity :: Lens.Lens' RenewOffering (Core.Maybe Core.Int)
roQuantity = Lens.field @"quantity"
{-# INLINEABLE roQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

instance Core.ToQuery RenewOffering where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RenewOffering where
        toHeaders RenewOffering{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.RenewOffering")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RenewOffering where
        toJSON RenewOffering{..}
          = Core.object
              (Core.catMaybes
                 [("offeringId" Core..=) Core.<$> offeringId,
                  ("quantity" Core..=) Core.<$> quantity])

instance Core.AWSRequest RenewOffering where
        type Rs RenewOffering = RenewOfferingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RenewOfferingResponse' Core.<$>
                   (x Core..:? "offeringTransaction") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a renewal offering.
--
-- /See:/ 'mkRenewOfferingResponse' smart constructor.
data RenewOfferingResponse = RenewOfferingResponse'
  { offeringTransaction :: Core.Maybe Types.OfferingTransaction
    -- ^ Represents the status of the offering transaction for the renewal.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RenewOfferingResponse' value with any optional fields omitted.
mkRenewOfferingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RenewOfferingResponse
mkRenewOfferingResponse responseStatus
  = RenewOfferingResponse'{offeringTransaction = Core.Nothing,
                           responseStatus}

-- | Represents the status of the offering transaction for the renewal.
--
-- /Note:/ Consider using 'offeringTransaction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rorrsOfferingTransaction :: Lens.Lens' RenewOfferingResponse (Core.Maybe Types.OfferingTransaction)
rorrsOfferingTransaction = Lens.field @"offeringTransaction"
{-# INLINEABLE rorrsOfferingTransaction #-}
{-# DEPRECATED offeringTransaction "Use generic-lens or generic-optics with 'offeringTransaction' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rorrsResponseStatus :: Lens.Lens' RenewOfferingResponse Core.Int
rorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
