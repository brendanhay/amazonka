{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.PurchaseReservedInstancesOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchases a Reserved Instance for use with your account. With Reserved Instances, you pay a lower hourly rate compared to On-Demand instance pricing.
--
-- Use 'DescribeReservedInstancesOfferings' to get a list of Reserved Instance offerings that match your specifications. After you've purchased a Reserved Instance, you can check for your new Reserved Instance with 'DescribeReservedInstances' .
-- To queue a purchase for a future date and time, specify a purchase time. If you do not specify a purchase time, the default is the current time.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-on-demand-reserved-instances.html Reserved Instances> and <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.PurchaseReservedInstancesOffering
    (
    -- * Creating a request
      PurchaseReservedInstancesOffering (..)
    , mkPurchaseReservedInstancesOffering
    -- ** Request lenses
    , prioInstanceCount
    , prioReservedInstancesOfferingId
    , prioDryRun
    , prioLimitPrice
    , prioPurchaseTime

    -- * Destructuring the response
    , PurchaseReservedInstancesOfferingResponse (..)
    , mkPurchaseReservedInstancesOfferingResponse
    -- ** Response lenses
    , priorrsReservedInstancesId
    , priorrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for PurchaseReservedInstancesOffering.
--
-- /See:/ 'mkPurchaseReservedInstancesOffering' smart constructor.
data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering'
  { instanceCount :: Core.Int
    -- ^ The number of Reserved Instances to purchase.
  , reservedInstancesOfferingId :: Types.ReservedInstancesOfferingId
    -- ^ The ID of the Reserved Instance offering to purchase.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , limitPrice :: Core.Maybe Types.ReservedInstanceLimitPrice
    -- ^ Specified for Reserved Instance Marketplace offerings to limit the total order and ensure that the Reserved Instances are not purchased at unexpected prices.
  , purchaseTime :: Core.Maybe Core.UTCTime
    -- ^ The time at which to purchase the Reserved Instance, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PurchaseReservedInstancesOffering' value with any optional fields omitted.
mkPurchaseReservedInstancesOffering
    :: Core.Int -- ^ 'instanceCount'
    -> Types.ReservedInstancesOfferingId -- ^ 'reservedInstancesOfferingId'
    -> PurchaseReservedInstancesOffering
mkPurchaseReservedInstancesOffering instanceCount
  reservedInstancesOfferingId
  = PurchaseReservedInstancesOffering'{instanceCount,
                                       reservedInstancesOfferingId, dryRun = Core.Nothing,
                                       limitPrice = Core.Nothing, purchaseTime = Core.Nothing}

-- | The number of Reserved Instances to purchase.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prioInstanceCount :: Lens.Lens' PurchaseReservedInstancesOffering Core.Int
prioInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE prioInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

-- | The ID of the Reserved Instance offering to purchase.
--
-- /Note:/ Consider using 'reservedInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prioReservedInstancesOfferingId :: Lens.Lens' PurchaseReservedInstancesOffering Types.ReservedInstancesOfferingId
prioReservedInstancesOfferingId = Lens.field @"reservedInstancesOfferingId"
{-# INLINEABLE prioReservedInstancesOfferingId #-}
{-# DEPRECATED reservedInstancesOfferingId "Use generic-lens or generic-optics with 'reservedInstancesOfferingId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prioDryRun :: Lens.Lens' PurchaseReservedInstancesOffering (Core.Maybe Core.Bool)
prioDryRun = Lens.field @"dryRun"
{-# INLINEABLE prioDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Specified for Reserved Instance Marketplace offerings to limit the total order and ensure that the Reserved Instances are not purchased at unexpected prices.
--
-- /Note:/ Consider using 'limitPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prioLimitPrice :: Lens.Lens' PurchaseReservedInstancesOffering (Core.Maybe Types.ReservedInstanceLimitPrice)
prioLimitPrice = Lens.field @"limitPrice"
{-# INLINEABLE prioLimitPrice #-}
{-# DEPRECATED limitPrice "Use generic-lens or generic-optics with 'limitPrice' instead"  #-}

-- | The time at which to purchase the Reserved Instance, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'purchaseTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prioPurchaseTime :: Lens.Lens' PurchaseReservedInstancesOffering (Core.Maybe Core.UTCTime)
prioPurchaseTime = Lens.field @"purchaseTime"
{-# INLINEABLE prioPurchaseTime #-}
{-# DEPRECATED purchaseTime "Use generic-lens or generic-optics with 'purchaseTime' instead"  #-}

instance Core.ToQuery PurchaseReservedInstancesOffering where
        toQuery PurchaseReservedInstancesOffering{..}
          = Core.toQueryPair "Action"
              ("PurchaseReservedInstancesOffering" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceCount" instanceCount
              Core.<>
              Core.toQueryPair "ReservedInstancesOfferingId"
                reservedInstancesOfferingId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LimitPrice") limitPrice
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PurchaseTime")
                purchaseTime

instance Core.ToHeaders PurchaseReservedInstancesOffering where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PurchaseReservedInstancesOffering where
        type Rs PurchaseReservedInstancesOffering =
             PurchaseReservedInstancesOfferingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 PurchaseReservedInstancesOfferingResponse' Core.<$>
                   (x Core..@? "reservedInstancesId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of PurchaseReservedInstancesOffering.
--
-- /See:/ 'mkPurchaseReservedInstancesOfferingResponse' smart constructor.
data PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse'
  { reservedInstancesId :: Core.Maybe Core.Text
    -- ^ The IDs of the purchased Reserved Instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseReservedInstancesOfferingResponse' value with any optional fields omitted.
mkPurchaseReservedInstancesOfferingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PurchaseReservedInstancesOfferingResponse
mkPurchaseReservedInstancesOfferingResponse responseStatus
  = PurchaseReservedInstancesOfferingResponse'{reservedInstancesId =
                                                 Core.Nothing,
                                               responseStatus}

-- | The IDs of the purchased Reserved Instances.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
priorrsReservedInstancesId :: Lens.Lens' PurchaseReservedInstancesOfferingResponse (Core.Maybe Core.Text)
priorrsReservedInstancesId = Lens.field @"reservedInstancesId"
{-# INLINEABLE priorrsReservedInstancesId #-}
{-# DEPRECATED reservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
priorrsResponseStatus :: Lens.Lens' PurchaseReservedInstancesOfferingResponse Core.Int
priorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE priorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
