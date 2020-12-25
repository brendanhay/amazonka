{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.MeterUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- API to emit metering records. For identical requests, the API is idempotent. It simply returns the metering record ID.
--
-- MeterUsage is authenticated on the buyer's AWS account using credentials from the EC2 instance, ECS task, or EKS pod.
-- MeterUsage can optionally include multiple usage allocations, to provide customers with usage data split into buckets by tags that you define (or allow the customer to define).
module Network.AWS.MarketplaceMetering.MeterUsage
  ( -- * Creating a request
    MeterUsage (..),
    mkMeterUsage,

    -- ** Request lenses
    muProductCode,
    muTimestamp,
    muUsageDimension,
    muDryRun,
    muUsageAllocations,
    muUsageQuantity,

    -- * Destructuring the response
    MeterUsageResponse (..),
    mkMeterUsageResponse,

    -- ** Response lenses
    murrsMeteringRecordId,
    murrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MarketplaceMetering.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkMeterUsage' smart constructor.
data MeterUsage = MeterUsage'
  { -- | Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
    productCode :: Types.ProductCode,
    -- | Timestamp, in UTC, for which the usage is being reported. Your application can meter usage for up to one hour in the past. Make sure the timestamp value is not before the start of the software usage.
    timestamp :: Core.NominalDiffTime,
    -- | It will be one of the fcp dimension name provided during the publishing of the product.
    usageDimension :: Types.UsageDimension,
    -- | Checks whether you have the permissions required for the action, but does not make the request. If you have the permissions, the request returns DryRunOperation; otherwise, it returns UnauthorizedException. Defaults to @false@ if not specified.
    dryRun :: Core.Maybe Core.Bool,
    -- | The set of UsageAllocations to submit.
    --
    -- The sum of all UsageAllocation quantities must equal the UsageQuantity of the MeterUsage request, and each UsageAllocation must have a unique set of tags (include no tags).
    usageAllocations :: Core.Maybe (Core.NonEmpty Types.UsageAllocation),
    -- | Consumption value for the hour. Defaults to @0@ if not specified.
    usageQuantity :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'MeterUsage' value with any optional fields omitted.
mkMeterUsage ::
  -- | 'productCode'
  Types.ProductCode ->
  -- | 'timestamp'
  Core.NominalDiffTime ->
  -- | 'usageDimension'
  Types.UsageDimension ->
  MeterUsage
mkMeterUsage productCode timestamp usageDimension =
  MeterUsage'
    { productCode,
      timestamp,
      usageDimension,
      dryRun = Core.Nothing,
      usageAllocations = Core.Nothing,
      usageQuantity = Core.Nothing
    }

-- | Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muProductCode :: Lens.Lens' MeterUsage Types.ProductCode
muProductCode = Lens.field @"productCode"
{-# DEPRECATED muProductCode "Use generic-lens or generic-optics with 'productCode' instead." #-}

-- | Timestamp, in UTC, for which the usage is being reported. Your application can meter usage for up to one hour in the past. Make sure the timestamp value is not before the start of the software usage.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muTimestamp :: Lens.Lens' MeterUsage Core.NominalDiffTime
muTimestamp = Lens.field @"timestamp"
{-# DEPRECATED muTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | It will be one of the fcp dimension name provided during the publishing of the product.
--
-- /Note:/ Consider using 'usageDimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muUsageDimension :: Lens.Lens' MeterUsage Types.UsageDimension
muUsageDimension = Lens.field @"usageDimension"
{-# DEPRECATED muUsageDimension "Use generic-lens or generic-optics with 'usageDimension' instead." #-}

-- | Checks whether you have the permissions required for the action, but does not make the request. If you have the permissions, the request returns DryRunOperation; otherwise, it returns UnauthorizedException. Defaults to @false@ if not specified.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muDryRun :: Lens.Lens' MeterUsage (Core.Maybe Core.Bool)
muDryRun = Lens.field @"dryRun"
{-# DEPRECATED muDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The set of UsageAllocations to submit.
--
-- The sum of all UsageAllocation quantities must equal the UsageQuantity of the MeterUsage request, and each UsageAllocation must have a unique set of tags (include no tags).
--
-- /Note:/ Consider using 'usageAllocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muUsageAllocations :: Lens.Lens' MeterUsage (Core.Maybe (Core.NonEmpty Types.UsageAllocation))
muUsageAllocations = Lens.field @"usageAllocations"
{-# DEPRECATED muUsageAllocations "Use generic-lens or generic-optics with 'usageAllocations' instead." #-}

-- | Consumption value for the hour. Defaults to @0@ if not specified.
--
-- /Note:/ Consider using 'usageQuantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muUsageQuantity :: Lens.Lens' MeterUsage (Core.Maybe Core.Natural)
muUsageQuantity = Lens.field @"usageQuantity"
{-# DEPRECATED muUsageQuantity "Use generic-lens or generic-optics with 'usageQuantity' instead." #-}

instance Core.FromJSON MeterUsage where
  toJSON MeterUsage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProductCode" Core..= productCode),
            Core.Just ("Timestamp" Core..= timestamp),
            Core.Just ("UsageDimension" Core..= usageDimension),
            ("DryRun" Core..=) Core.<$> dryRun,
            ("UsageAllocations" Core..=) Core.<$> usageAllocations,
            ("UsageQuantity" Core..=) Core.<$> usageQuantity
          ]
      )

instance Core.AWSRequest MeterUsage where
  type Rs MeterUsage = MeterUsageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSMPMeteringService.MeterUsage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          MeterUsageResponse'
            Core.<$> (x Core..:? "MeteringRecordId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkMeterUsageResponse' smart constructor.
data MeterUsageResponse = MeterUsageResponse'
  { -- | Metering record id.
    meteringRecordId :: Core.Maybe Types.MeteringRecordId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MeterUsageResponse' value with any optional fields omitted.
mkMeterUsageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  MeterUsageResponse
mkMeterUsageResponse responseStatus =
  MeterUsageResponse'
    { meteringRecordId = Core.Nothing,
      responseStatus
    }

-- | Metering record id.
--
-- /Note:/ Consider using 'meteringRecordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
murrsMeteringRecordId :: Lens.Lens' MeterUsageResponse (Core.Maybe Types.MeteringRecordId)
murrsMeteringRecordId = Lens.field @"meteringRecordId"
{-# DEPRECATED murrsMeteringRecordId "Use generic-lens or generic-optics with 'meteringRecordId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
murrsResponseStatus :: Lens.Lens' MeterUsageResponse Core.Int
murrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED murrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
