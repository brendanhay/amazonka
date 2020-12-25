{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.BatchMeterUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- BatchMeterUsage is called from a SaaS application listed on the AWS Marketplace to post metering records for a set of customers.
--
-- For identical requests, the API is idempotent; requests can be retried with the same records or a subset of the input records.
-- Every request to BatchMeterUsage is for one product. If you need to meter usage for multiple products, you must make multiple calls to BatchMeterUsage.
-- BatchMeterUsage can process up to 25 UsageRecords at a time.
-- A UsageRecord can optionally include multiple usage allocations, to provide customers with usagedata split into buckets by tags that you define (or allow the customer to define).
-- BatchMeterUsage requests must be less than 1MB in size.
module Network.AWS.MarketplaceMetering.BatchMeterUsage
  ( -- * Creating a request
    BatchMeterUsage (..),
    mkBatchMeterUsage,

    -- ** Request lenses
    bmuUsageRecords,
    bmuProductCode,

    -- * Destructuring the response
    BatchMeterUsageResponse (..),
    mkBatchMeterUsageResponse,

    -- ** Response lenses
    bmurrsResults,
    bmurrsUnprocessedRecords,
    bmurrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MarketplaceMetering.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A BatchMeterUsageRequest contains UsageRecords, which indicate quantities of usage within your application.
--
-- /See:/ 'mkBatchMeterUsage' smart constructor.
data BatchMeterUsage = BatchMeterUsage'
  { -- | The set of UsageRecords to submit. BatchMeterUsage accepts up to 25 UsageRecords at a time.
    usageRecords :: [Types.UsageRecord],
    -- | Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
    productCode :: Types.ProductCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchMeterUsage' value with any optional fields omitted.
mkBatchMeterUsage ::
  -- | 'productCode'
  Types.ProductCode ->
  BatchMeterUsage
mkBatchMeterUsage productCode =
  BatchMeterUsage' {usageRecords = Core.mempty, productCode}

-- | The set of UsageRecords to submit. BatchMeterUsage accepts up to 25 UsageRecords at a time.
--
-- /Note:/ Consider using 'usageRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmuUsageRecords :: Lens.Lens' BatchMeterUsage [Types.UsageRecord]
bmuUsageRecords = Lens.field @"usageRecords"
{-# DEPRECATED bmuUsageRecords "Use generic-lens or generic-optics with 'usageRecords' instead." #-}

-- | Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmuProductCode :: Lens.Lens' BatchMeterUsage Types.ProductCode
bmuProductCode = Lens.field @"productCode"
{-# DEPRECATED bmuProductCode "Use generic-lens or generic-optics with 'productCode' instead." #-}

instance Core.FromJSON BatchMeterUsage where
  toJSON BatchMeterUsage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UsageRecords" Core..= usageRecords),
            Core.Just ("ProductCode" Core..= productCode)
          ]
      )

instance Core.AWSRequest BatchMeterUsage where
  type Rs BatchMeterUsage = BatchMeterUsageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSMPMeteringService.BatchMeterUsage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchMeterUsageResponse'
            Core.<$> (x Core..:? "Results")
            Core.<*> (x Core..:? "UnprocessedRecords")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the UsageRecords processed by BatchMeterUsage and any records that have failed due to transient error.
--
-- /See:/ 'mkBatchMeterUsageResponse' smart constructor.
data BatchMeterUsageResponse = BatchMeterUsageResponse'
  { -- | Contains all UsageRecords processed by BatchMeterUsage. These records were either honored by AWS Marketplace Metering Service or were invalid.
    results :: Core.Maybe [Types.UsageRecordResult],
    -- | Contains all UsageRecords that were not processed by BatchMeterUsage. This is a list of UsageRecords. You can retry the failed request by making another BatchMeterUsage call with this list as input in the BatchMeterUsageRequest.
    unprocessedRecords :: Core.Maybe [Types.UsageRecord],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchMeterUsageResponse' value with any optional fields omitted.
mkBatchMeterUsageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchMeterUsageResponse
mkBatchMeterUsageResponse responseStatus =
  BatchMeterUsageResponse'
    { results = Core.Nothing,
      unprocessedRecords = Core.Nothing,
      responseStatus
    }

-- | Contains all UsageRecords processed by BatchMeterUsage. These records were either honored by AWS Marketplace Metering Service or were invalid.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmurrsResults :: Lens.Lens' BatchMeterUsageResponse (Core.Maybe [Types.UsageRecordResult])
bmurrsResults = Lens.field @"results"
{-# DEPRECATED bmurrsResults "Use generic-lens or generic-optics with 'results' instead." #-}

-- | Contains all UsageRecords that were not processed by BatchMeterUsage. This is a list of UsageRecords. You can retry the failed request by making another BatchMeterUsage call with this list as input in the BatchMeterUsageRequest.
--
-- /Note:/ Consider using 'unprocessedRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmurrsUnprocessedRecords :: Lens.Lens' BatchMeterUsageResponse (Core.Maybe [Types.UsageRecord])
bmurrsUnprocessedRecords = Lens.field @"unprocessedRecords"
{-# DEPRECATED bmurrsUnprocessedRecords "Use generic-lens or generic-optics with 'unprocessedRecords' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmurrsResponseStatus :: Lens.Lens' BatchMeterUsageResponse Core.Int
bmurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bmurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
