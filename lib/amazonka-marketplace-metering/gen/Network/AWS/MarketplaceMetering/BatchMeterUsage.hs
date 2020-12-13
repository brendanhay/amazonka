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
    bmursResults,
    bmursUnprocessedRecords,
    bmursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A BatchMeterUsageRequest contains UsageRecords, which indicate quantities of usage within your application.
--
-- /See:/ 'mkBatchMeterUsage' smart constructor.
data BatchMeterUsage = BatchMeterUsage'
  { -- | The set of UsageRecords to submit. BatchMeterUsage accepts up to 25 UsageRecords at a time.
    usageRecords :: [UsageRecord],
    -- | Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
    productCode :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchMeterUsage' with the minimum fields required to make a request.
--
-- * 'usageRecords' - The set of UsageRecords to submit. BatchMeterUsage accepts up to 25 UsageRecords at a time.
-- * 'productCode' - Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
mkBatchMeterUsage ::
  -- | 'productCode'
  Lude.Text ->
  BatchMeterUsage
mkBatchMeterUsage pProductCode_ =
  BatchMeterUsage'
    { usageRecords = Lude.mempty,
      productCode = pProductCode_
    }

-- | The set of UsageRecords to submit. BatchMeterUsage accepts up to 25 UsageRecords at a time.
--
-- /Note:/ Consider using 'usageRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmuUsageRecords :: Lens.Lens' BatchMeterUsage [UsageRecord]
bmuUsageRecords = Lens.lens (usageRecords :: BatchMeterUsage -> [UsageRecord]) (\s a -> s {usageRecords = a} :: BatchMeterUsage)
{-# DEPRECATED bmuUsageRecords "Use generic-lens or generic-optics with 'usageRecords' instead." #-}

-- | Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmuProductCode :: Lens.Lens' BatchMeterUsage Lude.Text
bmuProductCode = Lens.lens (productCode :: BatchMeterUsage -> Lude.Text) (\s a -> s {productCode = a} :: BatchMeterUsage)
{-# DEPRECATED bmuProductCode "Use generic-lens or generic-optics with 'productCode' instead." #-}

instance Lude.AWSRequest BatchMeterUsage where
  type Rs BatchMeterUsage = BatchMeterUsageResponse
  request = Req.postJSON marketplaceMeteringService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchMeterUsageResponse'
            Lude.<$> (x Lude..?> "Results" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "UnprocessedRecords" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchMeterUsage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMPMeteringService.BatchMeterUsage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchMeterUsage where
  toJSON BatchMeterUsage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UsageRecords" Lude..= usageRecords),
            Lude.Just ("ProductCode" Lude..= productCode)
          ]
      )

instance Lude.ToPath BatchMeterUsage where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchMeterUsage where
  toQuery = Lude.const Lude.mempty

-- | Contains the UsageRecords processed by BatchMeterUsage and any records that have failed due to transient error.
--
-- /See:/ 'mkBatchMeterUsageResponse' smart constructor.
data BatchMeterUsageResponse = BatchMeterUsageResponse'
  { -- | Contains all UsageRecords processed by BatchMeterUsage. These records were either honored by AWS Marketplace Metering Service or were invalid.
    results :: Lude.Maybe [UsageRecordResult],
    -- | Contains all UsageRecords that were not processed by BatchMeterUsage. This is a list of UsageRecords. You can retry the failed request by making another BatchMeterUsage call with this list as input in the BatchMeterUsageRequest.
    unprocessedRecords :: Lude.Maybe [UsageRecord],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchMeterUsageResponse' with the minimum fields required to make a request.
--
-- * 'results' - Contains all UsageRecords processed by BatchMeterUsage. These records were either honored by AWS Marketplace Metering Service or were invalid.
-- * 'unprocessedRecords' - Contains all UsageRecords that were not processed by BatchMeterUsage. This is a list of UsageRecords. You can retry the failed request by making another BatchMeterUsage call with this list as input in the BatchMeterUsageRequest.
-- * 'responseStatus' - The response status code.
mkBatchMeterUsageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchMeterUsageResponse
mkBatchMeterUsageResponse pResponseStatus_ =
  BatchMeterUsageResponse'
    { results = Lude.Nothing,
      unprocessedRecords = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains all UsageRecords processed by BatchMeterUsage. These records were either honored by AWS Marketplace Metering Service or were invalid.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmursResults :: Lens.Lens' BatchMeterUsageResponse (Lude.Maybe [UsageRecordResult])
bmursResults = Lens.lens (results :: BatchMeterUsageResponse -> Lude.Maybe [UsageRecordResult]) (\s a -> s {results = a} :: BatchMeterUsageResponse)
{-# DEPRECATED bmursResults "Use generic-lens or generic-optics with 'results' instead." #-}

-- | Contains all UsageRecords that were not processed by BatchMeterUsage. This is a list of UsageRecords. You can retry the failed request by making another BatchMeterUsage call with this list as input in the BatchMeterUsageRequest.
--
-- /Note:/ Consider using 'unprocessedRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmursUnprocessedRecords :: Lens.Lens' BatchMeterUsageResponse (Lude.Maybe [UsageRecord])
bmursUnprocessedRecords = Lens.lens (unprocessedRecords :: BatchMeterUsageResponse -> Lude.Maybe [UsageRecord]) (\s a -> s {unprocessedRecords = a} :: BatchMeterUsageResponse)
{-# DEPRECATED bmursUnprocessedRecords "Use generic-lens or generic-optics with 'unprocessedRecords' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmursResponseStatus :: Lens.Lens' BatchMeterUsageResponse Lude.Int
bmursResponseStatus = Lens.lens (responseStatus :: BatchMeterUsageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchMeterUsageResponse)
{-# DEPRECATED bmursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
