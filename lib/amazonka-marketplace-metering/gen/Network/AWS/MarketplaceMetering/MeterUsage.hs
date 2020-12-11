{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    muUsageQuantity,
    muUsageAllocations,
    muDryRun,
    muProductCode,
    muTimestamp,
    muUsageDimension,

    -- * Destructuring the response
    MeterUsageResponse (..),
    mkMeterUsageResponse,

    -- ** Response lenses
    mursMeteringRecordId,
    mursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkMeterUsage' smart constructor.
data MeterUsage = MeterUsage'
  { usageQuantity ::
      Lude.Maybe Lude.Natural,
    usageAllocations :: Lude.Maybe (Lude.NonEmpty UsageAllocation),
    dryRun :: Lude.Maybe Lude.Bool,
    productCode :: Lude.Text,
    timestamp :: Lude.Timestamp,
    usageDimension :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MeterUsage' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the permissions required for the action, but does not make the request. If you have the permissions, the request returns DryRunOperation; otherwise, it returns UnauthorizedException. Defaults to @false@ if not specified.
-- * 'productCode' - Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
-- * 'timestamp' - Timestamp, in UTC, for which the usage is being reported. Your application can meter usage for up to one hour in the past. Make sure the timestamp value is not before the start of the software usage.
-- * 'usageAllocations' - The set of UsageAllocations to submit.
--
-- The sum of all UsageAllocation quantities must equal the UsageQuantity of the MeterUsage request, and each UsageAllocation must have a unique set of tags (include no tags).
-- * 'usageDimension' - It will be one of the fcp dimension name provided during the publishing of the product.
-- * 'usageQuantity' - Consumption value for the hour. Defaults to @0@ if not specified.
mkMeterUsage ::
  -- | 'productCode'
  Lude.Text ->
  -- | 'timestamp'
  Lude.Timestamp ->
  -- | 'usageDimension'
  Lude.Text ->
  MeterUsage
mkMeterUsage pProductCode_ pTimestamp_ pUsageDimension_ =
  MeterUsage'
    { usageQuantity = Lude.Nothing,
      usageAllocations = Lude.Nothing,
      dryRun = Lude.Nothing,
      productCode = pProductCode_,
      timestamp = pTimestamp_,
      usageDimension = pUsageDimension_
    }

-- | Consumption value for the hour. Defaults to @0@ if not specified.
--
-- /Note:/ Consider using 'usageQuantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muUsageQuantity :: Lens.Lens' MeterUsage (Lude.Maybe Lude.Natural)
muUsageQuantity = Lens.lens (usageQuantity :: MeterUsage -> Lude.Maybe Lude.Natural) (\s a -> s {usageQuantity = a} :: MeterUsage)
{-# DEPRECATED muUsageQuantity "Use generic-lens or generic-optics with 'usageQuantity' instead." #-}

-- | The set of UsageAllocations to submit.
--
-- The sum of all UsageAllocation quantities must equal the UsageQuantity of the MeterUsage request, and each UsageAllocation must have a unique set of tags (include no tags).
--
-- /Note:/ Consider using 'usageAllocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muUsageAllocations :: Lens.Lens' MeterUsage (Lude.Maybe (Lude.NonEmpty UsageAllocation))
muUsageAllocations = Lens.lens (usageAllocations :: MeterUsage -> Lude.Maybe (Lude.NonEmpty UsageAllocation)) (\s a -> s {usageAllocations = a} :: MeterUsage)
{-# DEPRECATED muUsageAllocations "Use generic-lens or generic-optics with 'usageAllocations' instead." #-}

-- | Checks whether you have the permissions required for the action, but does not make the request. If you have the permissions, the request returns DryRunOperation; otherwise, it returns UnauthorizedException. Defaults to @false@ if not specified.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muDryRun :: Lens.Lens' MeterUsage (Lude.Maybe Lude.Bool)
muDryRun = Lens.lens (dryRun :: MeterUsage -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: MeterUsage)
{-# DEPRECATED muDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muProductCode :: Lens.Lens' MeterUsage Lude.Text
muProductCode = Lens.lens (productCode :: MeterUsage -> Lude.Text) (\s a -> s {productCode = a} :: MeterUsage)
{-# DEPRECATED muProductCode "Use generic-lens or generic-optics with 'productCode' instead." #-}

-- | Timestamp, in UTC, for which the usage is being reported. Your application can meter usage for up to one hour in the past. Make sure the timestamp value is not before the start of the software usage.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muTimestamp :: Lens.Lens' MeterUsage Lude.Timestamp
muTimestamp = Lens.lens (timestamp :: MeterUsage -> Lude.Timestamp) (\s a -> s {timestamp = a} :: MeterUsage)
{-# DEPRECATED muTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | It will be one of the fcp dimension name provided during the publishing of the product.
--
-- /Note:/ Consider using 'usageDimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muUsageDimension :: Lens.Lens' MeterUsage Lude.Text
muUsageDimension = Lens.lens (usageDimension :: MeterUsage -> Lude.Text) (\s a -> s {usageDimension = a} :: MeterUsage)
{-# DEPRECATED muUsageDimension "Use generic-lens or generic-optics with 'usageDimension' instead." #-}

instance Lude.AWSRequest MeterUsage where
  type Rs MeterUsage = MeterUsageResponse
  request = Req.postJSON marketplaceMeteringService
  response =
    Res.receiveJSON
      ( \s h x ->
          MeterUsageResponse'
            Lude.<$> (x Lude..?> "MeteringRecordId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders MeterUsage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMPMeteringService.MeterUsage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON MeterUsage where
  toJSON MeterUsage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UsageQuantity" Lude..=) Lude.<$> usageQuantity,
            ("UsageAllocations" Lude..=) Lude.<$> usageAllocations,
            ("DryRun" Lude..=) Lude.<$> dryRun,
            Lude.Just ("ProductCode" Lude..= productCode),
            Lude.Just ("Timestamp" Lude..= timestamp),
            Lude.Just ("UsageDimension" Lude..= usageDimension)
          ]
      )

instance Lude.ToPath MeterUsage where
  toPath = Lude.const "/"

instance Lude.ToQuery MeterUsage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkMeterUsageResponse' smart constructor.
data MeterUsageResponse = MeterUsageResponse'
  { meteringRecordId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MeterUsageResponse' with the minimum fields required to make a request.
--
-- * 'meteringRecordId' - Metering record id.
-- * 'responseStatus' - The response status code.
mkMeterUsageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  MeterUsageResponse
mkMeterUsageResponse pResponseStatus_ =
  MeterUsageResponse'
    { meteringRecordId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Metering record id.
--
-- /Note:/ Consider using 'meteringRecordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mursMeteringRecordId :: Lens.Lens' MeterUsageResponse (Lude.Maybe Lude.Text)
mursMeteringRecordId = Lens.lens (meteringRecordId :: MeterUsageResponse -> Lude.Maybe Lude.Text) (\s a -> s {meteringRecordId = a} :: MeterUsageResponse)
{-# DEPRECATED mursMeteringRecordId "Use generic-lens or generic-optics with 'meteringRecordId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mursResponseStatus :: Lens.Lens' MeterUsageResponse Lude.Int
mursResponseStatus = Lens.lens (responseStatus :: MeterUsageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: MeterUsageResponse)
{-# DEPRECATED mursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
