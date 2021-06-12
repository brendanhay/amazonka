{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.MeterUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- API to emit metering records. For identical requests, the API is
-- idempotent. It simply returns the metering record ID.
--
-- MeterUsage is authenticated on the buyer\'s AWS account using
-- credentials from the EC2 instance, ECS task, or EKS pod.
--
-- MeterUsage can optionally include multiple usage allocations, to provide
-- customers with usage data split into buckets by tags that you define (or
-- allow the customer to define).
module Network.AWS.MarketplaceMetering.MeterUsage
  ( -- * Creating a Request
    MeterUsage (..),
    newMeterUsage,

    -- * Request Lenses
    meterUsage_dryRun,
    meterUsage_usageAllocations,
    meterUsage_usageQuantity,
    meterUsage_productCode,
    meterUsage_timestamp,
    meterUsage_usageDimension,

    -- * Destructuring the Response
    MeterUsageResponse (..),
    newMeterUsageResponse,

    -- * Response Lenses
    meterUsageResponse_meteringRecordId,
    meterUsageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newMeterUsage' smart constructor.
data MeterUsage = MeterUsage'
  { -- | Checks whether you have the permissions required for the action, but
    -- does not make the request. If you have the permissions, the request
    -- returns DryRunOperation; otherwise, it returns UnauthorizedException.
    -- Defaults to @false@ if not specified.
    dryRun :: Core.Maybe Core.Bool,
    -- | The set of UsageAllocations to submit.
    --
    -- The sum of all UsageAllocation quantities must equal the UsageQuantity
    -- of the MeterUsage request, and each UsageAllocation must have a unique
    -- set of tags (include no tags).
    usageAllocations :: Core.Maybe (Core.NonEmpty UsageAllocation),
    -- | Consumption value for the hour. Defaults to @0@ if not specified.
    usageQuantity :: Core.Maybe Core.Natural,
    -- | Product code is used to uniquely identify a product in AWS Marketplace.
    -- The product code should be the same as the one used during the
    -- publishing of a new product.
    productCode :: Core.Text,
    -- | Timestamp, in UTC, for which the usage is being reported. Your
    -- application can meter usage for up to one hour in the past. Make sure
    -- the timestamp value is not before the start of the software usage.
    timestamp :: Core.POSIX,
    -- | It will be one of the fcp dimension name provided during the publishing
    -- of the product.
    usageDimension :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MeterUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'meterUsage_dryRun' - Checks whether you have the permissions required for the action, but
-- does not make the request. If you have the permissions, the request
-- returns DryRunOperation; otherwise, it returns UnauthorizedException.
-- Defaults to @false@ if not specified.
--
-- 'usageAllocations', 'meterUsage_usageAllocations' - The set of UsageAllocations to submit.
--
-- The sum of all UsageAllocation quantities must equal the UsageQuantity
-- of the MeterUsage request, and each UsageAllocation must have a unique
-- set of tags (include no tags).
--
-- 'usageQuantity', 'meterUsage_usageQuantity' - Consumption value for the hour. Defaults to @0@ if not specified.
--
-- 'productCode', 'meterUsage_productCode' - Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code should be the same as the one used during the
-- publishing of a new product.
--
-- 'timestamp', 'meterUsage_timestamp' - Timestamp, in UTC, for which the usage is being reported. Your
-- application can meter usage for up to one hour in the past. Make sure
-- the timestamp value is not before the start of the software usage.
--
-- 'usageDimension', 'meterUsage_usageDimension' - It will be one of the fcp dimension name provided during the publishing
-- of the product.
newMeterUsage ::
  -- | 'productCode'
  Core.Text ->
  -- | 'timestamp'
  Core.UTCTime ->
  -- | 'usageDimension'
  Core.Text ->
  MeterUsage
newMeterUsage
  pProductCode_
  pTimestamp_
  pUsageDimension_ =
    MeterUsage'
      { dryRun = Core.Nothing,
        usageAllocations = Core.Nothing,
        usageQuantity = Core.Nothing,
        productCode = pProductCode_,
        timestamp = Core._Time Lens.# pTimestamp_,
        usageDimension = pUsageDimension_
      }

-- | Checks whether you have the permissions required for the action, but
-- does not make the request. If you have the permissions, the request
-- returns DryRunOperation; otherwise, it returns UnauthorizedException.
-- Defaults to @false@ if not specified.
meterUsage_dryRun :: Lens.Lens' MeterUsage (Core.Maybe Core.Bool)
meterUsage_dryRun = Lens.lens (\MeterUsage' {dryRun} -> dryRun) (\s@MeterUsage' {} a -> s {dryRun = a} :: MeterUsage)

-- | The set of UsageAllocations to submit.
--
-- The sum of all UsageAllocation quantities must equal the UsageQuantity
-- of the MeterUsage request, and each UsageAllocation must have a unique
-- set of tags (include no tags).
meterUsage_usageAllocations :: Lens.Lens' MeterUsage (Core.Maybe (Core.NonEmpty UsageAllocation))
meterUsage_usageAllocations = Lens.lens (\MeterUsage' {usageAllocations} -> usageAllocations) (\s@MeterUsage' {} a -> s {usageAllocations = a} :: MeterUsage) Core.. Lens.mapping Lens._Coerce

-- | Consumption value for the hour. Defaults to @0@ if not specified.
meterUsage_usageQuantity :: Lens.Lens' MeterUsage (Core.Maybe Core.Natural)
meterUsage_usageQuantity = Lens.lens (\MeterUsage' {usageQuantity} -> usageQuantity) (\s@MeterUsage' {} a -> s {usageQuantity = a} :: MeterUsage)

-- | Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code should be the same as the one used during the
-- publishing of a new product.
meterUsage_productCode :: Lens.Lens' MeterUsage Core.Text
meterUsage_productCode = Lens.lens (\MeterUsage' {productCode} -> productCode) (\s@MeterUsage' {} a -> s {productCode = a} :: MeterUsage)

-- | Timestamp, in UTC, for which the usage is being reported. Your
-- application can meter usage for up to one hour in the past. Make sure
-- the timestamp value is not before the start of the software usage.
meterUsage_timestamp :: Lens.Lens' MeterUsage Core.UTCTime
meterUsage_timestamp = Lens.lens (\MeterUsage' {timestamp} -> timestamp) (\s@MeterUsage' {} a -> s {timestamp = a} :: MeterUsage) Core.. Core._Time

-- | It will be one of the fcp dimension name provided during the publishing
-- of the product.
meterUsage_usageDimension :: Lens.Lens' MeterUsage Core.Text
meterUsage_usageDimension = Lens.lens (\MeterUsage' {usageDimension} -> usageDimension) (\s@MeterUsage' {} a -> s {usageDimension = a} :: MeterUsage)

instance Core.AWSRequest MeterUsage where
  type AWSResponse MeterUsage = MeterUsageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          MeterUsageResponse'
            Core.<$> (x Core..?> "MeteringRecordId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable MeterUsage

instance Core.NFData MeterUsage

instance Core.ToHeaders MeterUsage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMPMeteringService.MeterUsage" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON MeterUsage where
  toJSON MeterUsage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DryRun" Core..=) Core.<$> dryRun,
            ("UsageAllocations" Core..=)
              Core.<$> usageAllocations,
            ("UsageQuantity" Core..=) Core.<$> usageQuantity,
            Core.Just ("ProductCode" Core..= productCode),
            Core.Just ("Timestamp" Core..= timestamp),
            Core.Just ("UsageDimension" Core..= usageDimension)
          ]
      )

instance Core.ToPath MeterUsage where
  toPath = Core.const "/"

instance Core.ToQuery MeterUsage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newMeterUsageResponse' smart constructor.
data MeterUsageResponse = MeterUsageResponse'
  { -- | Metering record id.
    meteringRecordId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MeterUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meteringRecordId', 'meterUsageResponse_meteringRecordId' - Metering record id.
--
-- 'httpStatus', 'meterUsageResponse_httpStatus' - The response's http status code.
newMeterUsageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  MeterUsageResponse
newMeterUsageResponse pHttpStatus_ =
  MeterUsageResponse'
    { meteringRecordId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metering record id.
meterUsageResponse_meteringRecordId :: Lens.Lens' MeterUsageResponse (Core.Maybe Core.Text)
meterUsageResponse_meteringRecordId = Lens.lens (\MeterUsageResponse' {meteringRecordId} -> meteringRecordId) (\s@MeterUsageResponse' {} a -> s {meteringRecordId = a} :: MeterUsageResponse)

-- | The response's http status code.
meterUsageResponse_httpStatus :: Lens.Lens' MeterUsageResponse Core.Int
meterUsageResponse_httpStatus = Lens.lens (\MeterUsageResponse' {httpStatus} -> httpStatus) (\s@MeterUsageResponse' {} a -> s {httpStatus = a} :: MeterUsageResponse)

instance Core.NFData MeterUsageResponse
