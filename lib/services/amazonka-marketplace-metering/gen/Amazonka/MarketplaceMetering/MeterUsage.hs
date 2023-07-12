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
-- Module      : Amazonka.MarketplaceMetering.MeterUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- API to emit metering records. For identical requests, the API is
-- idempotent. It simply returns the metering record ID.
--
-- @MeterUsage@ is authenticated on the buyer\'s AWS account using
-- credentials from the EC2 instance, ECS task, or EKS pod.
--
-- @MeterUsage@ can optionally include multiple usage allocations, to
-- provide customers with usage data split into buckets by tags that you
-- define (or allow the customer to define).
--
-- Usage records are expected to be submitted as quickly as possible after
-- the event that is being recorded, and are not accepted more than 6 hours
-- after the event.
module Amazonka.MarketplaceMetering.MeterUsage
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MarketplaceMetering.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newMeterUsage' smart constructor.
data MeterUsage = MeterUsage'
  { -- | Checks whether you have the permissions required for the action, but
    -- does not make the request. If you have the permissions, the request
    -- returns @DryRunOperation@; otherwise, it returns
    -- @UnauthorizedException@. Defaults to @false@ if not specified.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The set of @UsageAllocations@ to submit.
    --
    -- The sum of all @UsageAllocation@ quantities must equal the
    -- @UsageQuantity@ of the @MeterUsage@ request, and each @UsageAllocation@
    -- must have a unique set of tags (include no tags).
    usageAllocations :: Prelude.Maybe (Prelude.NonEmpty UsageAllocation),
    -- | Consumption value for the hour. Defaults to @0@ if not specified.
    usageQuantity :: Prelude.Maybe Prelude.Natural,
    -- | Product code is used to uniquely identify a product in AWS Marketplace.
    -- The product code should be the same as the one used during the
    -- publishing of a new product.
    productCode :: Prelude.Text,
    -- | Timestamp, in UTC, for which the usage is being reported. Your
    -- application can meter usage for up to one hour in the past. Make sure
    -- the @timestamp@ value is not before the start of the software usage.
    timestamp :: Data.POSIX,
    -- | It will be one of the fcp dimension name provided during the publishing
    -- of the product.
    usageDimension :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- returns @DryRunOperation@; otherwise, it returns
-- @UnauthorizedException@. Defaults to @false@ if not specified.
--
-- 'usageAllocations', 'meterUsage_usageAllocations' - The set of @UsageAllocations@ to submit.
--
-- The sum of all @UsageAllocation@ quantities must equal the
-- @UsageQuantity@ of the @MeterUsage@ request, and each @UsageAllocation@
-- must have a unique set of tags (include no tags).
--
-- 'usageQuantity', 'meterUsage_usageQuantity' - Consumption value for the hour. Defaults to @0@ if not specified.
--
-- 'productCode', 'meterUsage_productCode' - Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code should be the same as the one used during the
-- publishing of a new product.
--
-- 'timestamp', 'meterUsage_timestamp' - Timestamp, in UTC, for which the usage is being reported. Your
-- application can meter usage for up to one hour in the past. Make sure
-- the @timestamp@ value is not before the start of the software usage.
--
-- 'usageDimension', 'meterUsage_usageDimension' - It will be one of the fcp dimension name provided during the publishing
-- of the product.
newMeterUsage ::
  -- | 'productCode'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  -- | 'usageDimension'
  Prelude.Text ->
  MeterUsage
newMeterUsage
  pProductCode_
  pTimestamp_
  pUsageDimension_ =
    MeterUsage'
      { dryRun = Prelude.Nothing,
        usageAllocations = Prelude.Nothing,
        usageQuantity = Prelude.Nothing,
        productCode = pProductCode_,
        timestamp = Data._Time Lens.# pTimestamp_,
        usageDimension = pUsageDimension_
      }

-- | Checks whether you have the permissions required for the action, but
-- does not make the request. If you have the permissions, the request
-- returns @DryRunOperation@; otherwise, it returns
-- @UnauthorizedException@. Defaults to @false@ if not specified.
meterUsage_dryRun :: Lens.Lens' MeterUsage (Prelude.Maybe Prelude.Bool)
meterUsage_dryRun = Lens.lens (\MeterUsage' {dryRun} -> dryRun) (\s@MeterUsage' {} a -> s {dryRun = a} :: MeterUsage)

-- | The set of @UsageAllocations@ to submit.
--
-- The sum of all @UsageAllocation@ quantities must equal the
-- @UsageQuantity@ of the @MeterUsage@ request, and each @UsageAllocation@
-- must have a unique set of tags (include no tags).
meterUsage_usageAllocations :: Lens.Lens' MeterUsage (Prelude.Maybe (Prelude.NonEmpty UsageAllocation))
meterUsage_usageAllocations = Lens.lens (\MeterUsage' {usageAllocations} -> usageAllocations) (\s@MeterUsage' {} a -> s {usageAllocations = a} :: MeterUsage) Prelude.. Lens.mapping Lens.coerced

-- | Consumption value for the hour. Defaults to @0@ if not specified.
meterUsage_usageQuantity :: Lens.Lens' MeterUsage (Prelude.Maybe Prelude.Natural)
meterUsage_usageQuantity = Lens.lens (\MeterUsage' {usageQuantity} -> usageQuantity) (\s@MeterUsage' {} a -> s {usageQuantity = a} :: MeterUsage)

-- | Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code should be the same as the one used during the
-- publishing of a new product.
meterUsage_productCode :: Lens.Lens' MeterUsage Prelude.Text
meterUsage_productCode = Lens.lens (\MeterUsage' {productCode} -> productCode) (\s@MeterUsage' {} a -> s {productCode = a} :: MeterUsage)

-- | Timestamp, in UTC, for which the usage is being reported. Your
-- application can meter usage for up to one hour in the past. Make sure
-- the @timestamp@ value is not before the start of the software usage.
meterUsage_timestamp :: Lens.Lens' MeterUsage Prelude.UTCTime
meterUsage_timestamp = Lens.lens (\MeterUsage' {timestamp} -> timestamp) (\s@MeterUsage' {} a -> s {timestamp = a} :: MeterUsage) Prelude.. Data._Time

-- | It will be one of the fcp dimension name provided during the publishing
-- of the product.
meterUsage_usageDimension :: Lens.Lens' MeterUsage Prelude.Text
meterUsage_usageDimension = Lens.lens (\MeterUsage' {usageDimension} -> usageDimension) (\s@MeterUsage' {} a -> s {usageDimension = a} :: MeterUsage)

instance Core.AWSRequest MeterUsage where
  type AWSResponse MeterUsage = MeterUsageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          MeterUsageResponse'
            Prelude.<$> (x Data..?> "MeteringRecordId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable MeterUsage where
  hashWithSalt _salt MeterUsage' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` usageAllocations
      `Prelude.hashWithSalt` usageQuantity
      `Prelude.hashWithSalt` productCode
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` usageDimension

instance Prelude.NFData MeterUsage where
  rnf MeterUsage' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf usageAllocations
      `Prelude.seq` Prelude.rnf usageQuantity
      `Prelude.seq` Prelude.rnf productCode
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf usageDimension

instance Data.ToHeaders MeterUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMPMeteringService.MeterUsage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON MeterUsage where
  toJSON MeterUsage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DryRun" Data..=) Prelude.<$> dryRun,
            ("UsageAllocations" Data..=)
              Prelude.<$> usageAllocations,
            ("UsageQuantity" Data..=) Prelude.<$> usageQuantity,
            Prelude.Just ("ProductCode" Data..= productCode),
            Prelude.Just ("Timestamp" Data..= timestamp),
            Prelude.Just
              ("UsageDimension" Data..= usageDimension)
          ]
      )

instance Data.ToPath MeterUsage where
  toPath = Prelude.const "/"

instance Data.ToQuery MeterUsage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newMeterUsageResponse' smart constructor.
data MeterUsageResponse = MeterUsageResponse'
  { -- | Metering record id.
    meteringRecordId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  MeterUsageResponse
newMeterUsageResponse pHttpStatus_ =
  MeterUsageResponse'
    { meteringRecordId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metering record id.
meterUsageResponse_meteringRecordId :: Lens.Lens' MeterUsageResponse (Prelude.Maybe Prelude.Text)
meterUsageResponse_meteringRecordId = Lens.lens (\MeterUsageResponse' {meteringRecordId} -> meteringRecordId) (\s@MeterUsageResponse' {} a -> s {meteringRecordId = a} :: MeterUsageResponse)

-- | The response's http status code.
meterUsageResponse_httpStatus :: Lens.Lens' MeterUsageResponse Prelude.Int
meterUsageResponse_httpStatus = Lens.lens (\MeterUsageResponse' {httpStatus} -> httpStatus) (\s@MeterUsageResponse' {} a -> s {httpStatus = a} :: MeterUsageResponse)

instance Prelude.NFData MeterUsageResponse where
  rnf MeterUsageResponse' {..} =
    Prelude.rnf meteringRecordId
      `Prelude.seq` Prelude.rnf httpStatus
