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
-- Module      : Network.AWS.MarketplaceMetering.BatchMeterUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- BatchMeterUsage is called from a SaaS application listed on the AWS
-- Marketplace to post metering records for a set of customers.
--
-- For identical requests, the API is idempotent; requests can be retried
-- with the same records or a subset of the input records.
--
-- Every request to BatchMeterUsage is for one product. If you need to
-- meter usage for multiple products, you must make multiple calls to
-- BatchMeterUsage.
--
-- BatchMeterUsage can process up to 25 UsageRecords at a time.
--
-- A UsageRecord can optionally include multiple usage allocations, to
-- provide customers with usagedata split into buckets by tags that you
-- define (or allow the customer to define).
--
-- BatchMeterUsage requests must be less than 1MB in size.
module Network.AWS.MarketplaceMetering.BatchMeterUsage
  ( -- * Creating a Request
    BatchMeterUsage (..),
    newBatchMeterUsage,

    -- * Request Lenses
    batchMeterUsage_usageRecords,
    batchMeterUsage_productCode,

    -- * Destructuring the Response
    BatchMeterUsageResponse (..),
    newBatchMeterUsageResponse,

    -- * Response Lenses
    batchMeterUsageResponse_unprocessedRecords,
    batchMeterUsageResponse_results,
    batchMeterUsageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A BatchMeterUsageRequest contains UsageRecords, which indicate
-- quantities of usage within your application.
--
-- /See:/ 'newBatchMeterUsage' smart constructor.
data BatchMeterUsage = BatchMeterUsage'
  { -- | The set of UsageRecords to submit. BatchMeterUsage accepts up to 25
    -- UsageRecords at a time.
    usageRecords :: [UsageRecord],
    -- | Product code is used to uniquely identify a product in AWS Marketplace.
    -- The product code should be the same as the one used during the
    -- publishing of a new product.
    productCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchMeterUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageRecords', 'batchMeterUsage_usageRecords' - The set of UsageRecords to submit. BatchMeterUsage accepts up to 25
-- UsageRecords at a time.
--
-- 'productCode', 'batchMeterUsage_productCode' - Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code should be the same as the one used during the
-- publishing of a new product.
newBatchMeterUsage ::
  -- | 'productCode'
  Prelude.Text ->
  BatchMeterUsage
newBatchMeterUsage pProductCode_ =
  BatchMeterUsage'
    { usageRecords = Prelude.mempty,
      productCode = pProductCode_
    }

-- | The set of UsageRecords to submit. BatchMeterUsage accepts up to 25
-- UsageRecords at a time.
batchMeterUsage_usageRecords :: Lens.Lens' BatchMeterUsage [UsageRecord]
batchMeterUsage_usageRecords = Lens.lens (\BatchMeterUsage' {usageRecords} -> usageRecords) (\s@BatchMeterUsage' {} a -> s {usageRecords = a} :: BatchMeterUsage) Prelude.. Lens._Coerce

-- | Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code should be the same as the one used during the
-- publishing of a new product.
batchMeterUsage_productCode :: Lens.Lens' BatchMeterUsage Prelude.Text
batchMeterUsage_productCode = Lens.lens (\BatchMeterUsage' {productCode} -> productCode) (\s@BatchMeterUsage' {} a -> s {productCode = a} :: BatchMeterUsage)

instance Core.AWSRequest BatchMeterUsage where
  type
    AWSResponse BatchMeterUsage =
      BatchMeterUsageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchMeterUsageResponse'
            Prelude.<$> ( x Core..?> "UnprocessedRecords"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchMeterUsage

instance Prelude.NFData BatchMeterUsage

instance Core.ToHeaders BatchMeterUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMPMeteringService.BatchMeterUsage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchMeterUsage where
  toJSON BatchMeterUsage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UsageRecords" Core..= usageRecords),
            Prelude.Just ("ProductCode" Core..= productCode)
          ]
      )

instance Core.ToPath BatchMeterUsage where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchMeterUsage where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the UsageRecords processed by BatchMeterUsage and any records
-- that have failed due to transient error.
--
-- /See:/ 'newBatchMeterUsageResponse' smart constructor.
data BatchMeterUsageResponse = BatchMeterUsageResponse'
  { -- | Contains all UsageRecords that were not processed by BatchMeterUsage.
    -- This is a list of UsageRecords. You can retry the failed request by
    -- making another BatchMeterUsage call with this list as input in the
    -- BatchMeterUsageRequest.
    unprocessedRecords :: Prelude.Maybe [UsageRecord],
    -- | Contains all UsageRecords processed by BatchMeterUsage. These records
    -- were either honored by AWS Marketplace Metering Service or were invalid.
    results :: Prelude.Maybe [UsageRecordResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchMeterUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unprocessedRecords', 'batchMeterUsageResponse_unprocessedRecords' - Contains all UsageRecords that were not processed by BatchMeterUsage.
-- This is a list of UsageRecords. You can retry the failed request by
-- making another BatchMeterUsage call with this list as input in the
-- BatchMeterUsageRequest.
--
-- 'results', 'batchMeterUsageResponse_results' - Contains all UsageRecords processed by BatchMeterUsage. These records
-- were either honored by AWS Marketplace Metering Service or were invalid.
--
-- 'httpStatus', 'batchMeterUsageResponse_httpStatus' - The response's http status code.
newBatchMeterUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchMeterUsageResponse
newBatchMeterUsageResponse pHttpStatus_ =
  BatchMeterUsageResponse'
    { unprocessedRecords =
        Prelude.Nothing,
      results = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains all UsageRecords that were not processed by BatchMeterUsage.
-- This is a list of UsageRecords. You can retry the failed request by
-- making another BatchMeterUsage call with this list as input in the
-- BatchMeterUsageRequest.
batchMeterUsageResponse_unprocessedRecords :: Lens.Lens' BatchMeterUsageResponse (Prelude.Maybe [UsageRecord])
batchMeterUsageResponse_unprocessedRecords = Lens.lens (\BatchMeterUsageResponse' {unprocessedRecords} -> unprocessedRecords) (\s@BatchMeterUsageResponse' {} a -> s {unprocessedRecords = a} :: BatchMeterUsageResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Contains all UsageRecords processed by BatchMeterUsage. These records
-- were either honored by AWS Marketplace Metering Service or were invalid.
batchMeterUsageResponse_results :: Lens.Lens' BatchMeterUsageResponse (Prelude.Maybe [UsageRecordResult])
batchMeterUsageResponse_results = Lens.lens (\BatchMeterUsageResponse' {results} -> results) (\s@BatchMeterUsageResponse' {} a -> s {results = a} :: BatchMeterUsageResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchMeterUsageResponse_httpStatus :: Lens.Lens' BatchMeterUsageResponse Prelude.Int
batchMeterUsageResponse_httpStatus = Lens.lens (\BatchMeterUsageResponse' {httpStatus} -> httpStatus) (\s@BatchMeterUsageResponse' {} a -> s {httpStatus = a} :: BatchMeterUsageResponse)

instance Prelude.NFData BatchMeterUsageResponse
