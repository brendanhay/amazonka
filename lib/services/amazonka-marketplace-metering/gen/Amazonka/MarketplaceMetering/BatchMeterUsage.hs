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
-- Module      : Amazonka.MarketplaceMetering.BatchMeterUsage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @BatchMeterUsage@ is called from a SaaS application listed on AWS
-- Marketplace to post metering records for a set of customers.
--
-- For identical requests, the API is idempotent; requests can be retried
-- with the same records or a subset of the input records.
--
-- Every request to @BatchMeterUsage@ is for one product. If you need to
-- meter usage for multiple products, you must make multiple calls to
-- @BatchMeterUsage@.
--
-- Usage records are expected to be submitted as quickly as possible after
-- the event that is being recorded, and are not accepted more than 6 hours
-- after the event.
--
-- @BatchMeterUsage@ can process up to 25 @UsageRecords@ at a time.
--
-- A @UsageRecord@ can optionally include multiple usage allocations, to
-- provide customers with usage data split into buckets by tags that you
-- define (or allow the customer to define).
--
-- @BatchMeterUsage@ returns a list of @UsageRecordResult@ objects, showing
-- the result for each @UsageRecord@, as well as a list of
-- @UnprocessedRecords@, indicating errors in the service side that you
-- should retry.
--
-- @BatchMeterUsage@ requests must be less than 1MB in size.
--
-- For an example of using @BatchMeterUsage@, see
-- <https://docs.aws.amazon.com/marketplace/latest/userguide/saas-code-examples.html#saas-batchmeterusage-example BatchMeterUsage code example>
-- in the /AWS Marketplace Seller Guide/.
module Amazonka.MarketplaceMetering.BatchMeterUsage
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
    batchMeterUsageResponse_results,
    batchMeterUsageResponse_unprocessedRecords,
    batchMeterUsageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MarketplaceMetering.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A @BatchMeterUsageRequest@ contains @UsageRecords@, which indicate
-- quantities of usage within your application.
--
-- /See:/ 'newBatchMeterUsage' smart constructor.
data BatchMeterUsage = BatchMeterUsage'
  { -- | The set of @UsageRecords@ to submit. @BatchMeterUsage@ accepts up to 25
    -- @UsageRecords@ at a time.
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
-- 'usageRecords', 'batchMeterUsage_usageRecords' - The set of @UsageRecords@ to submit. @BatchMeterUsage@ accepts up to 25
-- @UsageRecords@ at a time.
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

-- | The set of @UsageRecords@ to submit. @BatchMeterUsage@ accepts up to 25
-- @UsageRecords@ at a time.
batchMeterUsage_usageRecords :: Lens.Lens' BatchMeterUsage [UsageRecord]
batchMeterUsage_usageRecords = Lens.lens (\BatchMeterUsage' {usageRecords} -> usageRecords) (\s@BatchMeterUsage' {} a -> s {usageRecords = a} :: BatchMeterUsage) Prelude.. Lens.coerced

-- | Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code should be the same as the one used during the
-- publishing of a new product.
batchMeterUsage_productCode :: Lens.Lens' BatchMeterUsage Prelude.Text
batchMeterUsage_productCode = Lens.lens (\BatchMeterUsage' {productCode} -> productCode) (\s@BatchMeterUsage' {} a -> s {productCode = a} :: BatchMeterUsage)

instance Core.AWSRequest BatchMeterUsage where
  type
    AWSResponse BatchMeterUsage =
      BatchMeterUsageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchMeterUsageResponse'
            Prelude.<$> (x Data..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "UnprocessedRecords"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchMeterUsage where
  hashWithSalt _salt BatchMeterUsage' {..} =
    _salt `Prelude.hashWithSalt` usageRecords
      `Prelude.hashWithSalt` productCode

instance Prelude.NFData BatchMeterUsage where
  rnf BatchMeterUsage' {..} =
    Prelude.rnf usageRecords
      `Prelude.seq` Prelude.rnf productCode

instance Data.ToHeaders BatchMeterUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMPMeteringService.BatchMeterUsage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchMeterUsage where
  toJSON BatchMeterUsage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UsageRecords" Data..= usageRecords),
            Prelude.Just ("ProductCode" Data..= productCode)
          ]
      )

instance Data.ToPath BatchMeterUsage where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchMeterUsage where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the @UsageRecords@ processed by @BatchMeterUsage@ and any
-- records that have failed due to transient error.
--
-- /See:/ 'newBatchMeterUsageResponse' smart constructor.
data BatchMeterUsageResponse = BatchMeterUsageResponse'
  { -- | Contains all @UsageRecords@ processed by @BatchMeterUsage@. These
    -- records were either honored by AWS Marketplace Metering Service or were
    -- invalid. Invalid records should be fixed before being resubmitted.
    results :: Prelude.Maybe [UsageRecordResult],
    -- | Contains all @UsageRecords@ that were not processed by
    -- @BatchMeterUsage@. This is a list of @UsageRecords@. You can retry the
    -- failed request by making another @BatchMeterUsage@ call with this list
    -- as input in the @BatchMeterUsageRequest@.
    unprocessedRecords :: Prelude.Maybe [UsageRecord],
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
-- 'results', 'batchMeterUsageResponse_results' - Contains all @UsageRecords@ processed by @BatchMeterUsage@. These
-- records were either honored by AWS Marketplace Metering Service or were
-- invalid. Invalid records should be fixed before being resubmitted.
--
-- 'unprocessedRecords', 'batchMeterUsageResponse_unprocessedRecords' - Contains all @UsageRecords@ that were not processed by
-- @BatchMeterUsage@. This is a list of @UsageRecords@. You can retry the
-- failed request by making another @BatchMeterUsage@ call with this list
-- as input in the @BatchMeterUsageRequest@.
--
-- 'httpStatus', 'batchMeterUsageResponse_httpStatus' - The response's http status code.
newBatchMeterUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchMeterUsageResponse
newBatchMeterUsageResponse pHttpStatus_ =
  BatchMeterUsageResponse'
    { results = Prelude.Nothing,
      unprocessedRecords = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains all @UsageRecords@ processed by @BatchMeterUsage@. These
-- records were either honored by AWS Marketplace Metering Service or were
-- invalid. Invalid records should be fixed before being resubmitted.
batchMeterUsageResponse_results :: Lens.Lens' BatchMeterUsageResponse (Prelude.Maybe [UsageRecordResult])
batchMeterUsageResponse_results = Lens.lens (\BatchMeterUsageResponse' {results} -> results) (\s@BatchMeterUsageResponse' {} a -> s {results = a} :: BatchMeterUsageResponse) Prelude.. Lens.mapping Lens.coerced

-- | Contains all @UsageRecords@ that were not processed by
-- @BatchMeterUsage@. This is a list of @UsageRecords@. You can retry the
-- failed request by making another @BatchMeterUsage@ call with this list
-- as input in the @BatchMeterUsageRequest@.
batchMeterUsageResponse_unprocessedRecords :: Lens.Lens' BatchMeterUsageResponse (Prelude.Maybe [UsageRecord])
batchMeterUsageResponse_unprocessedRecords = Lens.lens (\BatchMeterUsageResponse' {unprocessedRecords} -> unprocessedRecords) (\s@BatchMeterUsageResponse' {} a -> s {unprocessedRecords = a} :: BatchMeterUsageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchMeterUsageResponse_httpStatus :: Lens.Lens' BatchMeterUsageResponse Prelude.Int
batchMeterUsageResponse_httpStatus = Lens.lens (\BatchMeterUsageResponse' {httpStatus} -> httpStatus) (\s@BatchMeterUsageResponse' {} a -> s {httpStatus = a} :: BatchMeterUsageResponse)

instance Prelude.NFData BatchMeterUsageResponse where
  rnf BatchMeterUsageResponse' {..} =
    Prelude.rnf results
      `Prelude.seq` Prelude.rnf unprocessedRecords
      `Prelude.seq` Prelude.rnf httpStatus
