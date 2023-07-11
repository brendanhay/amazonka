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
-- Module      : Amazonka.SecurityHub.BatchEnableStandards
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the standards specified by the provided @StandardsArn@. To
-- obtain the ARN for a standard, use the @DescribeStandards@ operation.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards.html Security Standards>
-- section of the /Security Hub User Guide/.
module Amazonka.SecurityHub.BatchEnableStandards
  ( -- * Creating a Request
    BatchEnableStandards (..),
    newBatchEnableStandards,

    -- * Request Lenses
    batchEnableStandards_standardsSubscriptionRequests,

    -- * Destructuring the Response
    BatchEnableStandardsResponse (..),
    newBatchEnableStandardsResponse,

    -- * Response Lenses
    batchEnableStandardsResponse_standardsSubscriptions,
    batchEnableStandardsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newBatchEnableStandards' smart constructor.
data BatchEnableStandards = BatchEnableStandards'
  { -- | The list of standards checks to enable.
    standardsSubscriptionRequests :: Prelude.NonEmpty StandardsSubscriptionRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchEnableStandards' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardsSubscriptionRequests', 'batchEnableStandards_standardsSubscriptionRequests' - The list of standards checks to enable.
newBatchEnableStandards ::
  -- | 'standardsSubscriptionRequests'
  Prelude.NonEmpty StandardsSubscriptionRequest ->
  BatchEnableStandards
newBatchEnableStandards
  pStandardsSubscriptionRequests_ =
    BatchEnableStandards'
      { standardsSubscriptionRequests =
          Lens.coerced Lens.# pStandardsSubscriptionRequests_
      }

-- | The list of standards checks to enable.
batchEnableStandards_standardsSubscriptionRequests :: Lens.Lens' BatchEnableStandards (Prelude.NonEmpty StandardsSubscriptionRequest)
batchEnableStandards_standardsSubscriptionRequests = Lens.lens (\BatchEnableStandards' {standardsSubscriptionRequests} -> standardsSubscriptionRequests) (\s@BatchEnableStandards' {} a -> s {standardsSubscriptionRequests = a} :: BatchEnableStandards) Prelude.. Lens.coerced

instance Core.AWSRequest BatchEnableStandards where
  type
    AWSResponse BatchEnableStandards =
      BatchEnableStandardsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchEnableStandardsResponse'
            Prelude.<$> ( x
                            Data..?> "StandardsSubscriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchEnableStandards where
  hashWithSalt _salt BatchEnableStandards' {..} =
    _salt
      `Prelude.hashWithSalt` standardsSubscriptionRequests

instance Prelude.NFData BatchEnableStandards where
  rnf BatchEnableStandards' {..} =
    Prelude.rnf standardsSubscriptionRequests

instance Data.ToHeaders BatchEnableStandards where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchEnableStandards where
  toJSON BatchEnableStandards' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StandardsSubscriptionRequests"
                  Data..= standardsSubscriptionRequests
              )
          ]
      )

instance Data.ToPath BatchEnableStandards where
  toPath = Prelude.const "/standards/register"

instance Data.ToQuery BatchEnableStandards where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchEnableStandardsResponse' smart constructor.
data BatchEnableStandardsResponse = BatchEnableStandardsResponse'
  { -- | The details of the standards subscriptions that were enabled.
    standardsSubscriptions :: Prelude.Maybe [StandardsSubscription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchEnableStandardsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardsSubscriptions', 'batchEnableStandardsResponse_standardsSubscriptions' - The details of the standards subscriptions that were enabled.
--
-- 'httpStatus', 'batchEnableStandardsResponse_httpStatus' - The response's http status code.
newBatchEnableStandardsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchEnableStandardsResponse
newBatchEnableStandardsResponse pHttpStatus_ =
  BatchEnableStandardsResponse'
    { standardsSubscriptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the standards subscriptions that were enabled.
batchEnableStandardsResponse_standardsSubscriptions :: Lens.Lens' BatchEnableStandardsResponse (Prelude.Maybe [StandardsSubscription])
batchEnableStandardsResponse_standardsSubscriptions = Lens.lens (\BatchEnableStandardsResponse' {standardsSubscriptions} -> standardsSubscriptions) (\s@BatchEnableStandardsResponse' {} a -> s {standardsSubscriptions = a} :: BatchEnableStandardsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchEnableStandardsResponse_httpStatus :: Lens.Lens' BatchEnableStandardsResponse Prelude.Int
batchEnableStandardsResponse_httpStatus = Lens.lens (\BatchEnableStandardsResponse' {httpStatus} -> httpStatus) (\s@BatchEnableStandardsResponse' {} a -> s {httpStatus = a} :: BatchEnableStandardsResponse)

instance Prelude.NFData BatchEnableStandardsResponse where
  rnf BatchEnableStandardsResponse' {..} =
    Prelude.rnf standardsSubscriptions
      `Prelude.seq` Prelude.rnf httpStatus
