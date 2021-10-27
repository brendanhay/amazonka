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
-- Module      : Network.AWS.SecurityHub.BatchEnableStandards
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the standards specified by the provided @StandardsArn@. To
-- obtain the ARN for a standard, use the @DescribeStandards@ operation.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards.html Security Standards>
-- section of the /Security Hub User Guide/.
module Network.AWS.SecurityHub.BatchEnableStandards
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecurityHub.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchEnableStandardsResponse'
            Prelude.<$> ( x Core..?> "StandardsSubscriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchEnableStandards

instance Prelude.NFData BatchEnableStandards

instance Core.ToHeaders BatchEnableStandards where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchEnableStandards where
  toJSON BatchEnableStandards' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StandardsSubscriptionRequests"
                  Core..= standardsSubscriptionRequests
              )
          ]
      )

instance Core.ToPath BatchEnableStandards where
  toPath = Prelude.const "/standards/register"

instance Core.ToQuery BatchEnableStandards where
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

instance Prelude.NFData BatchEnableStandardsResponse
