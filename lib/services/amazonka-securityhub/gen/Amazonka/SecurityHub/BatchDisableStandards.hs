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
-- Module      : Amazonka.SecurityHub.BatchDisableStandards
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the standards specified by the provided
-- @StandardsSubscriptionArns@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards.html Security Standards>
-- section of the /Security Hub User Guide/.
module Amazonka.SecurityHub.BatchDisableStandards
  ( -- * Creating a Request
    BatchDisableStandards (..),
    newBatchDisableStandards,

    -- * Request Lenses
    batchDisableStandards_standardsSubscriptionArns,

    -- * Destructuring the Response
    BatchDisableStandardsResponse (..),
    newBatchDisableStandardsResponse,

    -- * Response Lenses
    batchDisableStandardsResponse_standardsSubscriptions,
    batchDisableStandardsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newBatchDisableStandards' smart constructor.
data BatchDisableStandards = BatchDisableStandards'
  { -- | The ARNs of the standards subscriptions to disable.
    standardsSubscriptionArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisableStandards' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardsSubscriptionArns', 'batchDisableStandards_standardsSubscriptionArns' - The ARNs of the standards subscriptions to disable.
newBatchDisableStandards ::
  -- | 'standardsSubscriptionArns'
  Prelude.NonEmpty Prelude.Text ->
  BatchDisableStandards
newBatchDisableStandards pStandardsSubscriptionArns_ =
  BatchDisableStandards'
    { standardsSubscriptionArns =
        Lens.coerced Lens.# pStandardsSubscriptionArns_
    }

-- | The ARNs of the standards subscriptions to disable.
batchDisableStandards_standardsSubscriptionArns :: Lens.Lens' BatchDisableStandards (Prelude.NonEmpty Prelude.Text)
batchDisableStandards_standardsSubscriptionArns = Lens.lens (\BatchDisableStandards' {standardsSubscriptionArns} -> standardsSubscriptionArns) (\s@BatchDisableStandards' {} a -> s {standardsSubscriptionArns = a} :: BatchDisableStandards) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDisableStandards where
  type
    AWSResponse BatchDisableStandards =
      BatchDisableStandardsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDisableStandardsResponse'
            Prelude.<$> ( x
                            Data..?> "StandardsSubscriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDisableStandards where
  hashWithSalt _salt BatchDisableStandards' {..} =
    _salt
      `Prelude.hashWithSalt` standardsSubscriptionArns

instance Prelude.NFData BatchDisableStandards where
  rnf BatchDisableStandards' {..} =
    Prelude.rnf standardsSubscriptionArns

instance Data.ToHeaders BatchDisableStandards where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDisableStandards where
  toJSON BatchDisableStandards' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StandardsSubscriptionArns"
                  Data..= standardsSubscriptionArns
              )
          ]
      )

instance Data.ToPath BatchDisableStandards where
  toPath = Prelude.const "/standards/deregister"

instance Data.ToQuery BatchDisableStandards where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDisableStandardsResponse' smart constructor.
data BatchDisableStandardsResponse = BatchDisableStandardsResponse'
  { -- | The details of the standards subscriptions that were disabled.
    standardsSubscriptions :: Prelude.Maybe [StandardsSubscription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisableStandardsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardsSubscriptions', 'batchDisableStandardsResponse_standardsSubscriptions' - The details of the standards subscriptions that were disabled.
--
-- 'httpStatus', 'batchDisableStandardsResponse_httpStatus' - The response's http status code.
newBatchDisableStandardsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDisableStandardsResponse
newBatchDisableStandardsResponse pHttpStatus_ =
  BatchDisableStandardsResponse'
    { standardsSubscriptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the standards subscriptions that were disabled.
batchDisableStandardsResponse_standardsSubscriptions :: Lens.Lens' BatchDisableStandardsResponse (Prelude.Maybe [StandardsSubscription])
batchDisableStandardsResponse_standardsSubscriptions = Lens.lens (\BatchDisableStandardsResponse' {standardsSubscriptions} -> standardsSubscriptions) (\s@BatchDisableStandardsResponse' {} a -> s {standardsSubscriptions = a} :: BatchDisableStandardsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDisableStandardsResponse_httpStatus :: Lens.Lens' BatchDisableStandardsResponse Prelude.Int
batchDisableStandardsResponse_httpStatus = Lens.lens (\BatchDisableStandardsResponse' {httpStatus} -> httpStatus) (\s@BatchDisableStandardsResponse' {} a -> s {httpStatus = a} :: BatchDisableStandardsResponse)

instance Prelude.NFData BatchDisableStandardsResponse where
  rnf BatchDisableStandardsResponse' {..} =
    Prelude.rnf standardsSubscriptions
      `Prelude.seq` Prelude.rnf httpStatus
