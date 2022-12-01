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
-- Module      : Amazonka.LakeFormation.DescribeTransaction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of a single transaction.
module Amazonka.LakeFormation.DescribeTransaction
  ( -- * Creating a Request
    DescribeTransaction (..),
    newDescribeTransaction,

    -- * Request Lenses
    describeTransaction_transactionId,

    -- * Destructuring the Response
    DescribeTransactionResponse (..),
    newDescribeTransactionResponse,

    -- * Response Lenses
    describeTransactionResponse_transactionDescription,
    describeTransactionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTransaction' smart constructor.
data DescribeTransaction = DescribeTransaction'
  { -- | The transaction for which to return status.
    transactionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransaction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionId', 'describeTransaction_transactionId' - The transaction for which to return status.
newDescribeTransaction ::
  -- | 'transactionId'
  Prelude.Text ->
  DescribeTransaction
newDescribeTransaction pTransactionId_ =
  DescribeTransaction'
    { transactionId =
        pTransactionId_
    }

-- | The transaction for which to return status.
describeTransaction_transactionId :: Lens.Lens' DescribeTransaction Prelude.Text
describeTransaction_transactionId = Lens.lens (\DescribeTransaction' {transactionId} -> transactionId) (\s@DescribeTransaction' {} a -> s {transactionId = a} :: DescribeTransaction)

instance Core.AWSRequest DescribeTransaction where
  type
    AWSResponse DescribeTransaction =
      DescribeTransactionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTransactionResponse'
            Prelude.<$> (x Core..?> "TransactionDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTransaction where
  hashWithSalt _salt DescribeTransaction' {..} =
    _salt `Prelude.hashWithSalt` transactionId

instance Prelude.NFData DescribeTransaction where
  rnf DescribeTransaction' {..} =
    Prelude.rnf transactionId

instance Core.ToHeaders DescribeTransaction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTransaction where
  toJSON DescribeTransaction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TransactionId" Core..= transactionId)
          ]
      )

instance Core.ToPath DescribeTransaction where
  toPath = Prelude.const "/DescribeTransaction"

instance Core.ToQuery DescribeTransaction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTransactionResponse' smart constructor.
data DescribeTransactionResponse = DescribeTransactionResponse'
  { -- | Returns a @TransactionDescription@ object containing information about
    -- the transaction.
    transactionDescription :: Prelude.Maybe TransactionDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransactionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionDescription', 'describeTransactionResponse_transactionDescription' - Returns a @TransactionDescription@ object containing information about
-- the transaction.
--
-- 'httpStatus', 'describeTransactionResponse_httpStatus' - The response's http status code.
newDescribeTransactionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTransactionResponse
newDescribeTransactionResponse pHttpStatus_ =
  DescribeTransactionResponse'
    { transactionDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a @TransactionDescription@ object containing information about
-- the transaction.
describeTransactionResponse_transactionDescription :: Lens.Lens' DescribeTransactionResponse (Prelude.Maybe TransactionDescription)
describeTransactionResponse_transactionDescription = Lens.lens (\DescribeTransactionResponse' {transactionDescription} -> transactionDescription) (\s@DescribeTransactionResponse' {} a -> s {transactionDescription = a} :: DescribeTransactionResponse)

-- | The response's http status code.
describeTransactionResponse_httpStatus :: Lens.Lens' DescribeTransactionResponse Prelude.Int
describeTransactionResponse_httpStatus = Lens.lens (\DescribeTransactionResponse' {httpStatus} -> httpStatus) (\s@DescribeTransactionResponse' {} a -> s {httpStatus = a} :: DescribeTransactionResponse)

instance Prelude.NFData DescribeTransactionResponse where
  rnf DescribeTransactionResponse' {..} =
    Prelude.rnf transactionDescription
      `Prelude.seq` Prelude.rnf httpStatus
