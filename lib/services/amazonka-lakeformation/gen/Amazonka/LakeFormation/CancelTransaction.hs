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
-- Module      : Amazonka.LakeFormation.CancelTransaction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to cancel the specified transaction. Returns an exception if
-- the transaction was previously committed.
module Amazonka.LakeFormation.CancelTransaction
  ( -- * Creating a Request
    CancelTransaction (..),
    newCancelTransaction,

    -- * Request Lenses
    cancelTransaction_transactionId,

    -- * Destructuring the Response
    CancelTransactionResponse (..),
    newCancelTransactionResponse,

    -- * Response Lenses
    cancelTransactionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelTransaction' smart constructor.
data CancelTransaction = CancelTransaction'
  { -- | The transaction to cancel.
    transactionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelTransaction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionId', 'cancelTransaction_transactionId' - The transaction to cancel.
newCancelTransaction ::
  -- | 'transactionId'
  Prelude.Text ->
  CancelTransaction
newCancelTransaction pTransactionId_ =
  CancelTransaction' {transactionId = pTransactionId_}

-- | The transaction to cancel.
cancelTransaction_transactionId :: Lens.Lens' CancelTransaction Prelude.Text
cancelTransaction_transactionId = Lens.lens (\CancelTransaction' {transactionId} -> transactionId) (\s@CancelTransaction' {} a -> s {transactionId = a} :: CancelTransaction)

instance Core.AWSRequest CancelTransaction where
  type
    AWSResponse CancelTransaction =
      CancelTransactionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelTransactionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelTransaction where
  hashWithSalt _salt CancelTransaction' {..} =
    _salt `Prelude.hashWithSalt` transactionId

instance Prelude.NFData CancelTransaction where
  rnf CancelTransaction' {..} =
    Prelude.rnf transactionId

instance Core.ToHeaders CancelTransaction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelTransaction where
  toJSON CancelTransaction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TransactionId" Core..= transactionId)
          ]
      )

instance Core.ToPath CancelTransaction where
  toPath = Prelude.const "/CancelTransaction"

instance Core.ToQuery CancelTransaction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelTransactionResponse' smart constructor.
data CancelTransactionResponse = CancelTransactionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelTransactionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelTransactionResponse_httpStatus' - The response's http status code.
newCancelTransactionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelTransactionResponse
newCancelTransactionResponse pHttpStatus_ =
  CancelTransactionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelTransactionResponse_httpStatus :: Lens.Lens' CancelTransactionResponse Prelude.Int
cancelTransactionResponse_httpStatus = Lens.lens (\CancelTransactionResponse' {httpStatus} -> httpStatus) (\s@CancelTransactionResponse' {} a -> s {httpStatus = a} :: CancelTransactionResponse)

instance Prelude.NFData CancelTransactionResponse where
  rnf CancelTransactionResponse' {..} =
    Prelude.rnf httpStatus
