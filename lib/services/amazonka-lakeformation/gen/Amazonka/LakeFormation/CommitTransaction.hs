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
-- Module      : Amazonka.LakeFormation.CommitTransaction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to commit the specified transaction. Returns an exception if
-- the transaction was previously aborted. This API action is idempotent if
-- called multiple times for the same transaction.
module Amazonka.LakeFormation.CommitTransaction
  ( -- * Creating a Request
    CommitTransaction (..),
    newCommitTransaction,

    -- * Request Lenses
    commitTransaction_transactionId,

    -- * Destructuring the Response
    CommitTransactionResponse (..),
    newCommitTransactionResponse,

    -- * Response Lenses
    commitTransactionResponse_transactionStatus,
    commitTransactionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCommitTransaction' smart constructor.
data CommitTransaction = CommitTransaction'
  { -- | The transaction to commit.
    transactionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CommitTransaction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionId', 'commitTransaction_transactionId' - The transaction to commit.
newCommitTransaction ::
  -- | 'transactionId'
  Prelude.Text ->
  CommitTransaction
newCommitTransaction pTransactionId_ =
  CommitTransaction' {transactionId = pTransactionId_}

-- | The transaction to commit.
commitTransaction_transactionId :: Lens.Lens' CommitTransaction Prelude.Text
commitTransaction_transactionId = Lens.lens (\CommitTransaction' {transactionId} -> transactionId) (\s@CommitTransaction' {} a -> s {transactionId = a} :: CommitTransaction)

instance Core.AWSRequest CommitTransaction where
  type
    AWSResponse CommitTransaction =
      CommitTransactionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CommitTransactionResponse'
            Prelude.<$> (x Data..?> "TransactionStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CommitTransaction where
  hashWithSalt _salt CommitTransaction' {..} =
    _salt `Prelude.hashWithSalt` transactionId

instance Prelude.NFData CommitTransaction where
  rnf CommitTransaction' {..} =
    Prelude.rnf transactionId

instance Data.ToHeaders CommitTransaction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CommitTransaction where
  toJSON CommitTransaction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TransactionId" Data..= transactionId)
          ]
      )

instance Data.ToPath CommitTransaction where
  toPath = Prelude.const "/CommitTransaction"

instance Data.ToQuery CommitTransaction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCommitTransactionResponse' smart constructor.
data CommitTransactionResponse = CommitTransactionResponse'
  { -- | The status of the transaction.
    transactionStatus :: Prelude.Maybe TransactionStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CommitTransactionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionStatus', 'commitTransactionResponse_transactionStatus' - The status of the transaction.
--
-- 'httpStatus', 'commitTransactionResponse_httpStatus' - The response's http status code.
newCommitTransactionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CommitTransactionResponse
newCommitTransactionResponse pHttpStatus_ =
  CommitTransactionResponse'
    { transactionStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the transaction.
commitTransactionResponse_transactionStatus :: Lens.Lens' CommitTransactionResponse (Prelude.Maybe TransactionStatus)
commitTransactionResponse_transactionStatus = Lens.lens (\CommitTransactionResponse' {transactionStatus} -> transactionStatus) (\s@CommitTransactionResponse' {} a -> s {transactionStatus = a} :: CommitTransactionResponse)

-- | The response's http status code.
commitTransactionResponse_httpStatus :: Lens.Lens' CommitTransactionResponse Prelude.Int
commitTransactionResponse_httpStatus = Lens.lens (\CommitTransactionResponse' {httpStatus} -> httpStatus) (\s@CommitTransactionResponse' {} a -> s {httpStatus = a} :: CommitTransactionResponse)

instance Prelude.NFData CommitTransactionResponse where
  rnf CommitTransactionResponse' {..} =
    Prelude.rnf transactionStatus
      `Prelude.seq` Prelude.rnf httpStatus
