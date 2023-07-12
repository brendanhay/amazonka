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
-- Module      : Amazonka.RDSData.CommitTransaction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ends a SQL transaction started with the @BeginTransaction@ operation and
-- commits the changes.
module Amazonka.RDSData.CommitTransaction
  ( -- * Creating a Request
    CommitTransaction (..),
    newCommitTransaction,

    -- * Request Lenses
    commitTransaction_resourceArn,
    commitTransaction_secretArn,
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
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDSData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request parameters represent the input of a commit transaction
-- request.
--
-- /See:/ 'newCommitTransaction' smart constructor.
data CommitTransaction = CommitTransaction'
  { -- | The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
    resourceArn :: Prelude.Text,
    -- | The name or ARN of the secret that enables access to the DB cluster.
    secretArn :: Prelude.Text,
    -- | The identifier of the transaction to end and commit.
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
-- 'resourceArn', 'commitTransaction_resourceArn' - The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
--
-- 'secretArn', 'commitTransaction_secretArn' - The name or ARN of the secret that enables access to the DB cluster.
--
-- 'transactionId', 'commitTransaction_transactionId' - The identifier of the transaction to end and commit.
newCommitTransaction ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  -- | 'transactionId'
  Prelude.Text ->
  CommitTransaction
newCommitTransaction
  pResourceArn_
  pSecretArn_
  pTransactionId_ =
    CommitTransaction'
      { resourceArn = pResourceArn_,
        secretArn = pSecretArn_,
        transactionId = pTransactionId_
      }

-- | The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
commitTransaction_resourceArn :: Lens.Lens' CommitTransaction Prelude.Text
commitTransaction_resourceArn = Lens.lens (\CommitTransaction' {resourceArn} -> resourceArn) (\s@CommitTransaction' {} a -> s {resourceArn = a} :: CommitTransaction)

-- | The name or ARN of the secret that enables access to the DB cluster.
commitTransaction_secretArn :: Lens.Lens' CommitTransaction Prelude.Text
commitTransaction_secretArn = Lens.lens (\CommitTransaction' {secretArn} -> secretArn) (\s@CommitTransaction' {} a -> s {secretArn = a} :: CommitTransaction)

-- | The identifier of the transaction to end and commit.
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
            Prelude.<$> (x Data..?> "transactionStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CommitTransaction where
  hashWithSalt _salt CommitTransaction' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` transactionId

instance Prelude.NFData CommitTransaction where
  rnf CommitTransaction' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf transactionId

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
          [ Prelude.Just ("resourceArn" Data..= resourceArn),
            Prelude.Just ("secretArn" Data..= secretArn),
            Prelude.Just
              ("transactionId" Data..= transactionId)
          ]
      )

instance Data.ToPath CommitTransaction where
  toPath = Prelude.const "/CommitTransaction"

instance Data.ToQuery CommitTransaction where
  toQuery = Prelude.const Prelude.mempty

-- | The response elements represent the output of a commit transaction
-- request.
--
-- /See:/ 'newCommitTransactionResponse' smart constructor.
data CommitTransactionResponse = CommitTransactionResponse'
  { -- | The status of the commit operation.
    transactionStatus :: Prelude.Maybe Prelude.Text,
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
-- 'transactionStatus', 'commitTransactionResponse_transactionStatus' - The status of the commit operation.
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

-- | The status of the commit operation.
commitTransactionResponse_transactionStatus :: Lens.Lens' CommitTransactionResponse (Prelude.Maybe Prelude.Text)
commitTransactionResponse_transactionStatus = Lens.lens (\CommitTransactionResponse' {transactionStatus} -> transactionStatus) (\s@CommitTransactionResponse' {} a -> s {transactionStatus = a} :: CommitTransactionResponse)

-- | The response's http status code.
commitTransactionResponse_httpStatus :: Lens.Lens' CommitTransactionResponse Prelude.Int
commitTransactionResponse_httpStatus = Lens.lens (\CommitTransactionResponse' {httpStatus} -> httpStatus) (\s@CommitTransactionResponse' {} a -> s {httpStatus = a} :: CommitTransactionResponse)

instance Prelude.NFData CommitTransactionResponse where
  rnf CommitTransactionResponse' {..} =
    Prelude.rnf transactionStatus
      `Prelude.seq` Prelude.rnf httpStatus
