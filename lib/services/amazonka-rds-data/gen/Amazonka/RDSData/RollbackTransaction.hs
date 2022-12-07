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
-- Module      : Amazonka.RDSData.RollbackTransaction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs a rollback of a transaction. Rolling back a transaction cancels
-- its changes.
module Amazonka.RDSData.RollbackTransaction
  ( -- * Creating a Request
    RollbackTransaction (..),
    newRollbackTransaction,

    -- * Request Lenses
    rollbackTransaction_resourceArn,
    rollbackTransaction_secretArn,
    rollbackTransaction_transactionId,

    -- * Destructuring the Response
    RollbackTransactionResponse (..),
    newRollbackTransactionResponse,

    -- * Response Lenses
    rollbackTransactionResponse_transactionStatus,
    rollbackTransactionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDSData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request parameters represent the input of a request to perform a
-- rollback of a transaction.
--
-- /See:/ 'newRollbackTransaction' smart constructor.
data RollbackTransaction = RollbackTransaction'
  { -- | The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
    resourceArn :: Prelude.Text,
    -- | The name or ARN of the secret that enables access to the DB cluster.
    secretArn :: Prelude.Text,
    -- | The identifier of the transaction to roll back.
    transactionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollbackTransaction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'rollbackTransaction_resourceArn' - The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
--
-- 'secretArn', 'rollbackTransaction_secretArn' - The name or ARN of the secret that enables access to the DB cluster.
--
-- 'transactionId', 'rollbackTransaction_transactionId' - The identifier of the transaction to roll back.
newRollbackTransaction ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  -- | 'transactionId'
  Prelude.Text ->
  RollbackTransaction
newRollbackTransaction
  pResourceArn_
  pSecretArn_
  pTransactionId_ =
    RollbackTransaction'
      { resourceArn = pResourceArn_,
        secretArn = pSecretArn_,
        transactionId = pTransactionId_
      }

-- | The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
rollbackTransaction_resourceArn :: Lens.Lens' RollbackTransaction Prelude.Text
rollbackTransaction_resourceArn = Lens.lens (\RollbackTransaction' {resourceArn} -> resourceArn) (\s@RollbackTransaction' {} a -> s {resourceArn = a} :: RollbackTransaction)

-- | The name or ARN of the secret that enables access to the DB cluster.
rollbackTransaction_secretArn :: Lens.Lens' RollbackTransaction Prelude.Text
rollbackTransaction_secretArn = Lens.lens (\RollbackTransaction' {secretArn} -> secretArn) (\s@RollbackTransaction' {} a -> s {secretArn = a} :: RollbackTransaction)

-- | The identifier of the transaction to roll back.
rollbackTransaction_transactionId :: Lens.Lens' RollbackTransaction Prelude.Text
rollbackTransaction_transactionId = Lens.lens (\RollbackTransaction' {transactionId} -> transactionId) (\s@RollbackTransaction' {} a -> s {transactionId = a} :: RollbackTransaction)

instance Core.AWSRequest RollbackTransaction where
  type
    AWSResponse RollbackTransaction =
      RollbackTransactionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RollbackTransactionResponse'
            Prelude.<$> (x Data..?> "transactionStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RollbackTransaction where
  hashWithSalt _salt RollbackTransaction' {..} =
    _salt `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` transactionId

instance Prelude.NFData RollbackTransaction where
  rnf RollbackTransaction' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf transactionId

instance Data.ToHeaders RollbackTransaction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RollbackTransaction where
  toJSON RollbackTransaction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("resourceArn" Data..= resourceArn),
            Prelude.Just ("secretArn" Data..= secretArn),
            Prelude.Just
              ("transactionId" Data..= transactionId)
          ]
      )

instance Data.ToPath RollbackTransaction where
  toPath = Prelude.const "/RollbackTransaction"

instance Data.ToQuery RollbackTransaction where
  toQuery = Prelude.const Prelude.mempty

-- | The response elements represent the output of a request to perform a
-- rollback of a transaction.
--
-- /See:/ 'newRollbackTransactionResponse' smart constructor.
data RollbackTransactionResponse = RollbackTransactionResponse'
  { -- | The status of the rollback operation.
    transactionStatus :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollbackTransactionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionStatus', 'rollbackTransactionResponse_transactionStatus' - The status of the rollback operation.
--
-- 'httpStatus', 'rollbackTransactionResponse_httpStatus' - The response's http status code.
newRollbackTransactionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RollbackTransactionResponse
newRollbackTransactionResponse pHttpStatus_ =
  RollbackTransactionResponse'
    { transactionStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the rollback operation.
rollbackTransactionResponse_transactionStatus :: Lens.Lens' RollbackTransactionResponse (Prelude.Maybe Prelude.Text)
rollbackTransactionResponse_transactionStatus = Lens.lens (\RollbackTransactionResponse' {transactionStatus} -> transactionStatus) (\s@RollbackTransactionResponse' {} a -> s {transactionStatus = a} :: RollbackTransactionResponse)

-- | The response's http status code.
rollbackTransactionResponse_httpStatus :: Lens.Lens' RollbackTransactionResponse Prelude.Int
rollbackTransactionResponse_httpStatus = Lens.lens (\RollbackTransactionResponse' {httpStatus} -> httpStatus) (\s@RollbackTransactionResponse' {} a -> s {httpStatus = a} :: RollbackTransactionResponse)

instance Prelude.NFData RollbackTransactionResponse where
  rnf RollbackTransactionResponse' {..} =
    Prelude.rnf transactionStatus
      `Prelude.seq` Prelude.rnf httpStatus
