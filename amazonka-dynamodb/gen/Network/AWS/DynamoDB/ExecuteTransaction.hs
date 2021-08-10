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
-- Module      : Network.AWS.DynamoDB.ExecuteTransaction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform transactional reads or writes on
-- data stored in DynamoDB, using PartiQL.
module Network.AWS.DynamoDB.ExecuteTransaction
  ( -- * Creating a Request
    ExecuteTransaction (..),
    newExecuteTransaction,

    -- * Request Lenses
    executeTransaction_clientRequestToken,
    executeTransaction_transactStatements,

    -- * Destructuring the Response
    ExecuteTransactionResponse (..),
    newExecuteTransactionResponse,

    -- * Response Lenses
    executeTransactionResponse_responses,
    executeTransactionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExecuteTransaction' smart constructor.
data ExecuteTransaction = ExecuteTransaction'
  { -- | Set this value to get remaining results, if @NextToken@ was returned in
    -- the statement response.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The list of PartiQL statements representing the transaction to run.
    transactStatements :: Prelude.NonEmpty ParameterizedStatement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteTransaction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'executeTransaction_clientRequestToken' - Set this value to get remaining results, if @NextToken@ was returned in
-- the statement response.
--
-- 'transactStatements', 'executeTransaction_transactStatements' - The list of PartiQL statements representing the transaction to run.
newExecuteTransaction ::
  -- | 'transactStatements'
  Prelude.NonEmpty ParameterizedStatement ->
  ExecuteTransaction
newExecuteTransaction pTransactStatements_ =
  ExecuteTransaction'
    { clientRequestToken =
        Prelude.Nothing,
      transactStatements =
        Lens._Coerce Lens.# pTransactStatements_
    }

-- | Set this value to get remaining results, if @NextToken@ was returned in
-- the statement response.
executeTransaction_clientRequestToken :: Lens.Lens' ExecuteTransaction (Prelude.Maybe Prelude.Text)
executeTransaction_clientRequestToken = Lens.lens (\ExecuteTransaction' {clientRequestToken} -> clientRequestToken) (\s@ExecuteTransaction' {} a -> s {clientRequestToken = a} :: ExecuteTransaction)

-- | The list of PartiQL statements representing the transaction to run.
executeTransaction_transactStatements :: Lens.Lens' ExecuteTransaction (Prelude.NonEmpty ParameterizedStatement)
executeTransaction_transactStatements = Lens.lens (\ExecuteTransaction' {transactStatements} -> transactStatements) (\s@ExecuteTransaction' {} a -> s {transactStatements = a} :: ExecuteTransaction) Prelude.. Lens._Coerce

instance Core.AWSRequest ExecuteTransaction where
  type
    AWSResponse ExecuteTransaction =
      ExecuteTransactionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ExecuteTransactionResponse'
            Prelude.<$> (x Core..?> "Responses")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExecuteTransaction

instance Prelude.NFData ExecuteTransaction

instance Core.ToHeaders ExecuteTransaction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.ExecuteTransaction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ExecuteTransaction where
  toJSON ExecuteTransaction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just
              ("TransactStatements" Core..= transactStatements)
          ]
      )

instance Core.ToPath ExecuteTransaction where
  toPath = Prelude.const "/"

instance Core.ToQuery ExecuteTransaction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExecuteTransactionResponse' smart constructor.
data ExecuteTransactionResponse = ExecuteTransactionResponse'
  { -- | The response to a PartiQL transaction.
    responses :: Prelude.Maybe (Prelude.NonEmpty ItemResponse),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteTransactionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responses', 'executeTransactionResponse_responses' - The response to a PartiQL transaction.
--
-- 'httpStatus', 'executeTransactionResponse_httpStatus' - The response's http status code.
newExecuteTransactionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExecuteTransactionResponse
newExecuteTransactionResponse pHttpStatus_ =
  ExecuteTransactionResponse'
    { responses =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The response to a PartiQL transaction.
executeTransactionResponse_responses :: Lens.Lens' ExecuteTransactionResponse (Prelude.Maybe (Prelude.NonEmpty ItemResponse))
executeTransactionResponse_responses = Lens.lens (\ExecuteTransactionResponse' {responses} -> responses) (\s@ExecuteTransactionResponse' {} a -> s {responses = a} :: ExecuteTransactionResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
executeTransactionResponse_httpStatus :: Lens.Lens' ExecuteTransactionResponse Prelude.Int
executeTransactionResponse_httpStatus = Lens.lens (\ExecuteTransactionResponse' {httpStatus} -> httpStatus) (\s@ExecuteTransactionResponse' {} a -> s {httpStatus = a} :: ExecuteTransactionResponse)

instance Prelude.NFData ExecuteTransactionResponse
