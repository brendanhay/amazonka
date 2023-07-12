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
-- Module      : Amazonka.DynamoDB.ExecuteTransaction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform transactional reads or writes on
-- data stored in DynamoDB, using PartiQL.
--
-- The entire transaction must consist of either read statements or write
-- statements, you cannot mix both in one transaction. The EXISTS function
-- is an exception and can be used to check the condition of specific
-- attributes of the item in a similar manner to @ConditionCheck@ in the
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/transaction-apis.html#transaction-apis-txwriteitems TransactWriteItems>
-- API.
module Amazonka.DynamoDB.ExecuteTransaction
  ( -- * Creating a Request
    ExecuteTransaction (..),
    newExecuteTransaction,

    -- * Request Lenses
    executeTransaction_clientRequestToken,
    executeTransaction_returnConsumedCapacity,
    executeTransaction_transactStatements,

    -- * Destructuring the Response
    ExecuteTransactionResponse (..),
    newExecuteTransactionResponse,

    -- * Response Lenses
    executeTransactionResponse_consumedCapacity,
    executeTransactionResponse_responses,
    executeTransactionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExecuteTransaction' smart constructor.
data ExecuteTransaction = ExecuteTransaction'
  { -- | Set this value to get remaining results, if @NextToken@ was returned in
    -- the statement response.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Determines the level of detail about either provisioned or on-demand
    -- throughput consumption that is returned in the response. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_TransactGetItems.html TransactGetItems>
    -- and
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_TransactWriteItems.html TransactWriteItems>.
    returnConsumedCapacity :: Prelude.Maybe ReturnConsumedCapacity,
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
-- 'returnConsumedCapacity', 'executeTransaction_returnConsumedCapacity' - Determines the level of detail about either provisioned or on-demand
-- throughput consumption that is returned in the response. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_TransactGetItems.html TransactGetItems>
-- and
-- <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_TransactWriteItems.html TransactWriteItems>.
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
      returnConsumedCapacity = Prelude.Nothing,
      transactStatements =
        Lens.coerced Lens.# pTransactStatements_
    }

-- | Set this value to get remaining results, if @NextToken@ was returned in
-- the statement response.
executeTransaction_clientRequestToken :: Lens.Lens' ExecuteTransaction (Prelude.Maybe Prelude.Text)
executeTransaction_clientRequestToken = Lens.lens (\ExecuteTransaction' {clientRequestToken} -> clientRequestToken) (\s@ExecuteTransaction' {} a -> s {clientRequestToken = a} :: ExecuteTransaction)

-- | Determines the level of detail about either provisioned or on-demand
-- throughput consumption that is returned in the response. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_TransactGetItems.html TransactGetItems>
-- and
-- <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_TransactWriteItems.html TransactWriteItems>.
executeTransaction_returnConsumedCapacity :: Lens.Lens' ExecuteTransaction (Prelude.Maybe ReturnConsumedCapacity)
executeTransaction_returnConsumedCapacity = Lens.lens (\ExecuteTransaction' {returnConsumedCapacity} -> returnConsumedCapacity) (\s@ExecuteTransaction' {} a -> s {returnConsumedCapacity = a} :: ExecuteTransaction)

-- | The list of PartiQL statements representing the transaction to run.
executeTransaction_transactStatements :: Lens.Lens' ExecuteTransaction (Prelude.NonEmpty ParameterizedStatement)
executeTransaction_transactStatements = Lens.lens (\ExecuteTransaction' {transactStatements} -> transactStatements) (\s@ExecuteTransaction' {} a -> s {transactStatements = a} :: ExecuteTransaction) Prelude.. Lens.coerced

instance Core.AWSRequest ExecuteTransaction where
  type
    AWSResponse ExecuteTransaction =
      ExecuteTransactionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExecuteTransactionResponse'
            Prelude.<$> ( x
                            Data..?> "ConsumedCapacity"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Responses")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExecuteTransaction where
  hashWithSalt _salt ExecuteTransaction' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` returnConsumedCapacity
      `Prelude.hashWithSalt` transactStatements

instance Prelude.NFData ExecuteTransaction where
  rnf ExecuteTransaction' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf returnConsumedCapacity
      `Prelude.seq` Prelude.rnf transactStatements

instance Data.ToHeaders ExecuteTransaction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.ExecuteTransaction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExecuteTransaction where
  toJSON ExecuteTransaction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("ReturnConsumedCapacity" Data..=)
              Prelude.<$> returnConsumedCapacity,
            Prelude.Just
              ("TransactStatements" Data..= transactStatements)
          ]
      )

instance Data.ToPath ExecuteTransaction where
  toPath = Prelude.const "/"

instance Data.ToQuery ExecuteTransaction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExecuteTransactionResponse' smart constructor.
data ExecuteTransactionResponse = ExecuteTransactionResponse'
  { -- | The capacity units consumed by the entire operation. The values of the
    -- list are ordered according to the ordering of the statements.
    consumedCapacity :: Prelude.Maybe [ConsumedCapacity],
    -- | The response to a PartiQL transaction.
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
-- 'consumedCapacity', 'executeTransactionResponse_consumedCapacity' - The capacity units consumed by the entire operation. The values of the
-- list are ordered according to the ordering of the statements.
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
    { consumedCapacity =
        Prelude.Nothing,
      responses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The capacity units consumed by the entire operation. The values of the
-- list are ordered according to the ordering of the statements.
executeTransactionResponse_consumedCapacity :: Lens.Lens' ExecuteTransactionResponse (Prelude.Maybe [ConsumedCapacity])
executeTransactionResponse_consumedCapacity = Lens.lens (\ExecuteTransactionResponse' {consumedCapacity} -> consumedCapacity) (\s@ExecuteTransactionResponse' {} a -> s {consumedCapacity = a} :: ExecuteTransactionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response to a PartiQL transaction.
executeTransactionResponse_responses :: Lens.Lens' ExecuteTransactionResponse (Prelude.Maybe (Prelude.NonEmpty ItemResponse))
executeTransactionResponse_responses = Lens.lens (\ExecuteTransactionResponse' {responses} -> responses) (\s@ExecuteTransactionResponse' {} a -> s {responses = a} :: ExecuteTransactionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
executeTransactionResponse_httpStatus :: Lens.Lens' ExecuteTransactionResponse Prelude.Int
executeTransactionResponse_httpStatus = Lens.lens (\ExecuteTransactionResponse' {httpStatus} -> httpStatus) (\s@ExecuteTransactionResponse' {} a -> s {httpStatus = a} :: ExecuteTransactionResponse)

instance Prelude.NFData ExecuteTransactionResponse where
  rnf ExecuteTransactionResponse' {..} =
    Prelude.rnf consumedCapacity
      `Prelude.seq` Prelude.rnf responses
      `Prelude.seq` Prelude.rnf httpStatus
