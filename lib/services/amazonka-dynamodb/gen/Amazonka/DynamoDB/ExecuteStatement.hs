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
-- Module      : Amazonka.DynamoDB.ExecuteStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform reads and singleton writes on data
-- stored in DynamoDB, using PartiQL.
--
-- For PartiQL reads (@SELECT@ statement), if the total number of processed
-- items exceeds the maximum dataset size limit of 1 MB, the read stops and
-- results are returned to the user as a @LastEvaluatedKey@ value to
-- continue the read in a subsequent operation. If the filter criteria in
-- @WHERE@ clause does not match any data, the read will return an empty
-- result set.
--
-- A single @SELECT@ statement response can return up to the maximum number
-- of items (if using the Limit parameter) or a maximum of 1 MB of data
-- (and then apply any filtering to the results using @WHERE@ clause). If
-- @LastEvaluatedKey@ is present in the response, you need to paginate the
-- result set.
module Amazonka.DynamoDB.ExecuteStatement
  ( -- * Creating a Request
    ExecuteStatement (..),
    newExecuteStatement,

    -- * Request Lenses
    executeStatement_consistentRead,
    executeStatement_limit,
    executeStatement_nextToken,
    executeStatement_parameters,
    executeStatement_returnConsumedCapacity,
    executeStatement_statement,

    -- * Destructuring the Response
    ExecuteStatementResponse (..),
    newExecuteStatementResponse,

    -- * Response Lenses
    executeStatementResponse_consumedCapacity,
    executeStatementResponse_items,
    executeStatementResponse_lastEvaluatedKey,
    executeStatementResponse_nextToken,
    executeStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExecuteStatement' smart constructor.
data ExecuteStatement = ExecuteStatement'
  { -- | The consistency of a read operation. If set to @true@, then a strongly
    -- consistent read is used; otherwise, an eventually consistent read is
    -- used.
    consistentRead :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of items to evaluate (not necessarily the number of
    -- matching items). If DynamoDB processes the number of items up to the
    -- limit while processing the results, it stops the operation and returns
    -- the matching values up to that point, along with a key in
    -- @LastEvaluatedKey@ to apply in a subsequent operation so you can pick up
    -- where you left off. Also, if the processed dataset size exceeds 1 MB
    -- before DynamoDB reaches this limit, it stops the operation and returns
    -- the matching values up to the limit, and a key in @LastEvaluatedKey@ to
    -- apply in a subsequent operation to continue the operation.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Set this value to get remaining results, if @NextToken@ was returned in
    -- the statement response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The parameters for the PartiQL statement, if any.
    parameters :: Prelude.Maybe (Prelude.NonEmpty AttributeValue),
    returnConsumedCapacity :: Prelude.Maybe ReturnConsumedCapacity,
    -- | The PartiQL statement representing the operation to run.
    statement :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consistentRead', 'executeStatement_consistentRead' - The consistency of a read operation. If set to @true@, then a strongly
-- consistent read is used; otherwise, an eventually consistent read is
-- used.
--
-- 'limit', 'executeStatement_limit' - The maximum number of items to evaluate (not necessarily the number of
-- matching items). If DynamoDB processes the number of items up to the
-- limit while processing the results, it stops the operation and returns
-- the matching values up to that point, along with a key in
-- @LastEvaluatedKey@ to apply in a subsequent operation so you can pick up
-- where you left off. Also, if the processed dataset size exceeds 1 MB
-- before DynamoDB reaches this limit, it stops the operation and returns
-- the matching values up to the limit, and a key in @LastEvaluatedKey@ to
-- apply in a subsequent operation to continue the operation.
--
-- 'nextToken', 'executeStatement_nextToken' - Set this value to get remaining results, if @NextToken@ was returned in
-- the statement response.
--
-- 'parameters', 'executeStatement_parameters' - The parameters for the PartiQL statement, if any.
--
-- 'returnConsumedCapacity', 'executeStatement_returnConsumedCapacity' - Undocumented member.
--
-- 'statement', 'executeStatement_statement' - The PartiQL statement representing the operation to run.
newExecuteStatement ::
  -- | 'statement'
  Prelude.Text ->
  ExecuteStatement
newExecuteStatement pStatement_ =
  ExecuteStatement'
    { consistentRead = Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      parameters = Prelude.Nothing,
      returnConsumedCapacity = Prelude.Nothing,
      statement = pStatement_
    }

-- | The consistency of a read operation. If set to @true@, then a strongly
-- consistent read is used; otherwise, an eventually consistent read is
-- used.
executeStatement_consistentRead :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Bool)
executeStatement_consistentRead = Lens.lens (\ExecuteStatement' {consistentRead} -> consistentRead) (\s@ExecuteStatement' {} a -> s {consistentRead = a} :: ExecuteStatement)

-- | The maximum number of items to evaluate (not necessarily the number of
-- matching items). If DynamoDB processes the number of items up to the
-- limit while processing the results, it stops the operation and returns
-- the matching values up to that point, along with a key in
-- @LastEvaluatedKey@ to apply in a subsequent operation so you can pick up
-- where you left off. Also, if the processed dataset size exceeds 1 MB
-- before DynamoDB reaches this limit, it stops the operation and returns
-- the matching values up to the limit, and a key in @LastEvaluatedKey@ to
-- apply in a subsequent operation to continue the operation.
executeStatement_limit :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Natural)
executeStatement_limit = Lens.lens (\ExecuteStatement' {limit} -> limit) (\s@ExecuteStatement' {} a -> s {limit = a} :: ExecuteStatement)

-- | Set this value to get remaining results, if @NextToken@ was returned in
-- the statement response.
executeStatement_nextToken :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Text)
executeStatement_nextToken = Lens.lens (\ExecuteStatement' {nextToken} -> nextToken) (\s@ExecuteStatement' {} a -> s {nextToken = a} :: ExecuteStatement)

-- | The parameters for the PartiQL statement, if any.
executeStatement_parameters :: Lens.Lens' ExecuteStatement (Prelude.Maybe (Prelude.NonEmpty AttributeValue))
executeStatement_parameters = Lens.lens (\ExecuteStatement' {parameters} -> parameters) (\s@ExecuteStatement' {} a -> s {parameters = a} :: ExecuteStatement) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
executeStatement_returnConsumedCapacity :: Lens.Lens' ExecuteStatement (Prelude.Maybe ReturnConsumedCapacity)
executeStatement_returnConsumedCapacity = Lens.lens (\ExecuteStatement' {returnConsumedCapacity} -> returnConsumedCapacity) (\s@ExecuteStatement' {} a -> s {returnConsumedCapacity = a} :: ExecuteStatement)

-- | The PartiQL statement representing the operation to run.
executeStatement_statement :: Lens.Lens' ExecuteStatement Prelude.Text
executeStatement_statement = Lens.lens (\ExecuteStatement' {statement} -> statement) (\s@ExecuteStatement' {} a -> s {statement = a} :: ExecuteStatement)

instance Core.AWSRequest ExecuteStatement where
  type
    AWSResponse ExecuteStatement =
      ExecuteStatementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExecuteStatementResponse'
            Prelude.<$> (x Data..?> "ConsumedCapacity")
            Prelude.<*> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "LastEvaluatedKey"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExecuteStatement where
  hashWithSalt _salt ExecuteStatement' {..} =
    _salt `Prelude.hashWithSalt` consistentRead
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` returnConsumedCapacity
      `Prelude.hashWithSalt` statement

instance Prelude.NFData ExecuteStatement where
  rnf ExecuteStatement' {..} =
    Prelude.rnf consistentRead
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf returnConsumedCapacity
      `Prelude.seq` Prelude.rnf statement

instance Data.ToHeaders ExecuteStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.ExecuteStatement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExecuteStatement where
  toJSON ExecuteStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConsistentRead" Data..=)
              Prelude.<$> consistentRead,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("ReturnConsumedCapacity" Data..=)
              Prelude.<$> returnConsumedCapacity,
            Prelude.Just ("Statement" Data..= statement)
          ]
      )

instance Data.ToPath ExecuteStatement where
  toPath = Prelude.const "/"

instance Data.ToQuery ExecuteStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExecuteStatementResponse' smart constructor.
data ExecuteStatementResponse = ExecuteStatementResponse'
  { consumedCapacity :: Prelude.Maybe ConsumedCapacity,
    -- | If a read operation was used, this property will contain the result of
    -- the read operation; a map of attribute names and their values. For the
    -- write operations this value will be empty.
    items :: Prelude.Maybe [Prelude.HashMap Prelude.Text AttributeValue],
    -- | The primary key of the item where the operation stopped, inclusive of
    -- the previous result set. Use this value to start a new operation,
    -- excluding this value in the new request. If @LastEvaluatedKey@ is empty,
    -- then the \"last page\" of results has been processed and there is no
    -- more data to be retrieved. If @LastEvaluatedKey@ is not empty, it does
    -- not necessarily mean that there is more data in the result set. The only
    -- way to know when you have reached the end of the result set is when
    -- @LastEvaluatedKey@ is empty.
    lastEvaluatedKey :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | If the response of a read request exceeds the response payload limit
    -- DynamoDB will set this value in the response. If set, you can use that
    -- this value in the subsequent request to get the remaining results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumedCapacity', 'executeStatementResponse_consumedCapacity' - Undocumented member.
--
-- 'items', 'executeStatementResponse_items' - If a read operation was used, this property will contain the result of
-- the read operation; a map of attribute names and their values. For the
-- write operations this value will be empty.
--
-- 'lastEvaluatedKey', 'executeStatementResponse_lastEvaluatedKey' - The primary key of the item where the operation stopped, inclusive of
-- the previous result set. Use this value to start a new operation,
-- excluding this value in the new request. If @LastEvaluatedKey@ is empty,
-- then the \"last page\" of results has been processed and there is no
-- more data to be retrieved. If @LastEvaluatedKey@ is not empty, it does
-- not necessarily mean that there is more data in the result set. The only
-- way to know when you have reached the end of the result set is when
-- @LastEvaluatedKey@ is empty.
--
-- 'nextToken', 'executeStatementResponse_nextToken' - If the response of a read request exceeds the response payload limit
-- DynamoDB will set this value in the response. If set, you can use that
-- this value in the subsequent request to get the remaining results.
--
-- 'httpStatus', 'executeStatementResponse_httpStatus' - The response's http status code.
newExecuteStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExecuteStatementResponse
newExecuteStatementResponse pHttpStatus_ =
  ExecuteStatementResponse'
    { consumedCapacity =
        Prelude.Nothing,
      items = Prelude.Nothing,
      lastEvaluatedKey = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
executeStatementResponse_consumedCapacity :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe ConsumedCapacity)
executeStatementResponse_consumedCapacity = Lens.lens (\ExecuteStatementResponse' {consumedCapacity} -> consumedCapacity) (\s@ExecuteStatementResponse' {} a -> s {consumedCapacity = a} :: ExecuteStatementResponse)

-- | If a read operation was used, this property will contain the result of
-- the read operation; a map of attribute names and their values. For the
-- write operations this value will be empty.
executeStatementResponse_items :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe [Prelude.HashMap Prelude.Text AttributeValue])
executeStatementResponse_items = Lens.lens (\ExecuteStatementResponse' {items} -> items) (\s@ExecuteStatementResponse' {} a -> s {items = a} :: ExecuteStatementResponse) Prelude.. Lens.mapping Lens.coerced

-- | The primary key of the item where the operation stopped, inclusive of
-- the previous result set. Use this value to start a new operation,
-- excluding this value in the new request. If @LastEvaluatedKey@ is empty,
-- then the \"last page\" of results has been processed and there is no
-- more data to be retrieved. If @LastEvaluatedKey@ is not empty, it does
-- not necessarily mean that there is more data in the result set. The only
-- way to know when you have reached the end of the result set is when
-- @LastEvaluatedKey@ is empty.
executeStatementResponse_lastEvaluatedKey :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
executeStatementResponse_lastEvaluatedKey = Lens.lens (\ExecuteStatementResponse' {lastEvaluatedKey} -> lastEvaluatedKey) (\s@ExecuteStatementResponse' {} a -> s {lastEvaluatedKey = a} :: ExecuteStatementResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response of a read request exceeds the response payload limit
-- DynamoDB will set this value in the response. If set, you can use that
-- this value in the subsequent request to get the remaining results.
executeStatementResponse_nextToken :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe Prelude.Text)
executeStatementResponse_nextToken = Lens.lens (\ExecuteStatementResponse' {nextToken} -> nextToken) (\s@ExecuteStatementResponse' {} a -> s {nextToken = a} :: ExecuteStatementResponse)

-- | The response's http status code.
executeStatementResponse_httpStatus :: Lens.Lens' ExecuteStatementResponse Prelude.Int
executeStatementResponse_httpStatus = Lens.lens (\ExecuteStatementResponse' {httpStatus} -> httpStatus) (\s@ExecuteStatementResponse' {} a -> s {httpStatus = a} :: ExecuteStatementResponse)

instance Prelude.NFData ExecuteStatementResponse where
  rnf ExecuteStatementResponse' {..} =
    Prelude.rnf consumedCapacity
      `Prelude.seq` Prelude.rnf items
      `Prelude.seq` Prelude.rnf lastEvaluatedKey
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
