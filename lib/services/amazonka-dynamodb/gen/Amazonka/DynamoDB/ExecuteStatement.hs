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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform reads and singleton writes on data
-- stored in DynamoDB, using PartiQL.
module Amazonka.DynamoDB.ExecuteStatement
  ( -- * Creating a Request
    ExecuteStatement (..),
    newExecuteStatement,

    -- * Request Lenses
    executeStatement_consistentRead,
    executeStatement_nextToken,
    executeStatement_parameters,
    executeStatement_statement,

    -- * Destructuring the Response
    ExecuteStatementResponse (..),
    newExecuteStatementResponse,

    -- * Response Lenses
    executeStatementResponse_items,
    executeStatementResponse_nextToken,
    executeStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExecuteStatement' smart constructor.
data ExecuteStatement = ExecuteStatement'
  { -- | The consistency of a read operation. If set to @true@, then a strongly
    -- consistent read is used; otherwise, an eventually consistent read is
    -- used.
    consistentRead :: Prelude.Maybe Prelude.Bool,
    -- | Set this value to get remaining results, if @NextToken@ was returned in
    -- the statement response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The parameters for the PartiQL statement, if any.
    parameters :: Prelude.Maybe (Prelude.NonEmpty AttributeValue),
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
-- 'nextToken', 'executeStatement_nextToken' - Set this value to get remaining results, if @NextToken@ was returned in
-- the statement response.
--
-- 'parameters', 'executeStatement_parameters' - The parameters for the PartiQL statement, if any.
--
-- 'statement', 'executeStatement_statement' - The PartiQL statement representing the operation to run.
newExecuteStatement ::
  -- | 'statement'
  Prelude.Text ->
  ExecuteStatement
newExecuteStatement pStatement_ =
  ExecuteStatement'
    { consistentRead = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      parameters = Prelude.Nothing,
      statement = pStatement_
    }

-- | The consistency of a read operation. If set to @true@, then a strongly
-- consistent read is used; otherwise, an eventually consistent read is
-- used.
executeStatement_consistentRead :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Bool)
executeStatement_consistentRead = Lens.lens (\ExecuteStatement' {consistentRead} -> consistentRead) (\s@ExecuteStatement' {} a -> s {consistentRead = a} :: ExecuteStatement)

-- | Set this value to get remaining results, if @NextToken@ was returned in
-- the statement response.
executeStatement_nextToken :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Text)
executeStatement_nextToken = Lens.lens (\ExecuteStatement' {nextToken} -> nextToken) (\s@ExecuteStatement' {} a -> s {nextToken = a} :: ExecuteStatement)

-- | The parameters for the PartiQL statement, if any.
executeStatement_parameters :: Lens.Lens' ExecuteStatement (Prelude.Maybe (Prelude.NonEmpty AttributeValue))
executeStatement_parameters = Lens.lens (\ExecuteStatement' {parameters} -> parameters) (\s@ExecuteStatement' {} a -> s {parameters = a} :: ExecuteStatement) Prelude.. Lens.mapping Lens.coerced

-- | The PartiQL statement representing the operation to run.
executeStatement_statement :: Lens.Lens' ExecuteStatement Prelude.Text
executeStatement_statement = Lens.lens (\ExecuteStatement' {statement} -> statement) (\s@ExecuteStatement' {} a -> s {statement = a} :: ExecuteStatement)

instance Core.AWSRequest ExecuteStatement where
  type
    AWSResponse ExecuteStatement =
      ExecuteStatementResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ExecuteStatementResponse'
            Prelude.<$> (x Core..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExecuteStatement where
  hashWithSalt salt' ExecuteStatement' {..} =
    salt' `Prelude.hashWithSalt` statement
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` consistentRead

instance Prelude.NFData ExecuteStatement where
  rnf ExecuteStatement' {..} =
    Prelude.rnf consistentRead
      `Prelude.seq` Prelude.rnf statement
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders ExecuteStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.ExecuteStatement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ExecuteStatement where
  toJSON ExecuteStatement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ConsistentRead" Core..=)
              Prelude.<$> consistentRead,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Parameters" Core..=) Prelude.<$> parameters,
            Prelude.Just ("Statement" Core..= statement)
          ]
      )

instance Core.ToPath ExecuteStatement where
  toPath = Prelude.const "/"

instance Core.ToQuery ExecuteStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExecuteStatementResponse' smart constructor.
data ExecuteStatementResponse = ExecuteStatementResponse'
  { -- | If a read operation was used, this property will contain the result of
    -- the reade operation; a map of attribute names and their values. For the
    -- write operations this value will be empty.
    items :: Prelude.Maybe [Prelude.HashMap Prelude.Text AttributeValue],
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
-- 'items', 'executeStatementResponse_items' - If a read operation was used, this property will contain the result of
-- the reade operation; a map of attribute names and their values. For the
-- write operations this value will be empty.
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
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a read operation was used, this property will contain the result of
-- the reade operation; a map of attribute names and their values. For the
-- write operations this value will be empty.
executeStatementResponse_items :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe [Prelude.HashMap Prelude.Text AttributeValue])
executeStatementResponse_items = Lens.lens (\ExecuteStatementResponse' {items} -> items) (\s@ExecuteStatementResponse' {} a -> s {items = a} :: ExecuteStatementResponse) Prelude.. Lens.mapping Lens.coerced

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
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
