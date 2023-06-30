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
-- Module      : Amazonka.Athena.CreatePreparedStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a prepared statement for use with SQL queries in Athena.
module Amazonka.Athena.CreatePreparedStatement
  ( -- * Creating a Request
    CreatePreparedStatement (..),
    newCreatePreparedStatement,

    -- * Request Lenses
    createPreparedStatement_description,
    createPreparedStatement_statementName,
    createPreparedStatement_workGroup,
    createPreparedStatement_queryStatement,

    -- * Destructuring the Response
    CreatePreparedStatementResponse (..),
    newCreatePreparedStatementResponse,

    -- * Response Lenses
    createPreparedStatementResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePreparedStatement' smart constructor.
data CreatePreparedStatement = CreatePreparedStatement'
  { -- | The description of the prepared statement.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the prepared statement.
    statementName :: Prelude.Text,
    -- | The name of the workgroup to which the prepared statement belongs.
    workGroup :: Prelude.Text,
    -- | The query string for the prepared statement.
    queryStatement :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePreparedStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createPreparedStatement_description' - The description of the prepared statement.
--
-- 'statementName', 'createPreparedStatement_statementName' - The name of the prepared statement.
--
-- 'workGroup', 'createPreparedStatement_workGroup' - The name of the workgroup to which the prepared statement belongs.
--
-- 'queryStatement', 'createPreparedStatement_queryStatement' - The query string for the prepared statement.
newCreatePreparedStatement ::
  -- | 'statementName'
  Prelude.Text ->
  -- | 'workGroup'
  Prelude.Text ->
  -- | 'queryStatement'
  Prelude.Text ->
  CreatePreparedStatement
newCreatePreparedStatement
  pStatementName_
  pWorkGroup_
  pQueryStatement_ =
    CreatePreparedStatement'
      { description =
          Prelude.Nothing,
        statementName = pStatementName_,
        workGroup = pWorkGroup_,
        queryStatement = pQueryStatement_
      }

-- | The description of the prepared statement.
createPreparedStatement_description :: Lens.Lens' CreatePreparedStatement (Prelude.Maybe Prelude.Text)
createPreparedStatement_description = Lens.lens (\CreatePreparedStatement' {description} -> description) (\s@CreatePreparedStatement' {} a -> s {description = a} :: CreatePreparedStatement)

-- | The name of the prepared statement.
createPreparedStatement_statementName :: Lens.Lens' CreatePreparedStatement Prelude.Text
createPreparedStatement_statementName = Lens.lens (\CreatePreparedStatement' {statementName} -> statementName) (\s@CreatePreparedStatement' {} a -> s {statementName = a} :: CreatePreparedStatement)

-- | The name of the workgroup to which the prepared statement belongs.
createPreparedStatement_workGroup :: Lens.Lens' CreatePreparedStatement Prelude.Text
createPreparedStatement_workGroup = Lens.lens (\CreatePreparedStatement' {workGroup} -> workGroup) (\s@CreatePreparedStatement' {} a -> s {workGroup = a} :: CreatePreparedStatement)

-- | The query string for the prepared statement.
createPreparedStatement_queryStatement :: Lens.Lens' CreatePreparedStatement Prelude.Text
createPreparedStatement_queryStatement = Lens.lens (\CreatePreparedStatement' {queryStatement} -> queryStatement) (\s@CreatePreparedStatement' {} a -> s {queryStatement = a} :: CreatePreparedStatement)

instance Core.AWSRequest CreatePreparedStatement where
  type
    AWSResponse CreatePreparedStatement =
      CreatePreparedStatementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreatePreparedStatementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePreparedStatement where
  hashWithSalt _salt CreatePreparedStatement' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` statementName
      `Prelude.hashWithSalt` workGroup
      `Prelude.hashWithSalt` queryStatement

instance Prelude.NFData CreatePreparedStatement where
  rnf CreatePreparedStatement' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf statementName
      `Prelude.seq` Prelude.rnf workGroup
      `Prelude.seq` Prelude.rnf queryStatement

instance Data.ToHeaders CreatePreparedStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.CreatePreparedStatement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePreparedStatement where
  toJSON CreatePreparedStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("StatementName" Data..= statementName),
            Prelude.Just ("WorkGroup" Data..= workGroup),
            Prelude.Just
              ("QueryStatement" Data..= queryStatement)
          ]
      )

instance Data.ToPath CreatePreparedStatement where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePreparedStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePreparedStatementResponse' smart constructor.
data CreatePreparedStatementResponse = CreatePreparedStatementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePreparedStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPreparedStatementResponse_httpStatus' - The response's http status code.
newCreatePreparedStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePreparedStatementResponse
newCreatePreparedStatementResponse pHttpStatus_ =
  CreatePreparedStatementResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createPreparedStatementResponse_httpStatus :: Lens.Lens' CreatePreparedStatementResponse Prelude.Int
createPreparedStatementResponse_httpStatus = Lens.lens (\CreatePreparedStatementResponse' {httpStatus} -> httpStatus) (\s@CreatePreparedStatementResponse' {} a -> s {httpStatus = a} :: CreatePreparedStatementResponse)

instance
  Prelude.NFData
    CreatePreparedStatementResponse
  where
  rnf CreatePreparedStatementResponse' {..} =
    Prelude.rnf httpStatus
