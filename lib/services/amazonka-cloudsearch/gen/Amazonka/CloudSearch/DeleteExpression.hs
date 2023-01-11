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
-- Module      : Amazonka.CloudSearch.DeleteExpression
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an @Expression@ from the search domain. For more information,
-- see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions>
-- in the /Amazon CloudSearch Developer Guide/.
module Amazonka.CloudSearch.DeleteExpression
  ( -- * Creating a Request
    DeleteExpression (..),
    newDeleteExpression,

    -- * Request Lenses
    deleteExpression_domainName,
    deleteExpression_expressionName,

    -- * Destructuring the Response
    DeleteExpressionResponse (..),
    newDeleteExpressionResponse,

    -- * Response Lenses
    deleteExpressionResponse_httpStatus,
    deleteExpressionResponse_expression,
  )
where

import Amazonka.CloudSearch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DeleteExpression@ operation.
-- Specifies the name of the domain you want to update and the name of the
-- expression you want to delete.
--
-- /See:/ 'newDeleteExpression' smart constructor.
data DeleteExpression = DeleteExpression'
  { domainName :: Prelude.Text,
    -- | The name of the @Expression@ to delete.
    expressionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExpression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteExpression_domainName' - Undocumented member.
--
-- 'expressionName', 'deleteExpression_expressionName' - The name of the @Expression@ to delete.
newDeleteExpression ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'expressionName'
  Prelude.Text ->
  DeleteExpression
newDeleteExpression pDomainName_ pExpressionName_ =
  DeleteExpression'
    { domainName = pDomainName_,
      expressionName = pExpressionName_
    }

-- | Undocumented member.
deleteExpression_domainName :: Lens.Lens' DeleteExpression Prelude.Text
deleteExpression_domainName = Lens.lens (\DeleteExpression' {domainName} -> domainName) (\s@DeleteExpression' {} a -> s {domainName = a} :: DeleteExpression)

-- | The name of the @Expression@ to delete.
deleteExpression_expressionName :: Lens.Lens' DeleteExpression Prelude.Text
deleteExpression_expressionName = Lens.lens (\DeleteExpression' {expressionName} -> expressionName) (\s@DeleteExpression' {} a -> s {expressionName = a} :: DeleteExpression)

instance Core.AWSRequest DeleteExpression where
  type
    AWSResponse DeleteExpression =
      DeleteExpressionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteExpressionResult"
      ( \s h x ->
          DeleteExpressionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "Expression")
      )

instance Prelude.Hashable DeleteExpression where
  hashWithSalt _salt DeleteExpression' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` expressionName

instance Prelude.NFData DeleteExpression where
  rnf DeleteExpression' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf expressionName

instance Data.ToHeaders DeleteExpression where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteExpression where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteExpression where
  toQuery DeleteExpression' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteExpression" :: Prelude.ByteString),
        "Version"
          Data.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Data.=: domainName,
        "ExpressionName" Data.=: expressionName
      ]

-- | The result of a @DeleteExpression@ request. Specifies the expression
-- being deleted.
--
-- /See:/ 'newDeleteExpressionResponse' smart constructor.
data DeleteExpressionResponse = DeleteExpressionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the expression being deleted.
    expression :: ExpressionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExpressionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteExpressionResponse_httpStatus' - The response's http status code.
--
-- 'expression', 'deleteExpressionResponse_expression' - The status of the expression being deleted.
newDeleteExpressionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'expression'
  ExpressionStatus ->
  DeleteExpressionResponse
newDeleteExpressionResponse pHttpStatus_ pExpression_ =
  DeleteExpressionResponse'
    { httpStatus =
        pHttpStatus_,
      expression = pExpression_
    }

-- | The response's http status code.
deleteExpressionResponse_httpStatus :: Lens.Lens' DeleteExpressionResponse Prelude.Int
deleteExpressionResponse_httpStatus = Lens.lens (\DeleteExpressionResponse' {httpStatus} -> httpStatus) (\s@DeleteExpressionResponse' {} a -> s {httpStatus = a} :: DeleteExpressionResponse)

-- | The status of the expression being deleted.
deleteExpressionResponse_expression :: Lens.Lens' DeleteExpressionResponse ExpressionStatus
deleteExpressionResponse_expression = Lens.lens (\DeleteExpressionResponse' {expression} -> expression) (\s@DeleteExpressionResponse' {} a -> s {expression = a} :: DeleteExpressionResponse)

instance Prelude.NFData DeleteExpressionResponse where
  rnf DeleteExpressionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf expression
