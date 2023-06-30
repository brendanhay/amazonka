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
-- Module      : Amazonka.CloudSearch.DefineExpression
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an @Expression@ for the search domain. Used to create new
-- expressions and modify existing ones. If the expression exists, the new
-- configuration replaces the old one. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions>
-- in the /Amazon CloudSearch Developer Guide/.
module Amazonka.CloudSearch.DefineExpression
  ( -- * Creating a Request
    DefineExpression (..),
    newDefineExpression,

    -- * Request Lenses
    defineExpression_domainName,
    defineExpression_expression,

    -- * Destructuring the Response
    DefineExpressionResponse (..),
    newDefineExpressionResponse,

    -- * Response Lenses
    defineExpressionResponse_httpStatus,
    defineExpressionResponse_expression,
  )
where

import Amazonka.CloudSearch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DefineExpression@ operation.
-- Specifies the name of the domain you want to update and the expression
-- you want to configure.
--
-- /See:/ 'newDefineExpression' smart constructor.
data DefineExpression = DefineExpression'
  { domainName :: Prelude.Text,
    expression :: Expression
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefineExpression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'defineExpression_domainName' - Undocumented member.
--
-- 'expression', 'defineExpression_expression' - Undocumented member.
newDefineExpression ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'expression'
  Expression ->
  DefineExpression
newDefineExpression pDomainName_ pExpression_ =
  DefineExpression'
    { domainName = pDomainName_,
      expression = pExpression_
    }

-- | Undocumented member.
defineExpression_domainName :: Lens.Lens' DefineExpression Prelude.Text
defineExpression_domainName = Lens.lens (\DefineExpression' {domainName} -> domainName) (\s@DefineExpression' {} a -> s {domainName = a} :: DefineExpression)

-- | Undocumented member.
defineExpression_expression :: Lens.Lens' DefineExpression Expression
defineExpression_expression = Lens.lens (\DefineExpression' {expression} -> expression) (\s@DefineExpression' {} a -> s {expression = a} :: DefineExpression)

instance Core.AWSRequest DefineExpression where
  type
    AWSResponse DefineExpression =
      DefineExpressionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DefineExpressionResult"
      ( \s h x ->
          DefineExpressionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "Expression")
      )

instance Prelude.Hashable DefineExpression where
  hashWithSalt _salt DefineExpression' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` expression

instance Prelude.NFData DefineExpression where
  rnf DefineExpression' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf expression

instance Data.ToHeaders DefineExpression where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DefineExpression where
  toPath = Prelude.const "/"

instance Data.ToQuery DefineExpression where
  toQuery DefineExpression' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DefineExpression" :: Prelude.ByteString),
        "Version"
          Data.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Data.=: domainName,
        "Expression" Data.=: expression
      ]

-- | The result of a @DefineExpression@ request. Contains the status of the
-- newly-configured expression.
--
-- /See:/ 'newDefineExpressionResponse' smart constructor.
data DefineExpressionResponse = DefineExpressionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    expression :: ExpressionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefineExpressionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'defineExpressionResponse_httpStatus' - The response's http status code.
--
-- 'expression', 'defineExpressionResponse_expression' - Undocumented member.
newDefineExpressionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'expression'
  ExpressionStatus ->
  DefineExpressionResponse
newDefineExpressionResponse pHttpStatus_ pExpression_ =
  DefineExpressionResponse'
    { httpStatus =
        pHttpStatus_,
      expression = pExpression_
    }

-- | The response's http status code.
defineExpressionResponse_httpStatus :: Lens.Lens' DefineExpressionResponse Prelude.Int
defineExpressionResponse_httpStatus = Lens.lens (\DefineExpressionResponse' {httpStatus} -> httpStatus) (\s@DefineExpressionResponse' {} a -> s {httpStatus = a} :: DefineExpressionResponse)

-- | Undocumented member.
defineExpressionResponse_expression :: Lens.Lens' DefineExpressionResponse ExpressionStatus
defineExpressionResponse_expression = Lens.lens (\DefineExpressionResponse' {expression} -> expression) (\s@DefineExpressionResponse' {} a -> s {expression = a} :: DefineExpressionResponse)

instance Prelude.NFData DefineExpressionResponse where
  rnf DefineExpressionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf expression
