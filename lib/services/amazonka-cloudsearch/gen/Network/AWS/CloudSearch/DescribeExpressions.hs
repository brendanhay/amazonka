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
-- Module      : Network.AWS.CloudSearch.DescribeExpressions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the expressions configured for the search domain. Can be limited to
-- specific expressions by name. By default, shows all expressions and
-- includes any pending changes to the configuration. Set the @Deployed@
-- option to @true@ to show the active configuration and exclude pending
-- changes. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions>
-- in the /Amazon CloudSearch Developer Guide/.
module Network.AWS.CloudSearch.DescribeExpressions
  ( -- * Creating a Request
    DescribeExpressions (..),
    newDescribeExpressions,

    -- * Request Lenses
    describeExpressions_deployed,
    describeExpressions_expressionNames,
    describeExpressions_domainName,

    -- * Destructuring the Response
    DescribeExpressionsResponse (..),
    newDescribeExpressionsResponse,

    -- * Response Lenses
    describeExpressionsResponse_httpStatus,
    describeExpressionsResponse_expressions,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @DescribeDomains@ operation.
-- Specifies the name of the domain you want to describe. To restrict the
-- response to particular expressions, specify the names of the expressions
-- you want to describe. To show the active configuration and exclude any
-- pending changes, set the @Deployed@ option to @true@.
--
-- /See:/ 'newDescribeExpressions' smart constructor.
data DescribeExpressions = DescribeExpressions'
  { -- | Whether to display the deployed configuration (@true@) or include any
    -- pending changes (@false@). Defaults to @false@.
    deployed :: Prelude.Maybe Prelude.Bool,
    -- | Limits the @DescribeExpressions@ response to the specified expressions.
    -- If not specified, all expressions are shown.
    expressionNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the domain you want to describe.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExpressions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployed', 'describeExpressions_deployed' - Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
--
-- 'expressionNames', 'describeExpressions_expressionNames' - Limits the @DescribeExpressions@ response to the specified expressions.
-- If not specified, all expressions are shown.
--
-- 'domainName', 'describeExpressions_domainName' - The name of the domain you want to describe.
newDescribeExpressions ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeExpressions
newDescribeExpressions pDomainName_ =
  DescribeExpressions'
    { deployed = Prelude.Nothing,
      expressionNames = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
describeExpressions_deployed :: Lens.Lens' DescribeExpressions (Prelude.Maybe Prelude.Bool)
describeExpressions_deployed = Lens.lens (\DescribeExpressions' {deployed} -> deployed) (\s@DescribeExpressions' {} a -> s {deployed = a} :: DescribeExpressions)

-- | Limits the @DescribeExpressions@ response to the specified expressions.
-- If not specified, all expressions are shown.
describeExpressions_expressionNames :: Lens.Lens' DescribeExpressions (Prelude.Maybe [Prelude.Text])
describeExpressions_expressionNames = Lens.lens (\DescribeExpressions' {expressionNames} -> expressionNames) (\s@DescribeExpressions' {} a -> s {expressionNames = a} :: DescribeExpressions) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the domain you want to describe.
describeExpressions_domainName :: Lens.Lens' DescribeExpressions Prelude.Text
describeExpressions_domainName = Lens.lens (\DescribeExpressions' {domainName} -> domainName) (\s@DescribeExpressions' {} a -> s {domainName = a} :: DescribeExpressions)

instance Core.AWSRequest DescribeExpressions where
  type
    AWSResponse DescribeExpressions =
      DescribeExpressionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeExpressionsResult"
      ( \s h x ->
          DescribeExpressionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "Expressions" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable DescribeExpressions

instance Prelude.NFData DescribeExpressions

instance Core.ToHeaders DescribeExpressions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeExpressions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeExpressions where
  toQuery DescribeExpressions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeExpressions" :: Prelude.ByteString),
        "Version"
          Core.=: ("2013-01-01" :: Prelude.ByteString),
        "Deployed" Core.=: deployed,
        "ExpressionNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> expressionNames
            ),
        "DomainName" Core.=: domainName
      ]

-- | The result of a @DescribeExpressions@ request. Contains the expressions
-- configured for the domain specified in the request.
--
-- /See:/ 'newDescribeExpressionsResponse' smart constructor.
data DescribeExpressionsResponse = DescribeExpressionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The expressions configured for the domain.
    expressions :: [ExpressionStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExpressionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeExpressionsResponse_httpStatus' - The response's http status code.
--
-- 'expressions', 'describeExpressionsResponse_expressions' - The expressions configured for the domain.
newDescribeExpressionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExpressionsResponse
newDescribeExpressionsResponse pHttpStatus_ =
  DescribeExpressionsResponse'
    { httpStatus =
        pHttpStatus_,
      expressions = Prelude.mempty
    }

-- | The response's http status code.
describeExpressionsResponse_httpStatus :: Lens.Lens' DescribeExpressionsResponse Prelude.Int
describeExpressionsResponse_httpStatus = Lens.lens (\DescribeExpressionsResponse' {httpStatus} -> httpStatus) (\s@DescribeExpressionsResponse' {} a -> s {httpStatus = a} :: DescribeExpressionsResponse)

-- | The expressions configured for the domain.
describeExpressionsResponse_expressions :: Lens.Lens' DescribeExpressionsResponse [ExpressionStatus]
describeExpressionsResponse_expressions = Lens.lens (\DescribeExpressionsResponse' {expressions} -> expressions) (\s@DescribeExpressionsResponse' {} a -> s {expressions = a} :: DescribeExpressionsResponse) Prelude.. Lens._Coerce

instance Prelude.NFData DescribeExpressionsResponse
