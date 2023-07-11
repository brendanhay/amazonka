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
-- Module      : Amazonka.PinpointEmail.ListDeliverabilityTestReports
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Show a list of the predictive inbox placement tests that you\'ve
-- performed, regardless of their statuses. For predictive inbox placement
-- tests that are complete, you can use the @GetDeliverabilityTestReport@
-- operation to view the results.
--
-- This operation returns paginated results.
module Amazonka.PinpointEmail.ListDeliverabilityTestReports
  ( -- * Creating a Request
    ListDeliverabilityTestReports (..),
    newListDeliverabilityTestReports,

    -- * Request Lenses
    listDeliverabilityTestReports_nextToken,
    listDeliverabilityTestReports_pageSize,

    -- * Destructuring the Response
    ListDeliverabilityTestReportsResponse (..),
    newListDeliverabilityTestReportsResponse,

    -- * Response Lenses
    listDeliverabilityTestReportsResponse_nextToken,
    listDeliverabilityTestReportsResponse_httpStatus,
    listDeliverabilityTestReportsResponse_deliverabilityTestReports,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to list all of the predictive inbox placement tests that
-- you\'ve performed.
--
-- /See:/ 'newListDeliverabilityTestReports' smart constructor.
data ListDeliverabilityTestReports = ListDeliverabilityTestReports'
  { -- | A token returned from a previous call to @ListDeliverabilityTestReports@
    -- to indicate the position in the list of predictive inbox placement
    -- tests.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of results to show in a single call to
    -- @ListDeliverabilityTestReports@. If the number of results is larger than
    -- the number you specified in this parameter, then the response includes a
    -- @NextToken@ element, which you can use to obtain additional results.
    --
    -- The value you specify has to be at least 0, and can be no more than
    -- 1000.
    pageSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeliverabilityTestReports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeliverabilityTestReports_nextToken' - A token returned from a previous call to @ListDeliverabilityTestReports@
-- to indicate the position in the list of predictive inbox placement
-- tests.
--
-- 'pageSize', 'listDeliverabilityTestReports_pageSize' - The number of results to show in a single call to
-- @ListDeliverabilityTestReports@. If the number of results is larger than
-- the number you specified in this parameter, then the response includes a
-- @NextToken@ element, which you can use to obtain additional results.
--
-- The value you specify has to be at least 0, and can be no more than
-- 1000.
newListDeliverabilityTestReports ::
  ListDeliverabilityTestReports
newListDeliverabilityTestReports =
  ListDeliverabilityTestReports'
    { nextToken =
        Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A token returned from a previous call to @ListDeliverabilityTestReports@
-- to indicate the position in the list of predictive inbox placement
-- tests.
listDeliverabilityTestReports_nextToken :: Lens.Lens' ListDeliverabilityTestReports (Prelude.Maybe Prelude.Text)
listDeliverabilityTestReports_nextToken = Lens.lens (\ListDeliverabilityTestReports' {nextToken} -> nextToken) (\s@ListDeliverabilityTestReports' {} a -> s {nextToken = a} :: ListDeliverabilityTestReports)

-- | The number of results to show in a single call to
-- @ListDeliverabilityTestReports@. If the number of results is larger than
-- the number you specified in this parameter, then the response includes a
-- @NextToken@ element, which you can use to obtain additional results.
--
-- The value you specify has to be at least 0, and can be no more than
-- 1000.
listDeliverabilityTestReports_pageSize :: Lens.Lens' ListDeliverabilityTestReports (Prelude.Maybe Prelude.Int)
listDeliverabilityTestReports_pageSize = Lens.lens (\ListDeliverabilityTestReports' {pageSize} -> pageSize) (\s@ListDeliverabilityTestReports' {} a -> s {pageSize = a} :: ListDeliverabilityTestReports)

instance Core.AWSPager ListDeliverabilityTestReports where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeliverabilityTestReportsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listDeliverabilityTestReportsResponse_deliverabilityTestReports
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDeliverabilityTestReports_nextToken
          Lens..~ rs
          Lens.^? listDeliverabilityTestReportsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListDeliverabilityTestReports
  where
  type
    AWSResponse ListDeliverabilityTestReports =
      ListDeliverabilityTestReportsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeliverabilityTestReportsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "DeliverabilityTestReports"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListDeliverabilityTestReports
  where
  hashWithSalt _salt ListDeliverabilityTestReports' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData ListDeliverabilityTestReports where
  rnf ListDeliverabilityTestReports' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize

instance Data.ToHeaders ListDeliverabilityTestReports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDeliverabilityTestReports where
  toPath =
    Prelude.const
      "/v1/email/deliverability-dashboard/test-reports"

instance Data.ToQuery ListDeliverabilityTestReports where
  toQuery ListDeliverabilityTestReports' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "PageSize" Data.=: pageSize
      ]

-- | A list of the predictive inbox placement test reports that are available
-- for your account, regardless of whether or not those tests are complete.
--
-- /See:/ 'newListDeliverabilityTestReportsResponse' smart constructor.
data ListDeliverabilityTestReportsResponse = ListDeliverabilityTestReportsResponse'
  { -- | A token that indicates that there are additional predictive inbox
    -- placement tests to list. To view additional predictive inbox placement
    -- tests, issue another request to @ListDeliverabilityTestReports@, and
    -- pass this token in the @NextToken@ parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object that contains a lists of predictive inbox placement tests that
    -- you\'ve performed.
    deliverabilityTestReports :: [DeliverabilityTestReport]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeliverabilityTestReportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeliverabilityTestReportsResponse_nextToken' - A token that indicates that there are additional predictive inbox
-- placement tests to list. To view additional predictive inbox placement
-- tests, issue another request to @ListDeliverabilityTestReports@, and
-- pass this token in the @NextToken@ parameter.
--
-- 'httpStatus', 'listDeliverabilityTestReportsResponse_httpStatus' - The response's http status code.
--
-- 'deliverabilityTestReports', 'listDeliverabilityTestReportsResponse_deliverabilityTestReports' - An object that contains a lists of predictive inbox placement tests that
-- you\'ve performed.
newListDeliverabilityTestReportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeliverabilityTestReportsResponse
newListDeliverabilityTestReportsResponse pHttpStatus_ =
  ListDeliverabilityTestReportsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      deliverabilityTestReports =
        Prelude.mempty
    }

-- | A token that indicates that there are additional predictive inbox
-- placement tests to list. To view additional predictive inbox placement
-- tests, issue another request to @ListDeliverabilityTestReports@, and
-- pass this token in the @NextToken@ parameter.
listDeliverabilityTestReportsResponse_nextToken :: Lens.Lens' ListDeliverabilityTestReportsResponse (Prelude.Maybe Prelude.Text)
listDeliverabilityTestReportsResponse_nextToken = Lens.lens (\ListDeliverabilityTestReportsResponse' {nextToken} -> nextToken) (\s@ListDeliverabilityTestReportsResponse' {} a -> s {nextToken = a} :: ListDeliverabilityTestReportsResponse)

-- | The response's http status code.
listDeliverabilityTestReportsResponse_httpStatus :: Lens.Lens' ListDeliverabilityTestReportsResponse Prelude.Int
listDeliverabilityTestReportsResponse_httpStatus = Lens.lens (\ListDeliverabilityTestReportsResponse' {httpStatus} -> httpStatus) (\s@ListDeliverabilityTestReportsResponse' {} a -> s {httpStatus = a} :: ListDeliverabilityTestReportsResponse)

-- | An object that contains a lists of predictive inbox placement tests that
-- you\'ve performed.
listDeliverabilityTestReportsResponse_deliverabilityTestReports :: Lens.Lens' ListDeliverabilityTestReportsResponse [DeliverabilityTestReport]
listDeliverabilityTestReportsResponse_deliverabilityTestReports = Lens.lens (\ListDeliverabilityTestReportsResponse' {deliverabilityTestReports} -> deliverabilityTestReports) (\s@ListDeliverabilityTestReportsResponse' {} a -> s {deliverabilityTestReports = a} :: ListDeliverabilityTestReportsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListDeliverabilityTestReportsResponse
  where
  rnf ListDeliverabilityTestReportsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf deliverabilityTestReports
