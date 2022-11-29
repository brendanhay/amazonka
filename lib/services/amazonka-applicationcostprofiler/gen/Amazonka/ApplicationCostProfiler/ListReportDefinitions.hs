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
-- Module      : Amazonka.ApplicationCostProfiler.ListReportDefinitions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all reports and their configurations for your AWS
-- account.
--
-- The maximum number of reports is one.
--
-- This operation returns paginated results.
module Amazonka.ApplicationCostProfiler.ListReportDefinitions
  ( -- * Creating a Request
    ListReportDefinitions (..),
    newListReportDefinitions,

    -- * Request Lenses
    listReportDefinitions_nextToken,
    listReportDefinitions_maxResults,

    -- * Destructuring the Response
    ListReportDefinitionsResponse (..),
    newListReportDefinitionsResponse,

    -- * Response Lenses
    listReportDefinitionsResponse_nextToken,
    listReportDefinitionsResponse_reportDefinitions,
    listReportDefinitionsResponse_httpStatus,
  )
where

import Amazonka.ApplicationCostProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReportDefinitions' smart constructor.
data ListReportDefinitions = ListReportDefinitions'
  { -- | The token value from a previous call to access the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReportDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReportDefinitions_nextToken' - The token value from a previous call to access the next page of results.
--
-- 'maxResults', 'listReportDefinitions_maxResults' - The maximum number of results to return.
newListReportDefinitions ::
  ListReportDefinitions
newListReportDefinitions =
  ListReportDefinitions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token value from a previous call to access the next page of results.
listReportDefinitions_nextToken :: Lens.Lens' ListReportDefinitions (Prelude.Maybe Prelude.Text)
listReportDefinitions_nextToken = Lens.lens (\ListReportDefinitions' {nextToken} -> nextToken) (\s@ListReportDefinitions' {} a -> s {nextToken = a} :: ListReportDefinitions)

-- | The maximum number of results to return.
listReportDefinitions_maxResults :: Lens.Lens' ListReportDefinitions (Prelude.Maybe Prelude.Natural)
listReportDefinitions_maxResults = Lens.lens (\ListReportDefinitions' {maxResults} -> maxResults) (\s@ListReportDefinitions' {} a -> s {maxResults = a} :: ListReportDefinitions)

instance Core.AWSPager ListReportDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReportDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listReportDefinitionsResponse_reportDefinitions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listReportDefinitions_nextToken
          Lens..~ rs
          Lens.^? listReportDefinitionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListReportDefinitions where
  type
    AWSResponse ListReportDefinitions =
      ListReportDefinitionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReportDefinitionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "reportDefinitions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReportDefinitions where
  hashWithSalt _salt ListReportDefinitions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListReportDefinitions where
  rnf ListReportDefinitions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListReportDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListReportDefinitions where
  toPath = Prelude.const "/reportDefinition"

instance Core.ToQuery ListReportDefinitions where
  toQuery ListReportDefinitions' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListReportDefinitionsResponse' smart constructor.
data ListReportDefinitionsResponse = ListReportDefinitionsResponse'
  { -- | The value of the next token, if it exists. Null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The retrieved reports.
    reportDefinitions :: Prelude.Maybe [ReportDefinition],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReportDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReportDefinitionsResponse_nextToken' - The value of the next token, if it exists. Null if there are no more
-- results.
--
-- 'reportDefinitions', 'listReportDefinitionsResponse_reportDefinitions' - The retrieved reports.
--
-- 'httpStatus', 'listReportDefinitionsResponse_httpStatus' - The response's http status code.
newListReportDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReportDefinitionsResponse
newListReportDefinitionsResponse pHttpStatus_ =
  ListReportDefinitionsResponse'
    { nextToken =
        Prelude.Nothing,
      reportDefinitions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The value of the next token, if it exists. Null if there are no more
-- results.
listReportDefinitionsResponse_nextToken :: Lens.Lens' ListReportDefinitionsResponse (Prelude.Maybe Prelude.Text)
listReportDefinitionsResponse_nextToken = Lens.lens (\ListReportDefinitionsResponse' {nextToken} -> nextToken) (\s@ListReportDefinitionsResponse' {} a -> s {nextToken = a} :: ListReportDefinitionsResponse)

-- | The retrieved reports.
listReportDefinitionsResponse_reportDefinitions :: Lens.Lens' ListReportDefinitionsResponse (Prelude.Maybe [ReportDefinition])
listReportDefinitionsResponse_reportDefinitions = Lens.lens (\ListReportDefinitionsResponse' {reportDefinitions} -> reportDefinitions) (\s@ListReportDefinitionsResponse' {} a -> s {reportDefinitions = a} :: ListReportDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReportDefinitionsResponse_httpStatus :: Lens.Lens' ListReportDefinitionsResponse Prelude.Int
listReportDefinitionsResponse_httpStatus = Lens.lens (\ListReportDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListReportDefinitionsResponse' {} a -> s {httpStatus = a} :: ListReportDefinitionsResponse)

instance Prelude.NFData ListReportDefinitionsResponse where
  rnf ListReportDefinitionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reportDefinitions
      `Prelude.seq` Prelude.rnf httpStatus
