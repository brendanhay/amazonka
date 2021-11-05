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
-- Module      : Amazonka.AuditManager.ListAssessmentReports
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of assessment reports created in Audit Manager.
module Amazonka.AuditManager.ListAssessmentReports
  ( -- * Creating a Request
    ListAssessmentReports (..),
    newListAssessmentReports,

    -- * Request Lenses
    listAssessmentReports_nextToken,
    listAssessmentReports_maxResults,

    -- * Destructuring the Response
    ListAssessmentReportsResponse (..),
    newListAssessmentReportsResponse,

    -- * Response Lenses
    listAssessmentReportsResponse_assessmentReports,
    listAssessmentReportsResponse_nextToken,
    listAssessmentReportsResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssessmentReports' smart constructor.
data ListAssessmentReports = ListAssessmentReports'
  { -- | The pagination token used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Represents the maximum number of results per page, or per API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentReports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentReports_nextToken' - The pagination token used to fetch the next set of results.
--
-- 'maxResults', 'listAssessmentReports_maxResults' - Represents the maximum number of results per page, or per API request
-- call.
newListAssessmentReports ::
  ListAssessmentReports
newListAssessmentReports =
  ListAssessmentReports'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token used to fetch the next set of results.
listAssessmentReports_nextToken :: Lens.Lens' ListAssessmentReports (Prelude.Maybe Prelude.Text)
listAssessmentReports_nextToken = Lens.lens (\ListAssessmentReports' {nextToken} -> nextToken) (\s@ListAssessmentReports' {} a -> s {nextToken = a} :: ListAssessmentReports)

-- | Represents the maximum number of results per page, or per API request
-- call.
listAssessmentReports_maxResults :: Lens.Lens' ListAssessmentReports (Prelude.Maybe Prelude.Natural)
listAssessmentReports_maxResults = Lens.lens (\ListAssessmentReports' {maxResults} -> maxResults) (\s@ListAssessmentReports' {} a -> s {maxResults = a} :: ListAssessmentReports)

instance Core.AWSRequest ListAssessmentReports where
  type
    AWSResponse ListAssessmentReports =
      ListAssessmentReportsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentReportsResponse'
            Prelude.<$> ( x Core..?> "assessmentReports"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssessmentReports

instance Prelude.NFData ListAssessmentReports

instance Core.ToHeaders ListAssessmentReports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListAssessmentReports where
  toPath = Prelude.const "/assessmentReports"

instance Core.ToQuery ListAssessmentReports where
  toQuery ListAssessmentReports' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListAssessmentReportsResponse' smart constructor.
data ListAssessmentReportsResponse = ListAssessmentReportsResponse'
  { -- | The list of assessment reports returned by the @ListAssessmentReports@
    -- API.
    assessmentReports :: Prelude.Maybe [AssessmentReportMetadata],
    -- | The pagination token used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentReportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentReports', 'listAssessmentReportsResponse_assessmentReports' - The list of assessment reports returned by the @ListAssessmentReports@
-- API.
--
-- 'nextToken', 'listAssessmentReportsResponse_nextToken' - The pagination token used to fetch the next set of results.
--
-- 'httpStatus', 'listAssessmentReportsResponse_httpStatus' - The response's http status code.
newListAssessmentReportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssessmentReportsResponse
newListAssessmentReportsResponse pHttpStatus_ =
  ListAssessmentReportsResponse'
    { assessmentReports =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of assessment reports returned by the @ListAssessmentReports@
-- API.
listAssessmentReportsResponse_assessmentReports :: Lens.Lens' ListAssessmentReportsResponse (Prelude.Maybe [AssessmentReportMetadata])
listAssessmentReportsResponse_assessmentReports = Lens.lens (\ListAssessmentReportsResponse' {assessmentReports} -> assessmentReports) (\s@ListAssessmentReportsResponse' {} a -> s {assessmentReports = a} :: ListAssessmentReportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token used to fetch the next set of results.
listAssessmentReportsResponse_nextToken :: Lens.Lens' ListAssessmentReportsResponse (Prelude.Maybe Prelude.Text)
listAssessmentReportsResponse_nextToken = Lens.lens (\ListAssessmentReportsResponse' {nextToken} -> nextToken) (\s@ListAssessmentReportsResponse' {} a -> s {nextToken = a} :: ListAssessmentReportsResponse)

-- | The response's http status code.
listAssessmentReportsResponse_httpStatus :: Lens.Lens' ListAssessmentReportsResponse Prelude.Int
listAssessmentReportsResponse_httpStatus = Lens.lens (\ListAssessmentReportsResponse' {httpStatus} -> httpStatus) (\s@ListAssessmentReportsResponse' {} a -> s {httpStatus = a} :: ListAssessmentReportsResponse)

instance Prelude.NFData ListAssessmentReportsResponse
