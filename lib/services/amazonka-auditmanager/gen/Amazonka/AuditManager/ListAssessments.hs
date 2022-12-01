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
-- Module      : Amazonka.AuditManager.ListAssessments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of current and past assessments from Audit Manager.
module Amazonka.AuditManager.ListAssessments
  ( -- * Creating a Request
    ListAssessments (..),
    newListAssessments,

    -- * Request Lenses
    listAssessments_nextToken,
    listAssessments_status,
    listAssessments_maxResults,

    -- * Destructuring the Response
    ListAssessmentsResponse (..),
    newListAssessmentsResponse,

    -- * Response Lenses
    listAssessmentsResponse_nextToken,
    listAssessmentsResponse_assessmentMetadata,
    listAssessmentsResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssessments' smart constructor.
data ListAssessments = ListAssessments'
  { -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The current status of the assessment.
    status :: Prelude.Maybe AssessmentStatus,
    -- | Represents the maximum number of results on a page or for an API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessments_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'status', 'listAssessments_status' - The current status of the assessment.
--
-- 'maxResults', 'listAssessments_maxResults' - Represents the maximum number of results on a page or for an API request
-- call.
newListAssessments ::
  ListAssessments
newListAssessments =
  ListAssessments'
    { nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token that\'s used to fetch the next set of results.
listAssessments_nextToken :: Lens.Lens' ListAssessments (Prelude.Maybe Prelude.Text)
listAssessments_nextToken = Lens.lens (\ListAssessments' {nextToken} -> nextToken) (\s@ListAssessments' {} a -> s {nextToken = a} :: ListAssessments)

-- | The current status of the assessment.
listAssessments_status :: Lens.Lens' ListAssessments (Prelude.Maybe AssessmentStatus)
listAssessments_status = Lens.lens (\ListAssessments' {status} -> status) (\s@ListAssessments' {} a -> s {status = a} :: ListAssessments)

-- | Represents the maximum number of results on a page or for an API request
-- call.
listAssessments_maxResults :: Lens.Lens' ListAssessments (Prelude.Maybe Prelude.Natural)
listAssessments_maxResults = Lens.lens (\ListAssessments' {maxResults} -> maxResults) (\s@ListAssessments' {} a -> s {maxResults = a} :: ListAssessments)

instance Core.AWSRequest ListAssessments where
  type
    AWSResponse ListAssessments =
      ListAssessmentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "assessmentMetadata"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssessments where
  hashWithSalt _salt ListAssessments' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListAssessments where
  rnf ListAssessments' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListAssessments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListAssessments where
  toPath = Prelude.const "/assessments"

instance Core.ToQuery ListAssessments where
  toQuery ListAssessments' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "status" Core.=: status,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListAssessmentsResponse' smart constructor.
data ListAssessmentsResponse = ListAssessmentsResponse'
  { -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The metadata that\'s associated with the assessment.
    assessmentMetadata :: Prelude.Maybe [AssessmentMetadataItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentsResponse_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'assessmentMetadata', 'listAssessmentsResponse_assessmentMetadata' - The metadata that\'s associated with the assessment.
--
-- 'httpStatus', 'listAssessmentsResponse_httpStatus' - The response's http status code.
newListAssessmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssessmentsResponse
newListAssessmentsResponse pHttpStatus_ =
  ListAssessmentsResponse'
    { nextToken =
        Prelude.Nothing,
      assessmentMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that\'s used to fetch the next set of results.
listAssessmentsResponse_nextToken :: Lens.Lens' ListAssessmentsResponse (Prelude.Maybe Prelude.Text)
listAssessmentsResponse_nextToken = Lens.lens (\ListAssessmentsResponse' {nextToken} -> nextToken) (\s@ListAssessmentsResponse' {} a -> s {nextToken = a} :: ListAssessmentsResponse)

-- | The metadata that\'s associated with the assessment.
listAssessmentsResponse_assessmentMetadata :: Lens.Lens' ListAssessmentsResponse (Prelude.Maybe [AssessmentMetadataItem])
listAssessmentsResponse_assessmentMetadata = Lens.lens (\ListAssessmentsResponse' {assessmentMetadata} -> assessmentMetadata) (\s@ListAssessmentsResponse' {} a -> s {assessmentMetadata = a} :: ListAssessmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAssessmentsResponse_httpStatus :: Lens.Lens' ListAssessmentsResponse Prelude.Int
listAssessmentsResponse_httpStatus = Lens.lens (\ListAssessmentsResponse' {httpStatus} -> httpStatus) (\s@ListAssessmentsResponse' {} a -> s {httpStatus = a} :: ListAssessmentsResponse)

instance Prelude.NFData ListAssessmentsResponse where
  rnf ListAssessmentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf assessmentMetadata
      `Prelude.seq` Prelude.rnf httpStatus
