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
-- Module      : Amazonka.AuditManager.ListAssessmentFrameworkShareRequests
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of sent or received share requests for custom frameworks
-- in Audit Manager.
module Amazonka.AuditManager.ListAssessmentFrameworkShareRequests
  ( -- * Creating a Request
    ListAssessmentFrameworkShareRequests (..),
    newListAssessmentFrameworkShareRequests,

    -- * Request Lenses
    listAssessmentFrameworkShareRequests_maxResults,
    listAssessmentFrameworkShareRequests_nextToken,
    listAssessmentFrameworkShareRequests_requestType,

    -- * Destructuring the Response
    ListAssessmentFrameworkShareRequestsResponse (..),
    newListAssessmentFrameworkShareRequestsResponse,

    -- * Response Lenses
    listAssessmentFrameworkShareRequestsResponse_assessmentFrameworkShareRequests,
    listAssessmentFrameworkShareRequestsResponse_nextToken,
    listAssessmentFrameworkShareRequestsResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssessmentFrameworkShareRequests' smart constructor.
data ListAssessmentFrameworkShareRequests = ListAssessmentFrameworkShareRequests'
  { -- | Represents the maximum number of results on a page or for an API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the share request is a sent request or a received
    -- request.
    requestType :: ShareRequestType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentFrameworkShareRequests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAssessmentFrameworkShareRequests_maxResults' - Represents the maximum number of results on a page or for an API request
-- call.
--
-- 'nextToken', 'listAssessmentFrameworkShareRequests_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'requestType', 'listAssessmentFrameworkShareRequests_requestType' - Specifies whether the share request is a sent request or a received
-- request.
newListAssessmentFrameworkShareRequests ::
  -- | 'requestType'
  ShareRequestType ->
  ListAssessmentFrameworkShareRequests
newListAssessmentFrameworkShareRequests pRequestType_ =
  ListAssessmentFrameworkShareRequests'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestType = pRequestType_
    }

-- | Represents the maximum number of results on a page or for an API request
-- call.
listAssessmentFrameworkShareRequests_maxResults :: Lens.Lens' ListAssessmentFrameworkShareRequests (Prelude.Maybe Prelude.Natural)
listAssessmentFrameworkShareRequests_maxResults = Lens.lens (\ListAssessmentFrameworkShareRequests' {maxResults} -> maxResults) (\s@ListAssessmentFrameworkShareRequests' {} a -> s {maxResults = a} :: ListAssessmentFrameworkShareRequests)

-- | The pagination token that\'s used to fetch the next set of results.
listAssessmentFrameworkShareRequests_nextToken :: Lens.Lens' ListAssessmentFrameworkShareRequests (Prelude.Maybe Prelude.Text)
listAssessmentFrameworkShareRequests_nextToken = Lens.lens (\ListAssessmentFrameworkShareRequests' {nextToken} -> nextToken) (\s@ListAssessmentFrameworkShareRequests' {} a -> s {nextToken = a} :: ListAssessmentFrameworkShareRequests)

-- | Specifies whether the share request is a sent request or a received
-- request.
listAssessmentFrameworkShareRequests_requestType :: Lens.Lens' ListAssessmentFrameworkShareRequests ShareRequestType
listAssessmentFrameworkShareRequests_requestType = Lens.lens (\ListAssessmentFrameworkShareRequests' {requestType} -> requestType) (\s@ListAssessmentFrameworkShareRequests' {} a -> s {requestType = a} :: ListAssessmentFrameworkShareRequests)

instance
  Core.AWSRequest
    ListAssessmentFrameworkShareRequests
  where
  type
    AWSResponse ListAssessmentFrameworkShareRequests =
      ListAssessmentFrameworkShareRequestsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentFrameworkShareRequestsResponse'
            Prelude.<$> ( x Data..?> "assessmentFrameworkShareRequests"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Data..?> "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAssessmentFrameworkShareRequests
  where
  hashWithSalt
    _salt
    ListAssessmentFrameworkShareRequests' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` requestType

instance
  Prelude.NFData
    ListAssessmentFrameworkShareRequests
  where
  rnf ListAssessmentFrameworkShareRequests' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestType

instance
  Data.ToHeaders
    ListAssessmentFrameworkShareRequests
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    ListAssessmentFrameworkShareRequests
  where
  toPath =
    Prelude.const "/assessmentFrameworkShareRequests"

instance
  Data.ToQuery
    ListAssessmentFrameworkShareRequests
  where
  toQuery ListAssessmentFrameworkShareRequests' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "requestType" Data.=: requestType
      ]

-- | /See:/ 'newListAssessmentFrameworkShareRequestsResponse' smart constructor.
data ListAssessmentFrameworkShareRequestsResponse = ListAssessmentFrameworkShareRequestsResponse'
  { -- | The list of share requests that the
    -- @ListAssessmentFrameworkShareRequests@ API returned.
    assessmentFrameworkShareRequests :: Prelude.Maybe [AssessmentFrameworkShareRequest],
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentFrameworkShareRequestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentFrameworkShareRequests', 'listAssessmentFrameworkShareRequestsResponse_assessmentFrameworkShareRequests' - The list of share requests that the
-- @ListAssessmentFrameworkShareRequests@ API returned.
--
-- 'nextToken', 'listAssessmentFrameworkShareRequestsResponse_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'httpStatus', 'listAssessmentFrameworkShareRequestsResponse_httpStatus' - The response's http status code.
newListAssessmentFrameworkShareRequestsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssessmentFrameworkShareRequestsResponse
newListAssessmentFrameworkShareRequestsResponse
  pHttpStatus_ =
    ListAssessmentFrameworkShareRequestsResponse'
      { assessmentFrameworkShareRequests =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of share requests that the
-- @ListAssessmentFrameworkShareRequests@ API returned.
listAssessmentFrameworkShareRequestsResponse_assessmentFrameworkShareRequests :: Lens.Lens' ListAssessmentFrameworkShareRequestsResponse (Prelude.Maybe [AssessmentFrameworkShareRequest])
listAssessmentFrameworkShareRequestsResponse_assessmentFrameworkShareRequests = Lens.lens (\ListAssessmentFrameworkShareRequestsResponse' {assessmentFrameworkShareRequests} -> assessmentFrameworkShareRequests) (\s@ListAssessmentFrameworkShareRequestsResponse' {} a -> s {assessmentFrameworkShareRequests = a} :: ListAssessmentFrameworkShareRequestsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s used to fetch the next set of results.
listAssessmentFrameworkShareRequestsResponse_nextToken :: Lens.Lens' ListAssessmentFrameworkShareRequestsResponse (Prelude.Maybe Prelude.Text)
listAssessmentFrameworkShareRequestsResponse_nextToken = Lens.lens (\ListAssessmentFrameworkShareRequestsResponse' {nextToken} -> nextToken) (\s@ListAssessmentFrameworkShareRequestsResponse' {} a -> s {nextToken = a} :: ListAssessmentFrameworkShareRequestsResponse)

-- | The response's http status code.
listAssessmentFrameworkShareRequestsResponse_httpStatus :: Lens.Lens' ListAssessmentFrameworkShareRequestsResponse Prelude.Int
listAssessmentFrameworkShareRequestsResponse_httpStatus = Lens.lens (\ListAssessmentFrameworkShareRequestsResponse' {httpStatus} -> httpStatus) (\s@ListAssessmentFrameworkShareRequestsResponse' {} a -> s {httpStatus = a} :: ListAssessmentFrameworkShareRequestsResponse)

instance
  Prelude.NFData
    ListAssessmentFrameworkShareRequestsResponse
  where
  rnf ListAssessmentFrameworkShareRequestsResponse' {..} =
    Prelude.rnf assessmentFrameworkShareRequests
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
