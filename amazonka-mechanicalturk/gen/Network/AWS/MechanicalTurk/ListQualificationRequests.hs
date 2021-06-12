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
-- Module      : Network.AWS.MechanicalTurk.ListQualificationRequests
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListQualificationRequests@ operation retrieves requests for
-- Qualifications of a particular Qualification type. The owner of the
-- Qualification type calls this operation to poll for pending requests,
-- and accepts them using the AcceptQualification operation.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListQualificationRequests
  ( -- * Creating a Request
    ListQualificationRequests (..),
    newListQualificationRequests,

    -- * Request Lenses
    listQualificationRequests_qualificationTypeId,
    listQualificationRequests_nextToken,
    listQualificationRequests_maxResults,

    -- * Destructuring the Response
    ListQualificationRequestsResponse (..),
    newListQualificationRequestsResponse,

    -- * Response Lenses
    listQualificationRequestsResponse_nextToken,
    listQualificationRequestsResponse_numResults,
    listQualificationRequestsResponse_qualificationRequests,
    listQualificationRequestsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListQualificationRequests' smart constructor.
data ListQualificationRequests = ListQualificationRequests'
  { -- | The ID of the QualificationType.
    qualificationTypeId :: Core.Maybe Core.Text,
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListQualificationRequests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualificationTypeId', 'listQualificationRequests_qualificationTypeId' - The ID of the QualificationType.
--
-- 'nextToken', 'listQualificationRequests_nextToken' - Undocumented member.
--
-- 'maxResults', 'listQualificationRequests_maxResults' - The maximum number of results to return in a single call.
newListQualificationRequests ::
  ListQualificationRequests
newListQualificationRequests =
  ListQualificationRequests'
    { qualificationTypeId =
        Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The ID of the QualificationType.
listQualificationRequests_qualificationTypeId :: Lens.Lens' ListQualificationRequests (Core.Maybe Core.Text)
listQualificationRequests_qualificationTypeId = Lens.lens (\ListQualificationRequests' {qualificationTypeId} -> qualificationTypeId) (\s@ListQualificationRequests' {} a -> s {qualificationTypeId = a} :: ListQualificationRequests)

-- | Undocumented member.
listQualificationRequests_nextToken :: Lens.Lens' ListQualificationRequests (Core.Maybe Core.Text)
listQualificationRequests_nextToken = Lens.lens (\ListQualificationRequests' {nextToken} -> nextToken) (\s@ListQualificationRequests' {} a -> s {nextToken = a} :: ListQualificationRequests)

-- | The maximum number of results to return in a single call.
listQualificationRequests_maxResults :: Lens.Lens' ListQualificationRequests (Core.Maybe Core.Natural)
listQualificationRequests_maxResults = Lens.lens (\ListQualificationRequests' {maxResults} -> maxResults) (\s@ListQualificationRequests' {} a -> s {maxResults = a} :: ListQualificationRequests)

instance Core.AWSPager ListQualificationRequests where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listQualificationRequestsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listQualificationRequestsResponse_qualificationRequests
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listQualificationRequests_nextToken
          Lens..~ rs
          Lens.^? listQualificationRequestsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListQualificationRequests where
  type
    AWSResponse ListQualificationRequests =
      ListQualificationRequestsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListQualificationRequestsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "NumResults")
            Core.<*> ( x Core..?> "QualificationRequests"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListQualificationRequests

instance Core.NFData ListQualificationRequests

instance Core.ToHeaders ListQualificationRequests where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.ListQualificationRequests" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListQualificationRequests where
  toJSON ListQualificationRequests' {..} =
    Core.object
      ( Core.catMaybes
          [ ("QualificationTypeId" Core..=)
              Core.<$> qualificationTypeId,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListQualificationRequests where
  toPath = Core.const "/"

instance Core.ToQuery ListQualificationRequests where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListQualificationRequestsResponse' smart constructor.
data ListQualificationRequestsResponse = ListQualificationRequestsResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The number of Qualification requests on this page in the filtered
    -- results list, equivalent to the number of Qualification requests being
    -- returned by this call.
    numResults :: Core.Maybe Core.Int,
    -- | The Qualification request. The response includes one
    -- QualificationRequest element for each Qualification request returned by
    -- the query.
    qualificationRequests :: Core.Maybe [QualificationRequest],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListQualificationRequestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQualificationRequestsResponse_nextToken' - Undocumented member.
--
-- 'numResults', 'listQualificationRequestsResponse_numResults' - The number of Qualification requests on this page in the filtered
-- results list, equivalent to the number of Qualification requests being
-- returned by this call.
--
-- 'qualificationRequests', 'listQualificationRequestsResponse_qualificationRequests' - The Qualification request. The response includes one
-- QualificationRequest element for each Qualification request returned by
-- the query.
--
-- 'httpStatus', 'listQualificationRequestsResponse_httpStatus' - The response's http status code.
newListQualificationRequestsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListQualificationRequestsResponse
newListQualificationRequestsResponse pHttpStatus_ =
  ListQualificationRequestsResponse'
    { nextToken =
        Core.Nothing,
      numResults = Core.Nothing,
      qualificationRequests = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listQualificationRequestsResponse_nextToken :: Lens.Lens' ListQualificationRequestsResponse (Core.Maybe Core.Text)
listQualificationRequestsResponse_nextToken = Lens.lens (\ListQualificationRequestsResponse' {nextToken} -> nextToken) (\s@ListQualificationRequestsResponse' {} a -> s {nextToken = a} :: ListQualificationRequestsResponse)

-- | The number of Qualification requests on this page in the filtered
-- results list, equivalent to the number of Qualification requests being
-- returned by this call.
listQualificationRequestsResponse_numResults :: Lens.Lens' ListQualificationRequestsResponse (Core.Maybe Core.Int)
listQualificationRequestsResponse_numResults = Lens.lens (\ListQualificationRequestsResponse' {numResults} -> numResults) (\s@ListQualificationRequestsResponse' {} a -> s {numResults = a} :: ListQualificationRequestsResponse)

-- | The Qualification request. The response includes one
-- QualificationRequest element for each Qualification request returned by
-- the query.
listQualificationRequestsResponse_qualificationRequests :: Lens.Lens' ListQualificationRequestsResponse (Core.Maybe [QualificationRequest])
listQualificationRequestsResponse_qualificationRequests = Lens.lens (\ListQualificationRequestsResponse' {qualificationRequests} -> qualificationRequests) (\s@ListQualificationRequestsResponse' {} a -> s {qualificationRequests = a} :: ListQualificationRequestsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listQualificationRequestsResponse_httpStatus :: Lens.Lens' ListQualificationRequestsResponse Core.Int
listQualificationRequestsResponse_httpStatus = Lens.lens (\ListQualificationRequestsResponse' {httpStatus} -> httpStatus) (\s@ListQualificationRequestsResponse' {} a -> s {httpStatus = a} :: ListQualificationRequestsResponse)

instance
  Core.NFData
    ListQualificationRequestsResponse
