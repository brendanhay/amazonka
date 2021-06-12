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
-- Module      : Network.AWS.Inspector.ListAssessmentTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the ARNs of the assessment targets within this AWS account. For
-- more information about assessment targets, see
-- <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_applications.html Amazon Inspector Assessment Targets>.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentTargets
  ( -- * Creating a Request
    ListAssessmentTargets (..),
    newListAssessmentTargets,

    -- * Request Lenses
    listAssessmentTargets_nextToken,
    listAssessmentTargets_maxResults,
    listAssessmentTargets_filter,

    -- * Destructuring the Response
    ListAssessmentTargetsResponse (..),
    newListAssessmentTargetsResponse,

    -- * Response Lenses
    listAssessmentTargetsResponse_nextToken,
    listAssessmentTargetsResponse_httpStatus,
    listAssessmentTargetsResponse_assessmentTargetArns,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAssessmentTargets' smart constructor.
data ListAssessmentTargets = ListAssessmentTargets'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the
    -- __ListAssessmentTargets__ action. Subsequent calls to the action fill
    -- __nextToken__ in the request with the value of __NextToken__ from the
    -- previous response to continue listing data.
    nextToken :: Core.Maybe Core.Text,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Core.Maybe Core.Int,
    -- | You can use this parameter to specify a subset of data to be included in
    -- the action\'s response.
    --
    -- For a record to match a filter, all specified filter attributes must
    -- match. When multiple values are specified for a filter attribute, any of
    -- the values can match.
    filter' :: Core.Maybe AssessmentTargetFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssessmentTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentTargets_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentTargets__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
--
-- 'maxResults', 'listAssessmentTargets_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
--
-- 'filter'', 'listAssessmentTargets_filter' - You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
newListAssessmentTargets ::
  ListAssessmentTargets
newListAssessmentTargets =
  ListAssessmentTargets'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentTargets__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
listAssessmentTargets_nextToken :: Lens.Lens' ListAssessmentTargets (Core.Maybe Core.Text)
listAssessmentTargets_nextToken = Lens.lens (\ListAssessmentTargets' {nextToken} -> nextToken) (\s@ListAssessmentTargets' {} a -> s {nextToken = a} :: ListAssessmentTargets)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
listAssessmentTargets_maxResults :: Lens.Lens' ListAssessmentTargets (Core.Maybe Core.Int)
listAssessmentTargets_maxResults = Lens.lens (\ListAssessmentTargets' {maxResults} -> maxResults) (\s@ListAssessmentTargets' {} a -> s {maxResults = a} :: ListAssessmentTargets)

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
listAssessmentTargets_filter :: Lens.Lens' ListAssessmentTargets (Core.Maybe AssessmentTargetFilter)
listAssessmentTargets_filter = Lens.lens (\ListAssessmentTargets' {filter'} -> filter') (\s@ListAssessmentTargets' {} a -> s {filter' = a} :: ListAssessmentTargets)

instance Core.AWSPager ListAssessmentTargets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssessmentTargetsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listAssessmentTargetsResponse_assessmentTargetArns
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAssessmentTargets_nextToken
          Lens..~ rs
          Lens.^? listAssessmentTargetsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListAssessmentTargets where
  type
    AWSResponse ListAssessmentTargets =
      ListAssessmentTargetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentTargetsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "assessmentTargetArns"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListAssessmentTargets

instance Core.NFData ListAssessmentTargets

instance Core.ToHeaders ListAssessmentTargets where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.ListAssessmentTargets" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAssessmentTargets where
  toJSON ListAssessmentTargets' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("filter" Core..=) Core.<$> filter'
          ]
      )

instance Core.ToPath ListAssessmentTargets where
  toPath = Core.const "/"

instance Core.ToQuery ListAssessmentTargets where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAssessmentTargetsResponse' smart constructor.
data ListAssessmentTargetsResponse = ListAssessmentTargetsResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameter is present in the response and contains the value to use for
    -- the __nextToken__ parameter in a subsequent pagination request. If there
    -- is no more data to be listed, this parameter is set to null.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of ARNs that specifies the assessment targets that are returned
    -- by the action.
    assessmentTargetArns :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssessmentTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentTargetsResponse_nextToken' - When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
--
-- 'httpStatus', 'listAssessmentTargetsResponse_httpStatus' - The response's http status code.
--
-- 'assessmentTargetArns', 'listAssessmentTargetsResponse_assessmentTargetArns' - A list of ARNs that specifies the assessment targets that are returned
-- by the action.
newListAssessmentTargetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAssessmentTargetsResponse
newListAssessmentTargetsResponse pHttpStatus_ =
  ListAssessmentTargetsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      assessmentTargetArns = Core.mempty
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
listAssessmentTargetsResponse_nextToken :: Lens.Lens' ListAssessmentTargetsResponse (Core.Maybe Core.Text)
listAssessmentTargetsResponse_nextToken = Lens.lens (\ListAssessmentTargetsResponse' {nextToken} -> nextToken) (\s@ListAssessmentTargetsResponse' {} a -> s {nextToken = a} :: ListAssessmentTargetsResponse)

-- | The response's http status code.
listAssessmentTargetsResponse_httpStatus :: Lens.Lens' ListAssessmentTargetsResponse Core.Int
listAssessmentTargetsResponse_httpStatus = Lens.lens (\ListAssessmentTargetsResponse' {httpStatus} -> httpStatus) (\s@ListAssessmentTargetsResponse' {} a -> s {httpStatus = a} :: ListAssessmentTargetsResponse)

-- | A list of ARNs that specifies the assessment targets that are returned
-- by the action.
listAssessmentTargetsResponse_assessmentTargetArns :: Lens.Lens' ListAssessmentTargetsResponse [Core.Text]
listAssessmentTargetsResponse_assessmentTargetArns = Lens.lens (\ListAssessmentTargetsResponse' {assessmentTargetArns} -> assessmentTargetArns) (\s@ListAssessmentTargetsResponse' {} a -> s {assessmentTargetArns = a} :: ListAssessmentTargetsResponse) Core.. Lens._Coerce

instance Core.NFData ListAssessmentTargetsResponse
