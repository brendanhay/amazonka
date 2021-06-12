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
-- Module      : Network.AWS.Inspector.ListAssessmentRuns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the assessment runs that correspond to the assessment templates
-- that are specified by the ARNs of the assessment templates.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentRuns
  ( -- * Creating a Request
    ListAssessmentRuns (..),
    newListAssessmentRuns,

    -- * Request Lenses
    listAssessmentRuns_nextToken,
    listAssessmentRuns_maxResults,
    listAssessmentRuns_filter,
    listAssessmentRuns_assessmentTemplateArns,

    -- * Destructuring the Response
    ListAssessmentRunsResponse (..),
    newListAssessmentRunsResponse,

    -- * Response Lenses
    listAssessmentRunsResponse_nextToken,
    listAssessmentRunsResponse_httpStatus,
    listAssessmentRunsResponse_assessmentRunArns,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAssessmentRuns' smart constructor.
data ListAssessmentRuns = ListAssessmentRuns'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the __ListAssessmentRuns__
    -- action. Subsequent calls to the action fill __nextToken__ in the request
    -- with the value of __NextToken__ from the previous response to continue
    -- listing data.
    nextToken :: Core.Maybe Core.Text,
    -- | You can use this parameter to indicate the maximum number of items that
    -- you want in the response. The default value is 10. The maximum value is
    -- 500.
    maxResults :: Core.Maybe Core.Int,
    -- | You can use this parameter to specify a subset of data to be included in
    -- the action\'s response.
    --
    -- For a record to match a filter, all specified filter attributes must
    -- match. When multiple values are specified for a filter attribute, any of
    -- the values can match.
    filter' :: Core.Maybe AssessmentRunFilter,
    -- | The ARNs that specify the assessment templates whose assessment runs you
    -- want to list.
    assessmentTemplateArns :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssessmentRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentRuns_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the __ListAssessmentRuns__
-- action. Subsequent calls to the action fill __nextToken__ in the request
-- with the value of __NextToken__ from the previous response to continue
-- listing data.
--
-- 'maxResults', 'listAssessmentRuns_maxResults' - You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 10. The maximum value is
-- 500.
--
-- 'filter'', 'listAssessmentRuns_filter' - You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
--
-- 'assessmentTemplateArns', 'listAssessmentRuns_assessmentTemplateArns' - The ARNs that specify the assessment templates whose assessment runs you
-- want to list.
newListAssessmentRuns ::
  ListAssessmentRuns
newListAssessmentRuns =
  ListAssessmentRuns'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing,
      assessmentTemplateArns = Core.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the __ListAssessmentRuns__
-- action. Subsequent calls to the action fill __nextToken__ in the request
-- with the value of __NextToken__ from the previous response to continue
-- listing data.
listAssessmentRuns_nextToken :: Lens.Lens' ListAssessmentRuns (Core.Maybe Core.Text)
listAssessmentRuns_nextToken = Lens.lens (\ListAssessmentRuns' {nextToken} -> nextToken) (\s@ListAssessmentRuns' {} a -> s {nextToken = a} :: ListAssessmentRuns)

-- | You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 10. The maximum value is
-- 500.
listAssessmentRuns_maxResults :: Lens.Lens' ListAssessmentRuns (Core.Maybe Core.Int)
listAssessmentRuns_maxResults = Lens.lens (\ListAssessmentRuns' {maxResults} -> maxResults) (\s@ListAssessmentRuns' {} a -> s {maxResults = a} :: ListAssessmentRuns)

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
listAssessmentRuns_filter :: Lens.Lens' ListAssessmentRuns (Core.Maybe AssessmentRunFilter)
listAssessmentRuns_filter = Lens.lens (\ListAssessmentRuns' {filter'} -> filter') (\s@ListAssessmentRuns' {} a -> s {filter' = a} :: ListAssessmentRuns)

-- | The ARNs that specify the assessment templates whose assessment runs you
-- want to list.
listAssessmentRuns_assessmentTemplateArns :: Lens.Lens' ListAssessmentRuns (Core.Maybe [Core.Text])
listAssessmentRuns_assessmentTemplateArns = Lens.lens (\ListAssessmentRuns' {assessmentTemplateArns} -> assessmentTemplateArns) (\s@ListAssessmentRuns' {} a -> s {assessmentTemplateArns = a} :: ListAssessmentRuns) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListAssessmentRuns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssessmentRunsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listAssessmentRunsResponse_assessmentRunArns
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAssessmentRuns_nextToken
          Lens..~ rs
          Lens.^? listAssessmentRunsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListAssessmentRuns where
  type
    AWSResponse ListAssessmentRuns =
      ListAssessmentRunsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentRunsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "assessmentRunArns"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListAssessmentRuns

instance Core.NFData ListAssessmentRuns

instance Core.ToHeaders ListAssessmentRuns where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.ListAssessmentRuns" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAssessmentRuns where
  toJSON ListAssessmentRuns' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("filter" Core..=) Core.<$> filter',
            ("assessmentTemplateArns" Core..=)
              Core.<$> assessmentTemplateArns
          ]
      )

instance Core.ToPath ListAssessmentRuns where
  toPath = Core.const "/"

instance Core.ToQuery ListAssessmentRuns where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAssessmentRunsResponse' smart constructor.
data ListAssessmentRunsResponse = ListAssessmentRunsResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameter is present in the response and contains the value to use for
    -- the __nextToken__ parameter in a subsequent pagination request. If there
    -- is no more data to be listed, this parameter is set to null.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of ARNs that specifies the assessment runs that are returned by
    -- the action.
    assessmentRunArns :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssessmentRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentRunsResponse_nextToken' - When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
--
-- 'httpStatus', 'listAssessmentRunsResponse_httpStatus' - The response's http status code.
--
-- 'assessmentRunArns', 'listAssessmentRunsResponse_assessmentRunArns' - A list of ARNs that specifies the assessment runs that are returned by
-- the action.
newListAssessmentRunsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAssessmentRunsResponse
newListAssessmentRunsResponse pHttpStatus_ =
  ListAssessmentRunsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      assessmentRunArns = Core.mempty
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
listAssessmentRunsResponse_nextToken :: Lens.Lens' ListAssessmentRunsResponse (Core.Maybe Core.Text)
listAssessmentRunsResponse_nextToken = Lens.lens (\ListAssessmentRunsResponse' {nextToken} -> nextToken) (\s@ListAssessmentRunsResponse' {} a -> s {nextToken = a} :: ListAssessmentRunsResponse)

-- | The response's http status code.
listAssessmentRunsResponse_httpStatus :: Lens.Lens' ListAssessmentRunsResponse Core.Int
listAssessmentRunsResponse_httpStatus = Lens.lens (\ListAssessmentRunsResponse' {httpStatus} -> httpStatus) (\s@ListAssessmentRunsResponse' {} a -> s {httpStatus = a} :: ListAssessmentRunsResponse)

-- | A list of ARNs that specifies the assessment runs that are returned by
-- the action.
listAssessmentRunsResponse_assessmentRunArns :: Lens.Lens' ListAssessmentRunsResponse [Core.Text]
listAssessmentRunsResponse_assessmentRunArns = Lens.lens (\ListAssessmentRunsResponse' {assessmentRunArns} -> assessmentRunArns) (\s@ListAssessmentRunsResponse' {} a -> s {assessmentRunArns = a} :: ListAssessmentRunsResponse) Core.. Lens._Coerce

instance Core.NFData ListAssessmentRunsResponse
