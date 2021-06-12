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
-- Module      : Network.AWS.Inspector.ListAssessmentTemplates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the assessment templates that correspond to the assessment targets
-- that are specified by the ARNs of the assessment targets.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentTemplates
  ( -- * Creating a Request
    ListAssessmentTemplates (..),
    newListAssessmentTemplates,

    -- * Request Lenses
    listAssessmentTemplates_nextToken,
    listAssessmentTemplates_maxResults,
    listAssessmentTemplates_assessmentTargetArns,
    listAssessmentTemplates_filter,

    -- * Destructuring the Response
    ListAssessmentTemplatesResponse (..),
    newListAssessmentTemplatesResponse,

    -- * Response Lenses
    listAssessmentTemplatesResponse_nextToken,
    listAssessmentTemplatesResponse_httpStatus,
    listAssessmentTemplatesResponse_assessmentTemplateArns,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAssessmentTemplates' smart constructor.
data ListAssessmentTemplates = ListAssessmentTemplates'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the
    -- __ListAssessmentTemplates__ action. Subsequent calls to the action fill
    -- __nextToken__ in the request with the value of __NextToken__ from the
    -- previous response to continue listing data.
    nextToken :: Core.Maybe Core.Text,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Core.Maybe Core.Int,
    -- | A list of ARNs that specifies the assessment targets whose assessment
    -- templates you want to list.
    assessmentTargetArns :: Core.Maybe [Core.Text],
    -- | You can use this parameter to specify a subset of data to be included in
    -- the action\'s response.
    --
    -- For a record to match a filter, all specified filter attributes must
    -- match. When multiple values are specified for a filter attribute, any of
    -- the values can match.
    filter' :: Core.Maybe AssessmentTemplateFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssessmentTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentTemplates_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentTemplates__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
--
-- 'maxResults', 'listAssessmentTemplates_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
--
-- 'assessmentTargetArns', 'listAssessmentTemplates_assessmentTargetArns' - A list of ARNs that specifies the assessment targets whose assessment
-- templates you want to list.
--
-- 'filter'', 'listAssessmentTemplates_filter' - You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
newListAssessmentTemplates ::
  ListAssessmentTemplates
newListAssessmentTemplates =
  ListAssessmentTemplates'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      assessmentTargetArns = Core.Nothing,
      filter' = Core.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentTemplates__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
listAssessmentTemplates_nextToken :: Lens.Lens' ListAssessmentTemplates (Core.Maybe Core.Text)
listAssessmentTemplates_nextToken = Lens.lens (\ListAssessmentTemplates' {nextToken} -> nextToken) (\s@ListAssessmentTemplates' {} a -> s {nextToken = a} :: ListAssessmentTemplates)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
listAssessmentTemplates_maxResults :: Lens.Lens' ListAssessmentTemplates (Core.Maybe Core.Int)
listAssessmentTemplates_maxResults = Lens.lens (\ListAssessmentTemplates' {maxResults} -> maxResults) (\s@ListAssessmentTemplates' {} a -> s {maxResults = a} :: ListAssessmentTemplates)

-- | A list of ARNs that specifies the assessment targets whose assessment
-- templates you want to list.
listAssessmentTemplates_assessmentTargetArns :: Lens.Lens' ListAssessmentTemplates (Core.Maybe [Core.Text])
listAssessmentTemplates_assessmentTargetArns = Lens.lens (\ListAssessmentTemplates' {assessmentTargetArns} -> assessmentTargetArns) (\s@ListAssessmentTemplates' {} a -> s {assessmentTargetArns = a} :: ListAssessmentTemplates) Core.. Lens.mapping Lens._Coerce

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
listAssessmentTemplates_filter :: Lens.Lens' ListAssessmentTemplates (Core.Maybe AssessmentTemplateFilter)
listAssessmentTemplates_filter = Lens.lens (\ListAssessmentTemplates' {filter'} -> filter') (\s@ListAssessmentTemplates' {} a -> s {filter' = a} :: ListAssessmentTemplates)

instance Core.AWSPager ListAssessmentTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssessmentTemplatesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listAssessmentTemplatesResponse_assessmentTemplateArns
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAssessmentTemplates_nextToken
          Lens..~ rs
          Lens.^? listAssessmentTemplatesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListAssessmentTemplates where
  type
    AWSResponse ListAssessmentTemplates =
      ListAssessmentTemplatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentTemplatesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "assessmentTemplateArns"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListAssessmentTemplates

instance Core.NFData ListAssessmentTemplates

instance Core.ToHeaders ListAssessmentTemplates where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.ListAssessmentTemplates" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAssessmentTemplates where
  toJSON ListAssessmentTemplates' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("assessmentTargetArns" Core..=)
              Core.<$> assessmentTargetArns,
            ("filter" Core..=) Core.<$> filter'
          ]
      )

instance Core.ToPath ListAssessmentTemplates where
  toPath = Core.const "/"

instance Core.ToQuery ListAssessmentTemplates where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAssessmentTemplatesResponse' smart constructor.
data ListAssessmentTemplatesResponse = ListAssessmentTemplatesResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameter is present in the response and contains the value to use for
    -- the __nextToken__ parameter in a subsequent pagination request. If there
    -- is no more data to be listed, this parameter is set to null.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of ARNs that specifies the assessment templates returned by the
    -- action.
    assessmentTemplateArns :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssessmentTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentTemplatesResponse_nextToken' - When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
--
-- 'httpStatus', 'listAssessmentTemplatesResponse_httpStatus' - The response's http status code.
--
-- 'assessmentTemplateArns', 'listAssessmentTemplatesResponse_assessmentTemplateArns' - A list of ARNs that specifies the assessment templates returned by the
-- action.
newListAssessmentTemplatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAssessmentTemplatesResponse
newListAssessmentTemplatesResponse pHttpStatus_ =
  ListAssessmentTemplatesResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      assessmentTemplateArns = Core.mempty
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
listAssessmentTemplatesResponse_nextToken :: Lens.Lens' ListAssessmentTemplatesResponse (Core.Maybe Core.Text)
listAssessmentTemplatesResponse_nextToken = Lens.lens (\ListAssessmentTemplatesResponse' {nextToken} -> nextToken) (\s@ListAssessmentTemplatesResponse' {} a -> s {nextToken = a} :: ListAssessmentTemplatesResponse)

-- | The response's http status code.
listAssessmentTemplatesResponse_httpStatus :: Lens.Lens' ListAssessmentTemplatesResponse Core.Int
listAssessmentTemplatesResponse_httpStatus = Lens.lens (\ListAssessmentTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListAssessmentTemplatesResponse' {} a -> s {httpStatus = a} :: ListAssessmentTemplatesResponse)

-- | A list of ARNs that specifies the assessment templates returned by the
-- action.
listAssessmentTemplatesResponse_assessmentTemplateArns :: Lens.Lens' ListAssessmentTemplatesResponse [Core.Text]
listAssessmentTemplatesResponse_assessmentTemplateArns = Lens.lens (\ListAssessmentTemplatesResponse' {assessmentTemplateArns} -> assessmentTemplateArns) (\s@ListAssessmentTemplatesResponse' {} a -> s {assessmentTemplateArns = a} :: ListAssessmentTemplatesResponse) Core.. Lens._Coerce

instance Core.NFData ListAssessmentTemplatesResponse
