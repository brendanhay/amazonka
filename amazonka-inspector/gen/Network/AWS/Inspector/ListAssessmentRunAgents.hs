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
-- Module      : Network.AWS.Inspector.ListAssessmentRunAgents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the agents of the assessment runs that are specified by the ARNs
-- of the assessment runs.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentRunAgents
  ( -- * Creating a Request
    ListAssessmentRunAgents (..),
    newListAssessmentRunAgents,

    -- * Request Lenses
    listAssessmentRunAgents_nextToken,
    listAssessmentRunAgents_maxResults,
    listAssessmentRunAgents_filter,
    listAssessmentRunAgents_assessmentRunArn,

    -- * Destructuring the Response
    ListAssessmentRunAgentsResponse (..),
    newListAssessmentRunAgentsResponse,

    -- * Response Lenses
    listAssessmentRunAgentsResponse_nextToken,
    listAssessmentRunAgentsResponse_httpStatus,
    listAssessmentRunAgentsResponse_assessmentRunAgents,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAssessmentRunAgents' smart constructor.
data ListAssessmentRunAgents = ListAssessmentRunAgents'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the
    -- __ListAssessmentRunAgents__ action. Subsequent calls to the action fill
    -- __nextToken__ in the request with the value of __NextToken__ from the
    -- previous response to continue listing data.
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
    filter' :: Core.Maybe AgentFilter,
    -- | The ARN that specifies the assessment run whose agents you want to list.
    assessmentRunArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssessmentRunAgents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentRunAgents_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentRunAgents__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
--
-- 'maxResults', 'listAssessmentRunAgents_maxResults' - You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 10. The maximum value is
-- 500.
--
-- 'filter'', 'listAssessmentRunAgents_filter' - You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
--
-- 'assessmentRunArn', 'listAssessmentRunAgents_assessmentRunArn' - The ARN that specifies the assessment run whose agents you want to list.
newListAssessmentRunAgents ::
  -- | 'assessmentRunArn'
  Core.Text ->
  ListAssessmentRunAgents
newListAssessmentRunAgents pAssessmentRunArn_ =
  ListAssessmentRunAgents'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing,
      assessmentRunArn = pAssessmentRunArn_
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentRunAgents__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
listAssessmentRunAgents_nextToken :: Lens.Lens' ListAssessmentRunAgents (Core.Maybe Core.Text)
listAssessmentRunAgents_nextToken = Lens.lens (\ListAssessmentRunAgents' {nextToken} -> nextToken) (\s@ListAssessmentRunAgents' {} a -> s {nextToken = a} :: ListAssessmentRunAgents)

-- | You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 10. The maximum value is
-- 500.
listAssessmentRunAgents_maxResults :: Lens.Lens' ListAssessmentRunAgents (Core.Maybe Core.Int)
listAssessmentRunAgents_maxResults = Lens.lens (\ListAssessmentRunAgents' {maxResults} -> maxResults) (\s@ListAssessmentRunAgents' {} a -> s {maxResults = a} :: ListAssessmentRunAgents)

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
listAssessmentRunAgents_filter :: Lens.Lens' ListAssessmentRunAgents (Core.Maybe AgentFilter)
listAssessmentRunAgents_filter = Lens.lens (\ListAssessmentRunAgents' {filter'} -> filter') (\s@ListAssessmentRunAgents' {} a -> s {filter' = a} :: ListAssessmentRunAgents)

-- | The ARN that specifies the assessment run whose agents you want to list.
listAssessmentRunAgents_assessmentRunArn :: Lens.Lens' ListAssessmentRunAgents Core.Text
listAssessmentRunAgents_assessmentRunArn = Lens.lens (\ListAssessmentRunAgents' {assessmentRunArn} -> assessmentRunArn) (\s@ListAssessmentRunAgents' {} a -> s {assessmentRunArn = a} :: ListAssessmentRunAgents)

instance Core.AWSPager ListAssessmentRunAgents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssessmentRunAgentsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listAssessmentRunAgentsResponse_assessmentRunAgents
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAssessmentRunAgents_nextToken
          Lens..~ rs
          Lens.^? listAssessmentRunAgentsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListAssessmentRunAgents where
  type
    AWSResponse ListAssessmentRunAgents =
      ListAssessmentRunAgentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentRunAgentsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "assessmentRunAgents"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListAssessmentRunAgents

instance Core.NFData ListAssessmentRunAgents

instance Core.ToHeaders ListAssessmentRunAgents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.ListAssessmentRunAgents" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAssessmentRunAgents where
  toJSON ListAssessmentRunAgents' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("filter" Core..=) Core.<$> filter',
            Core.Just
              ("assessmentRunArn" Core..= assessmentRunArn)
          ]
      )

instance Core.ToPath ListAssessmentRunAgents where
  toPath = Core.const "/"

instance Core.ToQuery ListAssessmentRunAgents where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAssessmentRunAgentsResponse' smart constructor.
data ListAssessmentRunAgentsResponse = ListAssessmentRunAgentsResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameter is present in the response and contains the value to use for
    -- the __nextToken__ parameter in a subsequent pagination request. If there
    -- is no more data to be listed, this parameter is set to null.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of ARNs that specifies the agents returned by the action.
    assessmentRunAgents :: [AssessmentRunAgent]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssessmentRunAgentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentRunAgentsResponse_nextToken' - When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
--
-- 'httpStatus', 'listAssessmentRunAgentsResponse_httpStatus' - The response's http status code.
--
-- 'assessmentRunAgents', 'listAssessmentRunAgentsResponse_assessmentRunAgents' - A list of ARNs that specifies the agents returned by the action.
newListAssessmentRunAgentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAssessmentRunAgentsResponse
newListAssessmentRunAgentsResponse pHttpStatus_ =
  ListAssessmentRunAgentsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      assessmentRunAgents = Core.mempty
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
listAssessmentRunAgentsResponse_nextToken :: Lens.Lens' ListAssessmentRunAgentsResponse (Core.Maybe Core.Text)
listAssessmentRunAgentsResponse_nextToken = Lens.lens (\ListAssessmentRunAgentsResponse' {nextToken} -> nextToken) (\s@ListAssessmentRunAgentsResponse' {} a -> s {nextToken = a} :: ListAssessmentRunAgentsResponse)

-- | The response's http status code.
listAssessmentRunAgentsResponse_httpStatus :: Lens.Lens' ListAssessmentRunAgentsResponse Core.Int
listAssessmentRunAgentsResponse_httpStatus = Lens.lens (\ListAssessmentRunAgentsResponse' {httpStatus} -> httpStatus) (\s@ListAssessmentRunAgentsResponse' {} a -> s {httpStatus = a} :: ListAssessmentRunAgentsResponse)

-- | A list of ARNs that specifies the agents returned by the action.
listAssessmentRunAgentsResponse_assessmentRunAgents :: Lens.Lens' ListAssessmentRunAgentsResponse [AssessmentRunAgent]
listAssessmentRunAgentsResponse_assessmentRunAgents = Lens.lens (\ListAssessmentRunAgentsResponse' {assessmentRunAgents} -> assessmentRunAgents) (\s@ListAssessmentRunAgentsResponse' {} a -> s {assessmentRunAgents = a} :: ListAssessmentRunAgentsResponse) Core.. Lens._Coerce

instance Core.NFData ListAssessmentRunAgentsResponse
