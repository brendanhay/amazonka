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
-- Module      : Network.AWS.SWF.ListOpenWorkflowExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of open workflow executions in the specified domain that
-- meet the filtering criteria. The results may be split into multiple
-- pages. To retrieve subsequent pages, make the call again using the
-- nextPageToken returned by the initial call.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--
--     -   @tagFilter.tag@: String constraint. The key is
--         @swf:tagFilter.tag@.
--
--     -   @typeFilter.name@: String constraint. The key is
--         @swf:typeFilter.name@.
--
--     -   @typeFilter.version@: String constraint. The key is
--         @swf:typeFilter.version@.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
--
-- This operation returns paginated results.
module Network.AWS.SWF.ListOpenWorkflowExecutions
  ( -- * Creating a Request
    ListOpenWorkflowExecutions (..),
    newListOpenWorkflowExecutions,

    -- * Request Lenses
    listOpenWorkflowExecutions_typeFilter,
    listOpenWorkflowExecutions_tagFilter,
    listOpenWorkflowExecutions_nextPageToken,
    listOpenWorkflowExecutions_maximumPageSize,
    listOpenWorkflowExecutions_reverseOrder,
    listOpenWorkflowExecutions_executionFilter,
    listOpenWorkflowExecutions_domain,
    listOpenWorkflowExecutions_startTimeFilter,

    -- * Destructuring the Response
    WorkflowExecutionInfos (..),
    newWorkflowExecutionInfos,

    -- * Response Lenses
    workflowExecutionInfos_nextPageToken,
    workflowExecutionInfos_executionInfos,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newListOpenWorkflowExecutions' smart constructor.
data ListOpenWorkflowExecutions = ListOpenWorkflowExecutions'
  { -- | If specified, only executions of the type specified in the filter are
    -- returned.
    --
    -- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
    -- You can specify at most one of these in a request.
    typeFilter :: Core.Maybe WorkflowTypeFilter,
    -- | If specified, only executions that have the matching tag are listed.
    --
    -- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
    -- You can specify at most one of these in a request.
    tagFilter :: Core.Maybe TagFilter,
    -- | If @NextPageToken@ is returned there are more results available. The
    -- value of @NextPageToken@ is a unique pagination token for each page.
    -- Make the call again using the returned token to retrieve the next page.
    -- Keep all other arguments unchanged. Each pagination token expires after
    -- 60 seconds. Using an expired pagination token will return a @400@ error:
    -- \"@Specified token has exceeded its maximum lifetime@\".
    --
    -- The configured @maximumPageSize@ determines how many results can be
    -- returned in a single call.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The maximum number of results that are returned per call. Use
    -- @nextPageToken@ to obtain further pages of results.
    maximumPageSize :: Core.Maybe Core.Natural,
    -- | When set to @true@, returns the results in reverse order. By default the
    -- results are returned in descending order of the start time of the
    -- executions.
    reverseOrder :: Core.Maybe Core.Bool,
    -- | If specified, only workflow executions matching the workflow ID
    -- specified in the filter are returned.
    --
    -- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
    -- You can specify at most one of these in a request.
    executionFilter :: Core.Maybe WorkflowExecutionFilter,
    -- | The name of the domain that contains the workflow executions to list.
    domain :: Core.Text,
    -- | Workflow executions are included in the returned results based on
    -- whether their start times are within the range specified by this filter.
    startTimeFilter :: ExecutionTimeFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOpenWorkflowExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeFilter', 'listOpenWorkflowExecutions_typeFilter' - If specified, only executions of the type specified in the filter are
-- returned.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
--
-- 'tagFilter', 'listOpenWorkflowExecutions_tagFilter' - If specified, only executions that have the matching tag are listed.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
--
-- 'nextPageToken', 'listOpenWorkflowExecutions_nextPageToken' - If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- 'maximumPageSize', 'listOpenWorkflowExecutions_maximumPageSize' - The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
--
-- 'reverseOrder', 'listOpenWorkflowExecutions_reverseOrder' - When set to @true@, returns the results in reverse order. By default the
-- results are returned in descending order of the start time of the
-- executions.
--
-- 'executionFilter', 'listOpenWorkflowExecutions_executionFilter' - If specified, only workflow executions matching the workflow ID
-- specified in the filter are returned.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
--
-- 'domain', 'listOpenWorkflowExecutions_domain' - The name of the domain that contains the workflow executions to list.
--
-- 'startTimeFilter', 'listOpenWorkflowExecutions_startTimeFilter' - Workflow executions are included in the returned results based on
-- whether their start times are within the range specified by this filter.
newListOpenWorkflowExecutions ::
  -- | 'domain'
  Core.Text ->
  -- | 'startTimeFilter'
  ExecutionTimeFilter ->
  ListOpenWorkflowExecutions
newListOpenWorkflowExecutions
  pDomain_
  pStartTimeFilter_ =
    ListOpenWorkflowExecutions'
      { typeFilter =
          Core.Nothing,
        tagFilter = Core.Nothing,
        nextPageToken = Core.Nothing,
        maximumPageSize = Core.Nothing,
        reverseOrder = Core.Nothing,
        executionFilter = Core.Nothing,
        domain = pDomain_,
        startTimeFilter = pStartTimeFilter_
      }

-- | If specified, only executions of the type specified in the filter are
-- returned.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
listOpenWorkflowExecutions_typeFilter :: Lens.Lens' ListOpenWorkflowExecutions (Core.Maybe WorkflowTypeFilter)
listOpenWorkflowExecutions_typeFilter = Lens.lens (\ListOpenWorkflowExecutions' {typeFilter} -> typeFilter) (\s@ListOpenWorkflowExecutions' {} a -> s {typeFilter = a} :: ListOpenWorkflowExecutions)

-- | If specified, only executions that have the matching tag are listed.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
listOpenWorkflowExecutions_tagFilter :: Lens.Lens' ListOpenWorkflowExecutions (Core.Maybe TagFilter)
listOpenWorkflowExecutions_tagFilter = Lens.lens (\ListOpenWorkflowExecutions' {tagFilter} -> tagFilter) (\s@ListOpenWorkflowExecutions' {} a -> s {tagFilter = a} :: ListOpenWorkflowExecutions)

-- | If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
listOpenWorkflowExecutions_nextPageToken :: Lens.Lens' ListOpenWorkflowExecutions (Core.Maybe Core.Text)
listOpenWorkflowExecutions_nextPageToken = Lens.lens (\ListOpenWorkflowExecutions' {nextPageToken} -> nextPageToken) (\s@ListOpenWorkflowExecutions' {} a -> s {nextPageToken = a} :: ListOpenWorkflowExecutions)

-- | The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
listOpenWorkflowExecutions_maximumPageSize :: Lens.Lens' ListOpenWorkflowExecutions (Core.Maybe Core.Natural)
listOpenWorkflowExecutions_maximumPageSize = Lens.lens (\ListOpenWorkflowExecutions' {maximumPageSize} -> maximumPageSize) (\s@ListOpenWorkflowExecutions' {} a -> s {maximumPageSize = a} :: ListOpenWorkflowExecutions)

-- | When set to @true@, returns the results in reverse order. By default the
-- results are returned in descending order of the start time of the
-- executions.
listOpenWorkflowExecutions_reverseOrder :: Lens.Lens' ListOpenWorkflowExecutions (Core.Maybe Core.Bool)
listOpenWorkflowExecutions_reverseOrder = Lens.lens (\ListOpenWorkflowExecutions' {reverseOrder} -> reverseOrder) (\s@ListOpenWorkflowExecutions' {} a -> s {reverseOrder = a} :: ListOpenWorkflowExecutions)

-- | If specified, only workflow executions matching the workflow ID
-- specified in the filter are returned.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
listOpenWorkflowExecutions_executionFilter :: Lens.Lens' ListOpenWorkflowExecutions (Core.Maybe WorkflowExecutionFilter)
listOpenWorkflowExecutions_executionFilter = Lens.lens (\ListOpenWorkflowExecutions' {executionFilter} -> executionFilter) (\s@ListOpenWorkflowExecutions' {} a -> s {executionFilter = a} :: ListOpenWorkflowExecutions)

-- | The name of the domain that contains the workflow executions to list.
listOpenWorkflowExecutions_domain :: Lens.Lens' ListOpenWorkflowExecutions Core.Text
listOpenWorkflowExecutions_domain = Lens.lens (\ListOpenWorkflowExecutions' {domain} -> domain) (\s@ListOpenWorkflowExecutions' {} a -> s {domain = a} :: ListOpenWorkflowExecutions)

-- | Workflow executions are included in the returned results based on
-- whether their start times are within the range specified by this filter.
listOpenWorkflowExecutions_startTimeFilter :: Lens.Lens' ListOpenWorkflowExecutions ExecutionTimeFilter
listOpenWorkflowExecutions_startTimeFilter = Lens.lens (\ListOpenWorkflowExecutions' {startTimeFilter} -> startTimeFilter) (\s@ListOpenWorkflowExecutions' {} a -> s {startTimeFilter = a} :: ListOpenWorkflowExecutions)

instance Core.AWSPager ListOpenWorkflowExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? workflowExecutionInfos_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. workflowExecutionInfos_executionInfos) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOpenWorkflowExecutions_nextPageToken
          Lens..~ rs
          Lens.^? workflowExecutionInfos_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest ListOpenWorkflowExecutions where
  type
    AWSResponse ListOpenWorkflowExecutions =
      WorkflowExecutionInfos
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable ListOpenWorkflowExecutions

instance Core.NFData ListOpenWorkflowExecutions

instance Core.ToHeaders ListOpenWorkflowExecutions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.ListOpenWorkflowExecutions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListOpenWorkflowExecutions where
  toJSON ListOpenWorkflowExecutions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("typeFilter" Core..=) Core.<$> typeFilter,
            ("tagFilter" Core..=) Core.<$> tagFilter,
            ("nextPageToken" Core..=) Core.<$> nextPageToken,
            ("maximumPageSize" Core..=) Core.<$> maximumPageSize,
            ("reverseOrder" Core..=) Core.<$> reverseOrder,
            ("executionFilter" Core..=) Core.<$> executionFilter,
            Core.Just ("domain" Core..= domain),
            Core.Just
              ("startTimeFilter" Core..= startTimeFilter)
          ]
      )

instance Core.ToPath ListOpenWorkflowExecutions where
  toPath = Core.const "/"

instance Core.ToQuery ListOpenWorkflowExecutions where
  toQuery = Core.const Core.mempty
