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
-- Module      : Network.AWS.SWF.ListClosedWorkflowExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of closed workflow executions in the specified domain
-- that meet the filtering criteria. The results may be split into multiple
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
module Network.AWS.SWF.ListClosedWorkflowExecutions
  ( -- * Creating a Request
    ListClosedWorkflowExecutions (..),
    newListClosedWorkflowExecutions,

    -- * Request Lenses
    listClosedWorkflowExecutions_typeFilter,
    listClosedWorkflowExecutions_tagFilter,
    listClosedWorkflowExecutions_nextPageToken,
    listClosedWorkflowExecutions_maximumPageSize,
    listClosedWorkflowExecutions_reverseOrder,
    listClosedWorkflowExecutions_closeTimeFilter,
    listClosedWorkflowExecutions_startTimeFilter,
    listClosedWorkflowExecutions_executionFilter,
    listClosedWorkflowExecutions_closeStatusFilter,
    listClosedWorkflowExecutions_domain,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newListClosedWorkflowExecutions' smart constructor.
data ListClosedWorkflowExecutions = ListClosedWorkflowExecutions'
  { -- | If specified, only executions of the type specified in the filter are
    -- returned.
    --
    -- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
    -- mutually exclusive. You can specify at most one of these in a request.
    typeFilter :: Prelude.Maybe WorkflowTypeFilter,
    -- | If specified, only executions that have the matching tag are listed.
    --
    -- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
    -- mutually exclusive. You can specify at most one of these in a request.
    tagFilter :: Prelude.Maybe TagFilter,
    -- | If @NextPageToken@ is returned there are more results available. The
    -- value of @NextPageToken@ is a unique pagination token for each page.
    -- Make the call again using the returned token to retrieve the next page.
    -- Keep all other arguments unchanged. Each pagination token expires after
    -- 60 seconds. Using an expired pagination token will return a @400@ error:
    -- \"@Specified token has exceeded its maximum lifetime@\".
    --
    -- The configured @maximumPageSize@ determines how many results can be
    -- returned in a single call.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that are returned per call. Use
    -- @nextPageToken@ to obtain further pages of results.
    maximumPageSize :: Prelude.Maybe Prelude.Natural,
    -- | When set to @true@, returns the results in reverse order. By default the
    -- results are returned in descending order of the start or the close time
    -- of the executions.
    reverseOrder :: Prelude.Maybe Prelude.Bool,
    -- | If specified, the workflow executions are included in the returned
    -- results based on whether their close times are within the range
    -- specified by this filter. Also, if this parameter is specified, the
    -- returned results are ordered by their close times.
    --
    -- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
    -- specify one of these in a request but not both.
    closeTimeFilter :: Prelude.Maybe ExecutionTimeFilter,
    -- | If specified, the workflow executions are included in the returned
    -- results based on whether their start times are within the range
    -- specified by this filter. Also, if this parameter is specified, the
    -- returned results are ordered by their start times.
    --
    -- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
    -- specify one of these in a request but not both.
    startTimeFilter :: Prelude.Maybe ExecutionTimeFilter,
    -- | If specified, only workflow executions matching the workflow ID
    -- specified in the filter are returned.
    --
    -- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
    -- mutually exclusive. You can specify at most one of these in a request.
    executionFilter :: Prelude.Maybe WorkflowExecutionFilter,
    -- | If specified, only workflow executions that match this /close status/
    -- are listed. For example, if TERMINATED is specified, then only
    -- TERMINATED workflow executions are listed.
    --
    -- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
    -- mutually exclusive. You can specify at most one of these in a request.
    closeStatusFilter :: Prelude.Maybe CloseStatusFilter,
    -- | The name of the domain that contains the workflow executions to list.
    domain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClosedWorkflowExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeFilter', 'listClosedWorkflowExecutions_typeFilter' - If specified, only executions of the type specified in the filter are
-- returned.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
--
-- 'tagFilter', 'listClosedWorkflowExecutions_tagFilter' - If specified, only executions that have the matching tag are listed.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
--
-- 'nextPageToken', 'listClosedWorkflowExecutions_nextPageToken' - If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- 'maximumPageSize', 'listClosedWorkflowExecutions_maximumPageSize' - The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
--
-- 'reverseOrder', 'listClosedWorkflowExecutions_reverseOrder' - When set to @true@, returns the results in reverse order. By default the
-- results are returned in descending order of the start or the close time
-- of the executions.
--
-- 'closeTimeFilter', 'listClosedWorkflowExecutions_closeTimeFilter' - If specified, the workflow executions are included in the returned
-- results based on whether their close times are within the range
-- specified by this filter. Also, if this parameter is specified, the
-- returned results are ordered by their close times.
--
-- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
-- specify one of these in a request but not both.
--
-- 'startTimeFilter', 'listClosedWorkflowExecutions_startTimeFilter' - If specified, the workflow executions are included in the returned
-- results based on whether their start times are within the range
-- specified by this filter. Also, if this parameter is specified, the
-- returned results are ordered by their start times.
--
-- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
-- specify one of these in a request but not both.
--
-- 'executionFilter', 'listClosedWorkflowExecutions_executionFilter' - If specified, only workflow executions matching the workflow ID
-- specified in the filter are returned.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
--
-- 'closeStatusFilter', 'listClosedWorkflowExecutions_closeStatusFilter' - If specified, only workflow executions that match this /close status/
-- are listed. For example, if TERMINATED is specified, then only
-- TERMINATED workflow executions are listed.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
--
-- 'domain', 'listClosedWorkflowExecutions_domain' - The name of the domain that contains the workflow executions to list.
newListClosedWorkflowExecutions ::
  -- | 'domain'
  Prelude.Text ->
  ListClosedWorkflowExecutions
newListClosedWorkflowExecutions pDomain_ =
  ListClosedWorkflowExecutions'
    { typeFilter =
        Prelude.Nothing,
      tagFilter = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      maximumPageSize = Prelude.Nothing,
      reverseOrder = Prelude.Nothing,
      closeTimeFilter = Prelude.Nothing,
      startTimeFilter = Prelude.Nothing,
      executionFilter = Prelude.Nothing,
      closeStatusFilter = Prelude.Nothing,
      domain = pDomain_
    }

-- | If specified, only executions of the type specified in the filter are
-- returned.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
listClosedWorkflowExecutions_typeFilter :: Lens.Lens' ListClosedWorkflowExecutions (Prelude.Maybe WorkflowTypeFilter)
listClosedWorkflowExecutions_typeFilter = Lens.lens (\ListClosedWorkflowExecutions' {typeFilter} -> typeFilter) (\s@ListClosedWorkflowExecutions' {} a -> s {typeFilter = a} :: ListClosedWorkflowExecutions)

-- | If specified, only executions that have the matching tag are listed.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
listClosedWorkflowExecutions_tagFilter :: Lens.Lens' ListClosedWorkflowExecutions (Prelude.Maybe TagFilter)
listClosedWorkflowExecutions_tagFilter = Lens.lens (\ListClosedWorkflowExecutions' {tagFilter} -> tagFilter) (\s@ListClosedWorkflowExecutions' {} a -> s {tagFilter = a} :: ListClosedWorkflowExecutions)

-- | If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
listClosedWorkflowExecutions_nextPageToken :: Lens.Lens' ListClosedWorkflowExecutions (Prelude.Maybe Prelude.Text)
listClosedWorkflowExecutions_nextPageToken = Lens.lens (\ListClosedWorkflowExecutions' {nextPageToken} -> nextPageToken) (\s@ListClosedWorkflowExecutions' {} a -> s {nextPageToken = a} :: ListClosedWorkflowExecutions)

-- | The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
listClosedWorkflowExecutions_maximumPageSize :: Lens.Lens' ListClosedWorkflowExecutions (Prelude.Maybe Prelude.Natural)
listClosedWorkflowExecutions_maximumPageSize = Lens.lens (\ListClosedWorkflowExecutions' {maximumPageSize} -> maximumPageSize) (\s@ListClosedWorkflowExecutions' {} a -> s {maximumPageSize = a} :: ListClosedWorkflowExecutions)

-- | When set to @true@, returns the results in reverse order. By default the
-- results are returned in descending order of the start or the close time
-- of the executions.
listClosedWorkflowExecutions_reverseOrder :: Lens.Lens' ListClosedWorkflowExecutions (Prelude.Maybe Prelude.Bool)
listClosedWorkflowExecutions_reverseOrder = Lens.lens (\ListClosedWorkflowExecutions' {reverseOrder} -> reverseOrder) (\s@ListClosedWorkflowExecutions' {} a -> s {reverseOrder = a} :: ListClosedWorkflowExecutions)

-- | If specified, the workflow executions are included in the returned
-- results based on whether their close times are within the range
-- specified by this filter. Also, if this parameter is specified, the
-- returned results are ordered by their close times.
--
-- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
-- specify one of these in a request but not both.
listClosedWorkflowExecutions_closeTimeFilter :: Lens.Lens' ListClosedWorkflowExecutions (Prelude.Maybe ExecutionTimeFilter)
listClosedWorkflowExecutions_closeTimeFilter = Lens.lens (\ListClosedWorkflowExecutions' {closeTimeFilter} -> closeTimeFilter) (\s@ListClosedWorkflowExecutions' {} a -> s {closeTimeFilter = a} :: ListClosedWorkflowExecutions)

-- | If specified, the workflow executions are included in the returned
-- results based on whether their start times are within the range
-- specified by this filter. Also, if this parameter is specified, the
-- returned results are ordered by their start times.
--
-- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
-- specify one of these in a request but not both.
listClosedWorkflowExecutions_startTimeFilter :: Lens.Lens' ListClosedWorkflowExecutions (Prelude.Maybe ExecutionTimeFilter)
listClosedWorkflowExecutions_startTimeFilter = Lens.lens (\ListClosedWorkflowExecutions' {startTimeFilter} -> startTimeFilter) (\s@ListClosedWorkflowExecutions' {} a -> s {startTimeFilter = a} :: ListClosedWorkflowExecutions)

-- | If specified, only workflow executions matching the workflow ID
-- specified in the filter are returned.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
listClosedWorkflowExecutions_executionFilter :: Lens.Lens' ListClosedWorkflowExecutions (Prelude.Maybe WorkflowExecutionFilter)
listClosedWorkflowExecutions_executionFilter = Lens.lens (\ListClosedWorkflowExecutions' {executionFilter} -> executionFilter) (\s@ListClosedWorkflowExecutions' {} a -> s {executionFilter = a} :: ListClosedWorkflowExecutions)

-- | If specified, only workflow executions that match this /close status/
-- are listed. For example, if TERMINATED is specified, then only
-- TERMINATED workflow executions are listed.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
listClosedWorkflowExecutions_closeStatusFilter :: Lens.Lens' ListClosedWorkflowExecutions (Prelude.Maybe CloseStatusFilter)
listClosedWorkflowExecutions_closeStatusFilter = Lens.lens (\ListClosedWorkflowExecutions' {closeStatusFilter} -> closeStatusFilter) (\s@ListClosedWorkflowExecutions' {} a -> s {closeStatusFilter = a} :: ListClosedWorkflowExecutions)

-- | The name of the domain that contains the workflow executions to list.
listClosedWorkflowExecutions_domain :: Lens.Lens' ListClosedWorkflowExecutions Prelude.Text
listClosedWorkflowExecutions_domain = Lens.lens (\ListClosedWorkflowExecutions' {domain} -> domain) (\s@ListClosedWorkflowExecutions' {} a -> s {domain = a} :: ListClosedWorkflowExecutions)

instance Core.AWSPager ListClosedWorkflowExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? workflowExecutionInfos_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. workflowExecutionInfos_executionInfos) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listClosedWorkflowExecutions_nextPageToken
          Lens..~ rs
          Lens.^? workflowExecutionInfos_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListClosedWorkflowExecutions where
  type
    AWSResponse ListClosedWorkflowExecutions =
      WorkflowExecutionInfos
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance
  Prelude.Hashable
    ListClosedWorkflowExecutions

instance Prelude.NFData ListClosedWorkflowExecutions

instance Core.ToHeaders ListClosedWorkflowExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.ListClosedWorkflowExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListClosedWorkflowExecutions where
  toJSON ListClosedWorkflowExecutions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("typeFilter" Core..=) Prelude.<$> typeFilter,
            ("tagFilter" Core..=) Prelude.<$> tagFilter,
            ("nextPageToken" Core..=) Prelude.<$> nextPageToken,
            ("maximumPageSize" Core..=)
              Prelude.<$> maximumPageSize,
            ("reverseOrder" Core..=) Prelude.<$> reverseOrder,
            ("closeTimeFilter" Core..=)
              Prelude.<$> closeTimeFilter,
            ("startTimeFilter" Core..=)
              Prelude.<$> startTimeFilter,
            ("executionFilter" Core..=)
              Prelude.<$> executionFilter,
            ("closeStatusFilter" Core..=)
              Prelude.<$> closeStatusFilter,
            Prelude.Just ("domain" Core..= domain)
          ]
      )

instance Core.ToPath ListClosedWorkflowExecutions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListClosedWorkflowExecutions where
  toQuery = Prelude.const Prelude.mempty
