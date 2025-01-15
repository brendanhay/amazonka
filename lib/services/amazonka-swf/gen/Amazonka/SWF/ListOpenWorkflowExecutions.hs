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
-- Module      : Amazonka.SWF.ListOpenWorkflowExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.SWF.ListOpenWorkflowExecutions
  ( -- * Creating a Request
    ListOpenWorkflowExecutions (..),
    newListOpenWorkflowExecutions,

    -- * Request Lenses
    listOpenWorkflowExecutions_executionFilter,
    listOpenWorkflowExecutions_maximumPageSize,
    listOpenWorkflowExecutions_nextPageToken,
    listOpenWorkflowExecutions_reverseOrder,
    listOpenWorkflowExecutions_tagFilter,
    listOpenWorkflowExecutions_typeFilter,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newListOpenWorkflowExecutions' smart constructor.
data ListOpenWorkflowExecutions = ListOpenWorkflowExecutions'
  { -- | If specified, only workflow executions matching the workflow ID
    -- specified in the filter are returned.
    --
    -- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
    -- You can specify at most one of these in a request.
    executionFilter :: Prelude.Maybe WorkflowExecutionFilter,
    -- | The maximum number of results that are returned per call. Use
    -- @nextPageToken@ to obtain further pages of results.
    maximumPageSize :: Prelude.Maybe Prelude.Natural,
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
    -- | When set to @true@, returns the results in reverse order. By default the
    -- results are returned in descending order of the start time of the
    -- executions.
    reverseOrder :: Prelude.Maybe Prelude.Bool,
    -- | If specified, only executions that have the matching tag are listed.
    --
    -- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
    -- You can specify at most one of these in a request.
    tagFilter :: Prelude.Maybe TagFilter,
    -- | If specified, only executions of the type specified in the filter are
    -- returned.
    --
    -- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
    -- You can specify at most one of these in a request.
    typeFilter :: Prelude.Maybe WorkflowTypeFilter,
    -- | The name of the domain that contains the workflow executions to list.
    domain :: Prelude.Text,
    -- | Workflow executions are included in the returned results based on
    -- whether their start times are within the range specified by this filter.
    startTimeFilter :: ExecutionTimeFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOpenWorkflowExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionFilter', 'listOpenWorkflowExecutions_executionFilter' - If specified, only workflow executions matching the workflow ID
-- specified in the filter are returned.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
--
-- 'maximumPageSize', 'listOpenWorkflowExecutions_maximumPageSize' - The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
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
-- 'reverseOrder', 'listOpenWorkflowExecutions_reverseOrder' - When set to @true@, returns the results in reverse order. By default the
-- results are returned in descending order of the start time of the
-- executions.
--
-- 'tagFilter', 'listOpenWorkflowExecutions_tagFilter' - If specified, only executions that have the matching tag are listed.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
--
-- 'typeFilter', 'listOpenWorkflowExecutions_typeFilter' - If specified, only executions of the type specified in the filter are
-- returned.
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
  Prelude.Text ->
  -- | 'startTimeFilter'
  ExecutionTimeFilter ->
  ListOpenWorkflowExecutions
newListOpenWorkflowExecutions
  pDomain_
  pStartTimeFilter_ =
    ListOpenWorkflowExecutions'
      { executionFilter =
          Prelude.Nothing,
        maximumPageSize = Prelude.Nothing,
        nextPageToken = Prelude.Nothing,
        reverseOrder = Prelude.Nothing,
        tagFilter = Prelude.Nothing,
        typeFilter = Prelude.Nothing,
        domain = pDomain_,
        startTimeFilter = pStartTimeFilter_
      }

-- | If specified, only workflow executions matching the workflow ID
-- specified in the filter are returned.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
listOpenWorkflowExecutions_executionFilter :: Lens.Lens' ListOpenWorkflowExecutions (Prelude.Maybe WorkflowExecutionFilter)
listOpenWorkflowExecutions_executionFilter = Lens.lens (\ListOpenWorkflowExecutions' {executionFilter} -> executionFilter) (\s@ListOpenWorkflowExecutions' {} a -> s {executionFilter = a} :: ListOpenWorkflowExecutions)

-- | The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
listOpenWorkflowExecutions_maximumPageSize :: Lens.Lens' ListOpenWorkflowExecutions (Prelude.Maybe Prelude.Natural)
listOpenWorkflowExecutions_maximumPageSize = Lens.lens (\ListOpenWorkflowExecutions' {maximumPageSize} -> maximumPageSize) (\s@ListOpenWorkflowExecutions' {} a -> s {maximumPageSize = a} :: ListOpenWorkflowExecutions)

-- | If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
listOpenWorkflowExecutions_nextPageToken :: Lens.Lens' ListOpenWorkflowExecutions (Prelude.Maybe Prelude.Text)
listOpenWorkflowExecutions_nextPageToken = Lens.lens (\ListOpenWorkflowExecutions' {nextPageToken} -> nextPageToken) (\s@ListOpenWorkflowExecutions' {} a -> s {nextPageToken = a} :: ListOpenWorkflowExecutions)

-- | When set to @true@, returns the results in reverse order. By default the
-- results are returned in descending order of the start time of the
-- executions.
listOpenWorkflowExecutions_reverseOrder :: Lens.Lens' ListOpenWorkflowExecutions (Prelude.Maybe Prelude.Bool)
listOpenWorkflowExecutions_reverseOrder = Lens.lens (\ListOpenWorkflowExecutions' {reverseOrder} -> reverseOrder) (\s@ListOpenWorkflowExecutions' {} a -> s {reverseOrder = a} :: ListOpenWorkflowExecutions)

-- | If specified, only executions that have the matching tag are listed.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
listOpenWorkflowExecutions_tagFilter :: Lens.Lens' ListOpenWorkflowExecutions (Prelude.Maybe TagFilter)
listOpenWorkflowExecutions_tagFilter = Lens.lens (\ListOpenWorkflowExecutions' {tagFilter} -> tagFilter) (\s@ListOpenWorkflowExecutions' {} a -> s {tagFilter = a} :: ListOpenWorkflowExecutions)

-- | If specified, only executions of the type specified in the filter are
-- returned.
--
-- @executionFilter@, @typeFilter@ and @tagFilter@ are mutually exclusive.
-- You can specify at most one of these in a request.
listOpenWorkflowExecutions_typeFilter :: Lens.Lens' ListOpenWorkflowExecutions (Prelude.Maybe WorkflowTypeFilter)
listOpenWorkflowExecutions_typeFilter = Lens.lens (\ListOpenWorkflowExecutions' {typeFilter} -> typeFilter) (\s@ListOpenWorkflowExecutions' {} a -> s {typeFilter = a} :: ListOpenWorkflowExecutions)

-- | The name of the domain that contains the workflow executions to list.
listOpenWorkflowExecutions_domain :: Lens.Lens' ListOpenWorkflowExecutions Prelude.Text
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
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. workflowExecutionInfos_executionInfos) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listOpenWorkflowExecutions_nextPageToken
              Lens..~ rs
              Lens.^? workflowExecutionInfos_nextPageToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListOpenWorkflowExecutions where
  type
    AWSResponse ListOpenWorkflowExecutions =
      WorkflowExecutionInfos
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable ListOpenWorkflowExecutions where
  hashWithSalt _salt ListOpenWorkflowExecutions' {..} =
    _salt
      `Prelude.hashWithSalt` executionFilter
      `Prelude.hashWithSalt` maximumPageSize
      `Prelude.hashWithSalt` nextPageToken
      `Prelude.hashWithSalt` reverseOrder
      `Prelude.hashWithSalt` tagFilter
      `Prelude.hashWithSalt` typeFilter
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` startTimeFilter

instance Prelude.NFData ListOpenWorkflowExecutions where
  rnf ListOpenWorkflowExecutions' {..} =
    Prelude.rnf executionFilter `Prelude.seq`
      Prelude.rnf maximumPageSize `Prelude.seq`
        Prelude.rnf nextPageToken `Prelude.seq`
          Prelude.rnf reverseOrder `Prelude.seq`
            Prelude.rnf tagFilter `Prelude.seq`
              Prelude.rnf typeFilter `Prelude.seq`
                Prelude.rnf domain `Prelude.seq`
                  Prelude.rnf startTimeFilter

instance Data.ToHeaders ListOpenWorkflowExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.ListOpenWorkflowExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListOpenWorkflowExecutions where
  toJSON ListOpenWorkflowExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("executionFilter" Data..=)
              Prelude.<$> executionFilter,
            ("maximumPageSize" Data..=)
              Prelude.<$> maximumPageSize,
            ("nextPageToken" Data..=) Prelude.<$> nextPageToken,
            ("reverseOrder" Data..=) Prelude.<$> reverseOrder,
            ("tagFilter" Data..=) Prelude.<$> tagFilter,
            ("typeFilter" Data..=) Prelude.<$> typeFilter,
            Prelude.Just ("domain" Data..= domain),
            Prelude.Just
              ("startTimeFilter" Data..= startTimeFilter)
          ]
      )

instance Data.ToPath ListOpenWorkflowExecutions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListOpenWorkflowExecutions where
  toQuery = Prelude.const Prelude.mempty
