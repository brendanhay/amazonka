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
-- Module      : Amazonka.CloudFormation.ListStackSetOperationResults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about the results of a stack set operation.
--
-- This operation returns paginated results.
module Amazonka.CloudFormation.ListStackSetOperationResults
  ( -- * Creating a Request
    ListStackSetOperationResults (..),
    newListStackSetOperationResults,

    -- * Request Lenses
    listStackSetOperationResults_callAs,
    listStackSetOperationResults_filters,
    listStackSetOperationResults_maxResults,
    listStackSetOperationResults_nextToken,
    listStackSetOperationResults_stackSetName,
    listStackSetOperationResults_operationId,

    -- * Destructuring the Response
    ListStackSetOperationResultsResponse (..),
    newListStackSetOperationResultsResponse,

    -- * Response Lenses
    listStackSetOperationResultsResponse_nextToken,
    listStackSetOperationResultsResponse_summaries,
    listStackSetOperationResultsResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStackSetOperationResults' smart constructor.
data ListStackSetOperationResults = ListStackSetOperationResults'
  { -- | [Service-managed permissions] Specifies whether you are acting as an
    -- account administrator in the organization\'s management account or as a
    -- delegated administrator in a member account.
    --
    -- By default, @SELF@ is specified. Use @SELF@ for stack sets with
    -- self-managed permissions.
    --
    -- -   If you are signed in to the management account, specify @SELF@.
    --
    -- -   If you are signed in to a delegated administrator account, specify
    --     @DELEGATED_ADMIN@.
    --
    --     Your Amazon Web Services account must be registered as a delegated
    --     administrator in the management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /CloudFormation User Guide/.
    callAs :: Prelude.Maybe CallAs,
    -- | The filter to apply to operation results.
    filters :: Prelude.Maybe [OperationResultFilter],
    -- | The maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous request didn\'t return all the remaining results, the
    -- response object\'s @NextToken@ parameter value is set to a token. To
    -- retrieve the next set of results, call @ListStackSetOperationResults@
    -- again and assign that token to the request object\'s @NextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s @NextToken@ parameter is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name or unique ID of the stack set that you want to get operation
    -- results for.
    stackSetName :: Prelude.Text,
    -- | The ID of the stack set operation.
    operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStackSetOperationResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAs', 'listStackSetOperationResults_callAs' - [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
--
-- 'filters', 'listStackSetOperationResults_filters' - The filter to apply to operation results.
--
-- 'maxResults', 'listStackSetOperationResults_maxResults' - The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
--
-- 'nextToken', 'listStackSetOperationResults_nextToken' - If the previous request didn\'t return all the remaining results, the
-- response object\'s @NextToken@ parameter value is set to a token. To
-- retrieve the next set of results, call @ListStackSetOperationResults@
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s @NextToken@ parameter is set to @null@.
--
-- 'stackSetName', 'listStackSetOperationResults_stackSetName' - The name or unique ID of the stack set that you want to get operation
-- results for.
--
-- 'operationId', 'listStackSetOperationResults_operationId' - The ID of the stack set operation.
newListStackSetOperationResults ::
  -- | 'stackSetName'
  Prelude.Text ->
  -- | 'operationId'
  Prelude.Text ->
  ListStackSetOperationResults
newListStackSetOperationResults
  pStackSetName_
  pOperationId_ =
    ListStackSetOperationResults'
      { callAs =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        stackSetName = pStackSetName_,
        operationId = pOperationId_
      }

-- | [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
listStackSetOperationResults_callAs :: Lens.Lens' ListStackSetOperationResults (Prelude.Maybe CallAs)
listStackSetOperationResults_callAs = Lens.lens (\ListStackSetOperationResults' {callAs} -> callAs) (\s@ListStackSetOperationResults' {} a -> s {callAs = a} :: ListStackSetOperationResults)

-- | The filter to apply to operation results.
listStackSetOperationResults_filters :: Lens.Lens' ListStackSetOperationResults (Prelude.Maybe [OperationResultFilter])
listStackSetOperationResults_filters = Lens.lens (\ListStackSetOperationResults' {filters} -> filters) (\s@ListStackSetOperationResults' {} a -> s {filters = a} :: ListStackSetOperationResults) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
listStackSetOperationResults_maxResults :: Lens.Lens' ListStackSetOperationResults (Prelude.Maybe Prelude.Natural)
listStackSetOperationResults_maxResults = Lens.lens (\ListStackSetOperationResults' {maxResults} -> maxResults) (\s@ListStackSetOperationResults' {} a -> s {maxResults = a} :: ListStackSetOperationResults)

-- | If the previous request didn\'t return all the remaining results, the
-- response object\'s @NextToken@ parameter value is set to a token. To
-- retrieve the next set of results, call @ListStackSetOperationResults@
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s @NextToken@ parameter is set to @null@.
listStackSetOperationResults_nextToken :: Lens.Lens' ListStackSetOperationResults (Prelude.Maybe Prelude.Text)
listStackSetOperationResults_nextToken = Lens.lens (\ListStackSetOperationResults' {nextToken} -> nextToken) (\s@ListStackSetOperationResults' {} a -> s {nextToken = a} :: ListStackSetOperationResults)

-- | The name or unique ID of the stack set that you want to get operation
-- results for.
listStackSetOperationResults_stackSetName :: Lens.Lens' ListStackSetOperationResults Prelude.Text
listStackSetOperationResults_stackSetName = Lens.lens (\ListStackSetOperationResults' {stackSetName} -> stackSetName) (\s@ListStackSetOperationResults' {} a -> s {stackSetName = a} :: ListStackSetOperationResults)

-- | The ID of the stack set operation.
listStackSetOperationResults_operationId :: Lens.Lens' ListStackSetOperationResults Prelude.Text
listStackSetOperationResults_operationId = Lens.lens (\ListStackSetOperationResults' {operationId} -> operationId) (\s@ListStackSetOperationResults' {} a -> s {operationId = a} :: ListStackSetOperationResults)

instance Core.AWSPager ListStackSetOperationResults where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStackSetOperationResultsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStackSetOperationResultsResponse_summaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listStackSetOperationResults_nextToken
              Lens..~ rs
              Lens.^? listStackSetOperationResultsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListStackSetOperationResults where
  type
    AWSResponse ListStackSetOperationResults =
      ListStackSetOperationResultsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListStackSetOperationResultsResult"
      ( \s h x ->
          ListStackSetOperationResultsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "Summaries" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListStackSetOperationResults
  where
  hashWithSalt _salt ListStackSetOperationResults' {..} =
    _salt
      `Prelude.hashWithSalt` callAs
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stackSetName
      `Prelude.hashWithSalt` operationId

instance Prelude.NFData ListStackSetOperationResults where
  rnf ListStackSetOperationResults' {..} =
    Prelude.rnf callAs `Prelude.seq`
      Prelude.rnf filters `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf stackSetName `Prelude.seq`
              Prelude.rnf operationId

instance Data.ToHeaders ListStackSetOperationResults where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListStackSetOperationResults where
  toPath = Prelude.const "/"

instance Data.ToQuery ListStackSetOperationResults where
  toQuery ListStackSetOperationResults' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ListStackSetOperationResults" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "CallAs" Data.=: callAs,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "StackSetName" Data.=: stackSetName,
        "OperationId" Data.=: operationId
      ]

-- | /See:/ 'newListStackSetOperationResultsResponse' smart constructor.
data ListStackSetOperationResultsResponse = ListStackSetOperationResultsResponse'
  { -- | If the request doesn\'t return all results, @NextToken@ is set to a
    -- token. To retrieve the next set of results, call @ListOperationResults@
    -- again and assign that token to the request object\'s @NextToken@
    -- parameter. If there are no remaining results, @NextToken@ is set to
    -- @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @StackSetOperationResultSummary@ structures that contain
    -- information about the specified operation results, for accounts and
    -- Amazon Web Services Regions that are included in the operation.
    summaries :: Prelude.Maybe [StackSetOperationResultSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStackSetOperationResultsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStackSetOperationResultsResponse_nextToken' - If the request doesn\'t return all results, @NextToken@ is set to a
-- token. To retrieve the next set of results, call @ListOperationResults@
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If there are no remaining results, @NextToken@ is set to
-- @null@.
--
-- 'summaries', 'listStackSetOperationResultsResponse_summaries' - A list of @StackSetOperationResultSummary@ structures that contain
-- information about the specified operation results, for accounts and
-- Amazon Web Services Regions that are included in the operation.
--
-- 'httpStatus', 'listStackSetOperationResultsResponse_httpStatus' - The response's http status code.
newListStackSetOperationResultsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStackSetOperationResultsResponse
newListStackSetOperationResultsResponse pHttpStatus_ =
  ListStackSetOperationResultsResponse'
    { nextToken =
        Prelude.Nothing,
      summaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the request doesn\'t return all results, @NextToken@ is set to a
-- token. To retrieve the next set of results, call @ListOperationResults@
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If there are no remaining results, @NextToken@ is set to
-- @null@.
listStackSetOperationResultsResponse_nextToken :: Lens.Lens' ListStackSetOperationResultsResponse (Prelude.Maybe Prelude.Text)
listStackSetOperationResultsResponse_nextToken = Lens.lens (\ListStackSetOperationResultsResponse' {nextToken} -> nextToken) (\s@ListStackSetOperationResultsResponse' {} a -> s {nextToken = a} :: ListStackSetOperationResultsResponse)

-- | A list of @StackSetOperationResultSummary@ structures that contain
-- information about the specified operation results, for accounts and
-- Amazon Web Services Regions that are included in the operation.
listStackSetOperationResultsResponse_summaries :: Lens.Lens' ListStackSetOperationResultsResponse (Prelude.Maybe [StackSetOperationResultSummary])
listStackSetOperationResultsResponse_summaries = Lens.lens (\ListStackSetOperationResultsResponse' {summaries} -> summaries) (\s@ListStackSetOperationResultsResponse' {} a -> s {summaries = a} :: ListStackSetOperationResultsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStackSetOperationResultsResponse_httpStatus :: Lens.Lens' ListStackSetOperationResultsResponse Prelude.Int
listStackSetOperationResultsResponse_httpStatus = Lens.lens (\ListStackSetOperationResultsResponse' {httpStatus} -> httpStatus) (\s@ListStackSetOperationResultsResponse' {} a -> s {httpStatus = a} :: ListStackSetOperationResultsResponse)

instance
  Prelude.NFData
    ListStackSetOperationResultsResponse
  where
  rnf ListStackSetOperationResultsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf summaries `Prelude.seq`
        Prelude.rnf httpStatus
