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
-- Module      : Network.AWS.CloudFormation.ListStackSetOperationResults
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about the results of a stack set operation.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStackSetOperationResults
  ( -- * Creating a Request
    ListStackSetOperationResults (..),
    newListStackSetOperationResults,

    -- * Request Lenses
    listStackSetOperationResults_nextToken,
    listStackSetOperationResults_maxResults,
    listStackSetOperationResults_callAs,
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

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListStackSetOperationResults' smart constructor.
data ListStackSetOperationResults = ListStackSetOperationResults'
  { -- | If the previous request didn\'t return all of the remaining results, the
    -- response object\'s @NextToken@ parameter value is set to a token. To
    -- retrieve the next set of results, call @ListStackSetOperationResults@
    -- again and assign that token to the request object\'s @NextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s @NextToken@ parameter is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
    --     Your AWS account must be registered as a delegated administrator in
    --     the management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /AWS CloudFormation User Guide/.
    callAs :: Prelude.Maybe CallAs,
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
-- 'nextToken', 'listStackSetOperationResults_nextToken' - If the previous request didn\'t return all of the remaining results, the
-- response object\'s @NextToken@ parameter value is set to a token. To
-- retrieve the next set of results, call @ListStackSetOperationResults@
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s @NextToken@ parameter is set to @null@.
--
-- 'maxResults', 'listStackSetOperationResults_maxResults' - The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
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
--     Your AWS account must be registered as a delegated administrator in
--     the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
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
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        callAs = Prelude.Nothing,
        stackSetName = pStackSetName_,
        operationId = pOperationId_
      }

-- | If the previous request didn\'t return all of the remaining results, the
-- response object\'s @NextToken@ parameter value is set to a token. To
-- retrieve the next set of results, call @ListStackSetOperationResults@
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s @NextToken@ parameter is set to @null@.
listStackSetOperationResults_nextToken :: Lens.Lens' ListStackSetOperationResults (Prelude.Maybe Prelude.Text)
listStackSetOperationResults_nextToken = Lens.lens (\ListStackSetOperationResults' {nextToken} -> nextToken) (\s@ListStackSetOperationResults' {} a -> s {nextToken = a} :: ListStackSetOperationResults)

-- | The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
listStackSetOperationResults_maxResults :: Lens.Lens' ListStackSetOperationResults (Prelude.Maybe Prelude.Natural)
listStackSetOperationResults_maxResults = Lens.lens (\ListStackSetOperationResults' {maxResults} -> maxResults) (\s@ListStackSetOperationResults' {} a -> s {maxResults = a} :: ListStackSetOperationResults)

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
--     Your AWS account must be registered as a delegated administrator in
--     the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
listStackSetOperationResults_callAs :: Lens.Lens' ListStackSetOperationResults (Prelude.Maybe CallAs)
listStackSetOperationResults_callAs = Lens.lens (\ListStackSetOperationResults' {callAs} -> callAs) (\s@ListStackSetOperationResults' {} a -> s {callAs = a} :: ListStackSetOperationResults)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListStackSetOperationResultsResult"
      ( \s h x ->
          ListStackSetOperationResultsResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> ( x Core..@? "Summaries" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListStackSetOperationResults

instance Prelude.NFData ListStackSetOperationResults

instance Core.ToHeaders ListStackSetOperationResults where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListStackSetOperationResults where
  toPath = Prelude.const "/"

instance Core.ToQuery ListStackSetOperationResults where
  toQuery ListStackSetOperationResults' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ListStackSetOperationResults" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        "CallAs" Core.=: callAs,
        "StackSetName" Core.=: stackSetName,
        "OperationId" Core.=: operationId
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
    -- Regions that are included in the operation.
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
-- Regions that are included in the operation.
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
-- Regions that are included in the operation.
listStackSetOperationResultsResponse_summaries :: Lens.Lens' ListStackSetOperationResultsResponse (Prelude.Maybe [StackSetOperationResultSummary])
listStackSetOperationResultsResponse_summaries = Lens.lens (\ListStackSetOperationResultsResponse' {summaries} -> summaries) (\s@ListStackSetOperationResultsResponse' {} a -> s {summaries = a} :: ListStackSetOperationResultsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listStackSetOperationResultsResponse_httpStatus :: Lens.Lens' ListStackSetOperationResultsResponse Prelude.Int
listStackSetOperationResultsResponse_httpStatus = Lens.lens (\ListStackSetOperationResultsResponse' {httpStatus} -> httpStatus) (\s@ListStackSetOperationResultsResponse' {} a -> s {httpStatus = a} :: ListStackSetOperationResultsResponse)

instance
  Prelude.NFData
    ListStackSetOperationResultsResponse
