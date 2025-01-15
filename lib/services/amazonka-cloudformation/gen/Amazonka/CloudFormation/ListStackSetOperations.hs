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
-- Module      : Amazonka.CloudFormation.ListStackSetOperations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about operations performed on a stack set.
--
-- This operation returns paginated results.
module Amazonka.CloudFormation.ListStackSetOperations
  ( -- * Creating a Request
    ListStackSetOperations (..),
    newListStackSetOperations,

    -- * Request Lenses
    listStackSetOperations_callAs,
    listStackSetOperations_maxResults,
    listStackSetOperations_nextToken,
    listStackSetOperations_stackSetName,

    -- * Destructuring the Response
    ListStackSetOperationsResponse (..),
    newListStackSetOperationsResponse,

    -- * Response Lenses
    listStackSetOperationsResponse_nextToken,
    listStackSetOperationsResponse_summaries,
    listStackSetOperationsResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStackSetOperations' smart constructor.
data ListStackSetOperations = ListStackSetOperations'
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
    -- | The maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous paginated request didn\'t return all of the remaining
    -- results, the response object\'s @NextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call
    -- @ListStackSetOperations@ again and assign that token to the request
    -- object\'s @NextToken@ parameter. If there are no remaining results, the
    -- previous response object\'s @NextToken@ parameter is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name or unique ID of the stack set that you want to get operation
    -- summaries for.
    stackSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStackSetOperations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAs', 'listStackSetOperations_callAs' - [Service-managed permissions] Specifies whether you are acting as an
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
-- 'maxResults', 'listStackSetOperations_maxResults' - The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
--
-- 'nextToken', 'listStackSetOperations_nextToken' - If the previous paginated request didn\'t return all of the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListStackSetOperations@ again and assign that token to the request
-- object\'s @NextToken@ parameter. If there are no remaining results, the
-- previous response object\'s @NextToken@ parameter is set to @null@.
--
-- 'stackSetName', 'listStackSetOperations_stackSetName' - The name or unique ID of the stack set that you want to get operation
-- summaries for.
newListStackSetOperations ::
  -- | 'stackSetName'
  Prelude.Text ->
  ListStackSetOperations
newListStackSetOperations pStackSetName_ =
  ListStackSetOperations'
    { callAs = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      stackSetName = pStackSetName_
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
listStackSetOperations_callAs :: Lens.Lens' ListStackSetOperations (Prelude.Maybe CallAs)
listStackSetOperations_callAs = Lens.lens (\ListStackSetOperations' {callAs} -> callAs) (\s@ListStackSetOperations' {} a -> s {callAs = a} :: ListStackSetOperations)

-- | The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
listStackSetOperations_maxResults :: Lens.Lens' ListStackSetOperations (Prelude.Maybe Prelude.Natural)
listStackSetOperations_maxResults = Lens.lens (\ListStackSetOperations' {maxResults} -> maxResults) (\s@ListStackSetOperations' {} a -> s {maxResults = a} :: ListStackSetOperations)

-- | If the previous paginated request didn\'t return all of the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListStackSetOperations@ again and assign that token to the request
-- object\'s @NextToken@ parameter. If there are no remaining results, the
-- previous response object\'s @NextToken@ parameter is set to @null@.
listStackSetOperations_nextToken :: Lens.Lens' ListStackSetOperations (Prelude.Maybe Prelude.Text)
listStackSetOperations_nextToken = Lens.lens (\ListStackSetOperations' {nextToken} -> nextToken) (\s@ListStackSetOperations' {} a -> s {nextToken = a} :: ListStackSetOperations)

-- | The name or unique ID of the stack set that you want to get operation
-- summaries for.
listStackSetOperations_stackSetName :: Lens.Lens' ListStackSetOperations Prelude.Text
listStackSetOperations_stackSetName = Lens.lens (\ListStackSetOperations' {stackSetName} -> stackSetName) (\s@ListStackSetOperations' {} a -> s {stackSetName = a} :: ListStackSetOperations)

instance Core.AWSPager ListStackSetOperations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStackSetOperationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStackSetOperationsResponse_summaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listStackSetOperations_nextToken
              Lens..~ rs
              Lens.^? listStackSetOperationsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListStackSetOperations where
  type
    AWSResponse ListStackSetOperations =
      ListStackSetOperationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListStackSetOperationsResult"
      ( \s h x ->
          ListStackSetOperationsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "Summaries" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStackSetOperations where
  hashWithSalt _salt ListStackSetOperations' {..} =
    _salt
      `Prelude.hashWithSalt` callAs
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stackSetName

instance Prelude.NFData ListStackSetOperations where
  rnf ListStackSetOperations' {..} =
    Prelude.rnf callAs `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf stackSetName

instance Data.ToHeaders ListStackSetOperations where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListStackSetOperations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListStackSetOperations where
  toQuery ListStackSetOperations' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListStackSetOperations" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "CallAs" Data.=: callAs,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "StackSetName" Data.=: stackSetName
      ]

-- | /See:/ 'newListStackSetOperationsResponse' smart constructor.
data ListStackSetOperationsResponse = ListStackSetOperationsResponse'
  { -- | If the request doesn\'t return all results, @NextToken@ is set to a
    -- token. To retrieve the next set of results, call @ListOperationResults@
    -- again and assign that token to the request object\'s @NextToken@
    -- parameter. If there are no remaining results, @NextToken@ is set to
    -- @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @StackSetOperationSummary@ structures that contain summary
    -- information about operations for the specified stack set.
    summaries :: Prelude.Maybe [StackSetOperationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStackSetOperationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStackSetOperationsResponse_nextToken' - If the request doesn\'t return all results, @NextToken@ is set to a
-- token. To retrieve the next set of results, call @ListOperationResults@
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If there are no remaining results, @NextToken@ is set to
-- @null@.
--
-- 'summaries', 'listStackSetOperationsResponse_summaries' - A list of @StackSetOperationSummary@ structures that contain summary
-- information about operations for the specified stack set.
--
-- 'httpStatus', 'listStackSetOperationsResponse_httpStatus' - The response's http status code.
newListStackSetOperationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStackSetOperationsResponse
newListStackSetOperationsResponse pHttpStatus_ =
  ListStackSetOperationsResponse'
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
listStackSetOperationsResponse_nextToken :: Lens.Lens' ListStackSetOperationsResponse (Prelude.Maybe Prelude.Text)
listStackSetOperationsResponse_nextToken = Lens.lens (\ListStackSetOperationsResponse' {nextToken} -> nextToken) (\s@ListStackSetOperationsResponse' {} a -> s {nextToken = a} :: ListStackSetOperationsResponse)

-- | A list of @StackSetOperationSummary@ structures that contain summary
-- information about operations for the specified stack set.
listStackSetOperationsResponse_summaries :: Lens.Lens' ListStackSetOperationsResponse (Prelude.Maybe [StackSetOperationSummary])
listStackSetOperationsResponse_summaries = Lens.lens (\ListStackSetOperationsResponse' {summaries} -> summaries) (\s@ListStackSetOperationsResponse' {} a -> s {summaries = a} :: ListStackSetOperationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStackSetOperationsResponse_httpStatus :: Lens.Lens' ListStackSetOperationsResponse Prelude.Int
listStackSetOperationsResponse_httpStatus = Lens.lens (\ListStackSetOperationsResponse' {httpStatus} -> httpStatus) (\s@ListStackSetOperationsResponse' {} a -> s {httpStatus = a} :: ListStackSetOperationsResponse)

instance
  Prelude.NFData
    ListStackSetOperationsResponse
  where
  rnf ListStackSetOperationsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf summaries `Prelude.seq`
        Prelude.rnf httpStatus
