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
-- Module      : Network.AWS.CloudFormation.ListStackInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about stack instances that are associated
-- with the specified stack set. You can filter for stack instances that
-- are associated with a specific AWS account name or Region, or that have
-- a specific status.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStackInstances
  ( -- * Creating a Request
    ListStackInstances (..),
    newListStackInstances,

    -- * Request Lenses
    listStackInstances_nextToken,
    listStackInstances_stackInstanceAccount,
    listStackInstances_maxResults,
    listStackInstances_callAs,
    listStackInstances_stackInstanceRegion,
    listStackInstances_filters,
    listStackInstances_stackSetName,

    -- * Destructuring the Response
    ListStackInstancesResponse (..),
    newListStackInstancesResponse,

    -- * Response Lenses
    listStackInstancesResponse_nextToken,
    listStackInstancesResponse_summaries,
    listStackInstancesResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListStackInstances' smart constructor.
data ListStackInstances = ListStackInstances'
  { -- | If the previous request didn\'t return all of the remaining results, the
    -- response\'s @NextToken@ parameter value is set to a token. To retrieve
    -- the next set of results, call @ListStackInstances@ again and assign that
    -- token to the request object\'s @NextToken@ parameter. If there are no
    -- remaining results, the previous response object\'s @NextToken@ parameter
    -- is set to @null@.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of the AWS account that you want to list stack instances for.
    stackInstanceAccount :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
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
    callAs :: Core.Maybe CallAs,
    -- | The name of the Region where you want to list stack instances.
    stackInstanceRegion :: Core.Maybe Core.Text,
    -- | The status that stack instances are filtered by.
    filters :: Core.Maybe [StackInstanceFilter],
    -- | The name or unique ID of the stack set that you want to list stack
    -- instances for.
    stackSetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStackInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStackInstances_nextToken' - If the previous request didn\'t return all of the remaining results, the
-- response\'s @NextToken@ parameter value is set to a token. To retrieve
-- the next set of results, call @ListStackInstances@ again and assign that
-- token to the request object\'s @NextToken@ parameter. If there are no
-- remaining results, the previous response object\'s @NextToken@ parameter
-- is set to @null@.
--
-- 'stackInstanceAccount', 'listStackInstances_stackInstanceAccount' - The name of the AWS account that you want to list stack instances for.
--
-- 'maxResults', 'listStackInstances_maxResults' - The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
--
-- 'callAs', 'listStackInstances_callAs' - [Service-managed permissions] Specifies whether you are acting as an
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
-- 'stackInstanceRegion', 'listStackInstances_stackInstanceRegion' - The name of the Region where you want to list stack instances.
--
-- 'filters', 'listStackInstances_filters' - The status that stack instances are filtered by.
--
-- 'stackSetName', 'listStackInstances_stackSetName' - The name or unique ID of the stack set that you want to list stack
-- instances for.
newListStackInstances ::
  -- | 'stackSetName'
  Core.Text ->
  ListStackInstances
newListStackInstances pStackSetName_ =
  ListStackInstances'
    { nextToken = Core.Nothing,
      stackInstanceAccount = Core.Nothing,
      maxResults = Core.Nothing,
      callAs = Core.Nothing,
      stackInstanceRegion = Core.Nothing,
      filters = Core.Nothing,
      stackSetName = pStackSetName_
    }

-- | If the previous request didn\'t return all of the remaining results, the
-- response\'s @NextToken@ parameter value is set to a token. To retrieve
-- the next set of results, call @ListStackInstances@ again and assign that
-- token to the request object\'s @NextToken@ parameter. If there are no
-- remaining results, the previous response object\'s @NextToken@ parameter
-- is set to @null@.
listStackInstances_nextToken :: Lens.Lens' ListStackInstances (Core.Maybe Core.Text)
listStackInstances_nextToken = Lens.lens (\ListStackInstances' {nextToken} -> nextToken) (\s@ListStackInstances' {} a -> s {nextToken = a} :: ListStackInstances)

-- | The name of the AWS account that you want to list stack instances for.
listStackInstances_stackInstanceAccount :: Lens.Lens' ListStackInstances (Core.Maybe Core.Text)
listStackInstances_stackInstanceAccount = Lens.lens (\ListStackInstances' {stackInstanceAccount} -> stackInstanceAccount) (\s@ListStackInstances' {} a -> s {stackInstanceAccount = a} :: ListStackInstances)

-- | The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
listStackInstances_maxResults :: Lens.Lens' ListStackInstances (Core.Maybe Core.Natural)
listStackInstances_maxResults = Lens.lens (\ListStackInstances' {maxResults} -> maxResults) (\s@ListStackInstances' {} a -> s {maxResults = a} :: ListStackInstances)

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
listStackInstances_callAs :: Lens.Lens' ListStackInstances (Core.Maybe CallAs)
listStackInstances_callAs = Lens.lens (\ListStackInstances' {callAs} -> callAs) (\s@ListStackInstances' {} a -> s {callAs = a} :: ListStackInstances)

-- | The name of the Region where you want to list stack instances.
listStackInstances_stackInstanceRegion :: Lens.Lens' ListStackInstances (Core.Maybe Core.Text)
listStackInstances_stackInstanceRegion = Lens.lens (\ListStackInstances' {stackInstanceRegion} -> stackInstanceRegion) (\s@ListStackInstances' {} a -> s {stackInstanceRegion = a} :: ListStackInstances)

-- | The status that stack instances are filtered by.
listStackInstances_filters :: Lens.Lens' ListStackInstances (Core.Maybe [StackInstanceFilter])
listStackInstances_filters = Lens.lens (\ListStackInstances' {filters} -> filters) (\s@ListStackInstances' {} a -> s {filters = a} :: ListStackInstances) Core.. Lens.mapping Lens._Coerce

-- | The name or unique ID of the stack set that you want to list stack
-- instances for.
listStackInstances_stackSetName :: Lens.Lens' ListStackInstances Core.Text
listStackInstances_stackSetName = Lens.lens (\ListStackInstances' {stackSetName} -> stackSetName) (\s@ListStackInstances' {} a -> s {stackSetName = a} :: ListStackInstances)

instance Core.AWSPager ListStackInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStackInstancesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listStackInstancesResponse_summaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listStackInstances_nextToken
          Lens..~ rs
          Lens.^? listStackInstancesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListStackInstances where
  type
    AWSResponse ListStackInstances =
      ListStackInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListStackInstancesResult"
      ( \s h x ->
          ListStackInstancesResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "Summaries" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListStackInstances

instance Core.NFData ListStackInstances

instance Core.ToHeaders ListStackInstances where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListStackInstances where
  toPath = Core.const "/"

instance Core.ToQuery ListStackInstances where
  toQuery ListStackInstances' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListStackInstances" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "StackInstanceAccount" Core.=: stackInstanceAccount,
        "MaxResults" Core.=: maxResults,
        "CallAs" Core.=: callAs,
        "StackInstanceRegion" Core.=: stackInstanceRegion,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> filters),
        "StackSetName" Core.=: stackSetName
      ]

-- | /See:/ 'newListStackInstancesResponse' smart constructor.
data ListStackInstancesResponse = ListStackInstancesResponse'
  { -- | If the request doesn\'t return all of the remaining results, @NextToken@
    -- is set to a token. To retrieve the next set of results, call
    -- @ListStackInstances@ again and assign that token to the request
    -- object\'s @NextToken@ parameter. If the request returns all results,
    -- @NextToken@ is set to @null@.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @StackInstanceSummary@ structures that contain information
    -- about the specified stack instances.
    summaries :: Core.Maybe [StackInstanceSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStackInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStackInstancesResponse_nextToken' - If the request doesn\'t return all of the remaining results, @NextToken@
-- is set to a token. To retrieve the next set of results, call
-- @ListStackInstances@ again and assign that token to the request
-- object\'s @NextToken@ parameter. If the request returns all results,
-- @NextToken@ is set to @null@.
--
-- 'summaries', 'listStackInstancesResponse_summaries' - A list of @StackInstanceSummary@ structures that contain information
-- about the specified stack instances.
--
-- 'httpStatus', 'listStackInstancesResponse_httpStatus' - The response's http status code.
newListStackInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListStackInstancesResponse
newListStackInstancesResponse pHttpStatus_ =
  ListStackInstancesResponse'
    { nextToken =
        Core.Nothing,
      summaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the request doesn\'t return all of the remaining results, @NextToken@
-- is set to a token. To retrieve the next set of results, call
-- @ListStackInstances@ again and assign that token to the request
-- object\'s @NextToken@ parameter. If the request returns all results,
-- @NextToken@ is set to @null@.
listStackInstancesResponse_nextToken :: Lens.Lens' ListStackInstancesResponse (Core.Maybe Core.Text)
listStackInstancesResponse_nextToken = Lens.lens (\ListStackInstancesResponse' {nextToken} -> nextToken) (\s@ListStackInstancesResponse' {} a -> s {nextToken = a} :: ListStackInstancesResponse)

-- | A list of @StackInstanceSummary@ structures that contain information
-- about the specified stack instances.
listStackInstancesResponse_summaries :: Lens.Lens' ListStackInstancesResponse (Core.Maybe [StackInstanceSummary])
listStackInstancesResponse_summaries = Lens.lens (\ListStackInstancesResponse' {summaries} -> summaries) (\s@ListStackInstancesResponse' {} a -> s {summaries = a} :: ListStackInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listStackInstancesResponse_httpStatus :: Lens.Lens' ListStackInstancesResponse Core.Int
listStackInstancesResponse_httpStatus = Lens.lens (\ListStackInstancesResponse' {httpStatus} -> httpStatus) (\s@ListStackInstancesResponse' {} a -> s {httpStatus = a} :: ListStackInstancesResponse)

instance Core.NFData ListStackInstancesResponse
