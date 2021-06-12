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
-- Module      : Network.AWS.EC2.DescribeManagedPrefixLists
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your managed prefix lists and any AWS-managed prefix lists.
--
-- To view the entries for your prefix list, use
-- GetManagedPrefixListEntries.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeManagedPrefixLists
  ( -- * Creating a Request
    DescribeManagedPrefixLists (..),
    newDescribeManagedPrefixLists,

    -- * Request Lenses
    describeManagedPrefixLists_nextToken,
    describeManagedPrefixLists_prefixListIds,
    describeManagedPrefixLists_dryRun,
    describeManagedPrefixLists_maxResults,
    describeManagedPrefixLists_filters,

    -- * Destructuring the Response
    DescribeManagedPrefixListsResponse (..),
    newDescribeManagedPrefixListsResponse,

    -- * Response Lenses
    describeManagedPrefixListsResponse_nextToken,
    describeManagedPrefixListsResponse_prefixLists,
    describeManagedPrefixListsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeManagedPrefixLists' smart constructor.
data DescribeManagedPrefixLists = DescribeManagedPrefixLists'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | One or more prefix list IDs.
    prefixListIds :: Core.Maybe [Core.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more filters.
    --
    -- -   @owner-id@ - The ID of the prefix list owner.
    --
    -- -   @prefix-list-id@ - The ID of the prefix list.
    --
    -- -   @prefix-list-name@ - The name of the prefix list.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeManagedPrefixLists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeManagedPrefixLists_nextToken' - The token for the next page of results.
--
-- 'prefixListIds', 'describeManagedPrefixLists_prefixListIds' - One or more prefix list IDs.
--
-- 'dryRun', 'describeManagedPrefixLists_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeManagedPrefixLists_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describeManagedPrefixLists_filters' - One or more filters.
--
-- -   @owner-id@ - The ID of the prefix list owner.
--
-- -   @prefix-list-id@ - The ID of the prefix list.
--
-- -   @prefix-list-name@ - The name of the prefix list.
newDescribeManagedPrefixLists ::
  DescribeManagedPrefixLists
newDescribeManagedPrefixLists =
  DescribeManagedPrefixLists'
    { nextToken =
        Core.Nothing,
      prefixListIds = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next page of results.
describeManagedPrefixLists_nextToken :: Lens.Lens' DescribeManagedPrefixLists (Core.Maybe Core.Text)
describeManagedPrefixLists_nextToken = Lens.lens (\DescribeManagedPrefixLists' {nextToken} -> nextToken) (\s@DescribeManagedPrefixLists' {} a -> s {nextToken = a} :: DescribeManagedPrefixLists)

-- | One or more prefix list IDs.
describeManagedPrefixLists_prefixListIds :: Lens.Lens' DescribeManagedPrefixLists (Core.Maybe [Core.Text])
describeManagedPrefixLists_prefixListIds = Lens.lens (\DescribeManagedPrefixLists' {prefixListIds} -> prefixListIds) (\s@DescribeManagedPrefixLists' {} a -> s {prefixListIds = a} :: DescribeManagedPrefixLists) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeManagedPrefixLists_dryRun :: Lens.Lens' DescribeManagedPrefixLists (Core.Maybe Core.Bool)
describeManagedPrefixLists_dryRun = Lens.lens (\DescribeManagedPrefixLists' {dryRun} -> dryRun) (\s@DescribeManagedPrefixLists' {} a -> s {dryRun = a} :: DescribeManagedPrefixLists)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeManagedPrefixLists_maxResults :: Lens.Lens' DescribeManagedPrefixLists (Core.Maybe Core.Natural)
describeManagedPrefixLists_maxResults = Lens.lens (\DescribeManagedPrefixLists' {maxResults} -> maxResults) (\s@DescribeManagedPrefixLists' {} a -> s {maxResults = a} :: DescribeManagedPrefixLists)

-- | One or more filters.
--
-- -   @owner-id@ - The ID of the prefix list owner.
--
-- -   @prefix-list-id@ - The ID of the prefix list.
--
-- -   @prefix-list-name@ - The name of the prefix list.
describeManagedPrefixLists_filters :: Lens.Lens' DescribeManagedPrefixLists (Core.Maybe [Filter])
describeManagedPrefixLists_filters = Lens.lens (\DescribeManagedPrefixLists' {filters} -> filters) (\s@DescribeManagedPrefixLists' {} a -> s {filters = a} :: DescribeManagedPrefixLists) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeManagedPrefixLists where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeManagedPrefixListsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeManagedPrefixListsResponse_prefixLists
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeManagedPrefixLists_nextToken
          Lens..~ rs
          Lens.^? describeManagedPrefixListsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeManagedPrefixLists where
  type
    AWSResponse DescribeManagedPrefixLists =
      DescribeManagedPrefixListsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeManagedPrefixListsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "prefixListSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeManagedPrefixLists

instance Core.NFData DescribeManagedPrefixLists

instance Core.ToHeaders DescribeManagedPrefixLists where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeManagedPrefixLists where
  toPath = Core.const "/"

instance Core.ToQuery DescribeManagedPrefixLists where
  toQuery DescribeManagedPrefixLists' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeManagedPrefixLists" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "PrefixListId"
              Core.<$> prefixListIds
          ),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeManagedPrefixListsResponse' smart constructor.
data DescribeManagedPrefixListsResponse = DescribeManagedPrefixListsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the prefix lists.
    prefixLists :: Core.Maybe [ManagedPrefixList],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeManagedPrefixListsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeManagedPrefixListsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'prefixLists', 'describeManagedPrefixListsResponse_prefixLists' - Information about the prefix lists.
--
-- 'httpStatus', 'describeManagedPrefixListsResponse_httpStatus' - The response's http status code.
newDescribeManagedPrefixListsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeManagedPrefixListsResponse
newDescribeManagedPrefixListsResponse pHttpStatus_ =
  DescribeManagedPrefixListsResponse'
    { nextToken =
        Core.Nothing,
      prefixLists = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeManagedPrefixListsResponse_nextToken :: Lens.Lens' DescribeManagedPrefixListsResponse (Core.Maybe Core.Text)
describeManagedPrefixListsResponse_nextToken = Lens.lens (\DescribeManagedPrefixListsResponse' {nextToken} -> nextToken) (\s@DescribeManagedPrefixListsResponse' {} a -> s {nextToken = a} :: DescribeManagedPrefixListsResponse)

-- | Information about the prefix lists.
describeManagedPrefixListsResponse_prefixLists :: Lens.Lens' DescribeManagedPrefixListsResponse (Core.Maybe [ManagedPrefixList])
describeManagedPrefixListsResponse_prefixLists = Lens.lens (\DescribeManagedPrefixListsResponse' {prefixLists} -> prefixLists) (\s@DescribeManagedPrefixListsResponse' {} a -> s {prefixLists = a} :: DescribeManagedPrefixListsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeManagedPrefixListsResponse_httpStatus :: Lens.Lens' DescribeManagedPrefixListsResponse Core.Int
describeManagedPrefixListsResponse_httpStatus = Lens.lens (\DescribeManagedPrefixListsResponse' {httpStatus} -> httpStatus) (\s@DescribeManagedPrefixListsResponse' {} a -> s {httpStatus = a} :: DescribeManagedPrefixListsResponse)

instance
  Core.NFData
    DescribeManagedPrefixListsResponse
