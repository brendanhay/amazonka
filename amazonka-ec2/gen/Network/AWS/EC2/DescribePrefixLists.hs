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
-- Module      : Network.AWS.EC2.DescribePrefixLists
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes available AWS services in a prefix list format, which includes
-- the prefix list name and prefix list ID of the service and the IP
-- address range for the service.
--
-- We recommend that you use DescribeManagedPrefixLists instead.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribePrefixLists
  ( -- * Creating a Request
    DescribePrefixLists (..),
    newDescribePrefixLists,

    -- * Request Lenses
    describePrefixLists_nextToken,
    describePrefixLists_prefixListIds,
    describePrefixLists_dryRun,
    describePrefixLists_maxResults,
    describePrefixLists_filters,

    -- * Destructuring the Response
    DescribePrefixListsResponse (..),
    newDescribePrefixListsResponse,

    -- * Response Lenses
    describePrefixListsResponse_nextToken,
    describePrefixListsResponse_prefixLists,
    describePrefixListsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePrefixLists' smart constructor.
data DescribePrefixLists = DescribePrefixLists'
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
    maxResults :: Core.Maybe Core.Int,
    -- | One or more filters.
    --
    -- -   @prefix-list-id@: The ID of a prefix list.
    --
    -- -   @prefix-list-name@: The name of a prefix list.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePrefixLists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePrefixLists_nextToken' - The token for the next page of results.
--
-- 'prefixListIds', 'describePrefixLists_prefixListIds' - One or more prefix list IDs.
--
-- 'dryRun', 'describePrefixLists_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describePrefixLists_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describePrefixLists_filters' - One or more filters.
--
-- -   @prefix-list-id@: The ID of a prefix list.
--
-- -   @prefix-list-name@: The name of a prefix list.
newDescribePrefixLists ::
  DescribePrefixLists
newDescribePrefixLists =
  DescribePrefixLists'
    { nextToken = Core.Nothing,
      prefixListIds = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next page of results.
describePrefixLists_nextToken :: Lens.Lens' DescribePrefixLists (Core.Maybe Core.Text)
describePrefixLists_nextToken = Lens.lens (\DescribePrefixLists' {nextToken} -> nextToken) (\s@DescribePrefixLists' {} a -> s {nextToken = a} :: DescribePrefixLists)

-- | One or more prefix list IDs.
describePrefixLists_prefixListIds :: Lens.Lens' DescribePrefixLists (Core.Maybe [Core.Text])
describePrefixLists_prefixListIds = Lens.lens (\DescribePrefixLists' {prefixListIds} -> prefixListIds) (\s@DescribePrefixLists' {} a -> s {prefixListIds = a} :: DescribePrefixLists) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describePrefixLists_dryRun :: Lens.Lens' DescribePrefixLists (Core.Maybe Core.Bool)
describePrefixLists_dryRun = Lens.lens (\DescribePrefixLists' {dryRun} -> dryRun) (\s@DescribePrefixLists' {} a -> s {dryRun = a} :: DescribePrefixLists)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describePrefixLists_maxResults :: Lens.Lens' DescribePrefixLists (Core.Maybe Core.Int)
describePrefixLists_maxResults = Lens.lens (\DescribePrefixLists' {maxResults} -> maxResults) (\s@DescribePrefixLists' {} a -> s {maxResults = a} :: DescribePrefixLists)

-- | One or more filters.
--
-- -   @prefix-list-id@: The ID of a prefix list.
--
-- -   @prefix-list-name@: The name of a prefix list.
describePrefixLists_filters :: Lens.Lens' DescribePrefixLists (Core.Maybe [Filter])
describePrefixLists_filters = Lens.lens (\DescribePrefixLists' {filters} -> filters) (\s@DescribePrefixLists' {} a -> s {filters = a} :: DescribePrefixLists) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribePrefixLists where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePrefixListsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describePrefixListsResponse_prefixLists
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describePrefixLists_nextToken
          Lens..~ rs
          Lens.^? describePrefixListsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribePrefixLists where
  type
    AWSResponse DescribePrefixLists =
      DescribePrefixListsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribePrefixListsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "prefixListSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePrefixLists

instance Core.NFData DescribePrefixLists

instance Core.ToHeaders DescribePrefixLists where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribePrefixLists where
  toPath = Core.const "/"

instance Core.ToQuery DescribePrefixLists where
  toQuery DescribePrefixLists' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribePrefixLists" :: Core.ByteString),
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

-- | /See:/ 'newDescribePrefixListsResponse' smart constructor.
data DescribePrefixListsResponse = DescribePrefixListsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | All available prefix lists.
    prefixLists :: Core.Maybe [PrefixList],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePrefixListsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePrefixListsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'prefixLists', 'describePrefixListsResponse_prefixLists' - All available prefix lists.
--
-- 'httpStatus', 'describePrefixListsResponse_httpStatus' - The response's http status code.
newDescribePrefixListsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePrefixListsResponse
newDescribePrefixListsResponse pHttpStatus_ =
  DescribePrefixListsResponse'
    { nextToken =
        Core.Nothing,
      prefixLists = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describePrefixListsResponse_nextToken :: Lens.Lens' DescribePrefixListsResponse (Core.Maybe Core.Text)
describePrefixListsResponse_nextToken = Lens.lens (\DescribePrefixListsResponse' {nextToken} -> nextToken) (\s@DescribePrefixListsResponse' {} a -> s {nextToken = a} :: DescribePrefixListsResponse)

-- | All available prefix lists.
describePrefixListsResponse_prefixLists :: Lens.Lens' DescribePrefixListsResponse (Core.Maybe [PrefixList])
describePrefixListsResponse_prefixLists = Lens.lens (\DescribePrefixListsResponse' {prefixLists} -> prefixLists) (\s@DescribePrefixListsResponse' {} a -> s {prefixLists = a} :: DescribePrefixListsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describePrefixListsResponse_httpStatus :: Lens.Lens' DescribePrefixListsResponse Core.Int
describePrefixListsResponse_httpStatus = Lens.lens (\DescribePrefixListsResponse' {httpStatus} -> httpStatus) (\s@DescribePrefixListsResponse' {} a -> s {httpStatus = a} :: DescribePrefixListsResponse)

instance Core.NFData DescribePrefixListsResponse
