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
-- Describes your managed prefix lists and any Amazon Web Services-managed
-- prefix lists.
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
    describeManagedPrefixLists_prefixListIds,
    describeManagedPrefixLists_nextToken,
    describeManagedPrefixLists_maxResults,
    describeManagedPrefixLists_dryRun,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeManagedPrefixLists' smart constructor.
data DescribeManagedPrefixLists = DescribeManagedPrefixLists'
  { -- | One or more prefix list IDs.
    prefixListIds :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @owner-id@ - The ID of the prefix list owner.
    --
    -- -   @prefix-list-id@ - The ID of the prefix list.
    --
    -- -   @prefix-list-name@ - The name of the prefix list.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeManagedPrefixLists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefixListIds', 'describeManagedPrefixLists_prefixListIds' - One or more prefix list IDs.
--
-- 'nextToken', 'describeManagedPrefixLists_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'describeManagedPrefixLists_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'dryRun', 'describeManagedPrefixLists_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
    { prefixListIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | One or more prefix list IDs.
describeManagedPrefixLists_prefixListIds :: Lens.Lens' DescribeManagedPrefixLists (Prelude.Maybe [Prelude.Text])
describeManagedPrefixLists_prefixListIds = Lens.lens (\DescribeManagedPrefixLists' {prefixListIds} -> prefixListIds) (\s@DescribeManagedPrefixLists' {} a -> s {prefixListIds = a} :: DescribeManagedPrefixLists) Prelude.. Lens.mapping Lens._Coerce

-- | The token for the next page of results.
describeManagedPrefixLists_nextToken :: Lens.Lens' DescribeManagedPrefixLists (Prelude.Maybe Prelude.Text)
describeManagedPrefixLists_nextToken = Lens.lens (\DescribeManagedPrefixLists' {nextToken} -> nextToken) (\s@DescribeManagedPrefixLists' {} a -> s {nextToken = a} :: DescribeManagedPrefixLists)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeManagedPrefixLists_maxResults :: Lens.Lens' DescribeManagedPrefixLists (Prelude.Maybe Prelude.Natural)
describeManagedPrefixLists_maxResults = Lens.lens (\DescribeManagedPrefixLists' {maxResults} -> maxResults) (\s@DescribeManagedPrefixLists' {} a -> s {maxResults = a} :: DescribeManagedPrefixLists)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeManagedPrefixLists_dryRun :: Lens.Lens' DescribeManagedPrefixLists (Prelude.Maybe Prelude.Bool)
describeManagedPrefixLists_dryRun = Lens.lens (\DescribeManagedPrefixLists' {dryRun} -> dryRun) (\s@DescribeManagedPrefixLists' {} a -> s {dryRun = a} :: DescribeManagedPrefixLists)

-- | One or more filters.
--
-- -   @owner-id@ - The ID of the prefix list owner.
--
-- -   @prefix-list-id@ - The ID of the prefix list.
--
-- -   @prefix-list-name@ - The name of the prefix list.
describeManagedPrefixLists_filters :: Lens.Lens' DescribeManagedPrefixLists (Prelude.Maybe [Filter])
describeManagedPrefixLists_filters = Lens.lens (\DescribeManagedPrefixLists' {filters} -> filters) (\s@DescribeManagedPrefixLists' {} a -> s {filters = a} :: DescribeManagedPrefixLists) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeManagedPrefixLists where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeManagedPrefixListsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeManagedPrefixListsResponse_prefixLists
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeManagedPrefixLists_nextToken
          Lens..~ rs
          Lens.^? describeManagedPrefixListsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeManagedPrefixLists where
  type
    AWSResponse DescribeManagedPrefixLists =
      DescribeManagedPrefixListsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeManagedPrefixListsResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "prefixListSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeManagedPrefixLists

instance Prelude.NFData DescribeManagedPrefixLists

instance Core.ToHeaders DescribeManagedPrefixLists where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeManagedPrefixLists where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeManagedPrefixLists where
  toQuery DescribeManagedPrefixLists' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeManagedPrefixLists" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "PrefixListId"
              Prelude.<$> prefixListIds
          ),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeManagedPrefixListsResponse' smart constructor.
data DescribeManagedPrefixListsResponse = DescribeManagedPrefixListsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the prefix lists.
    prefixLists :: Prelude.Maybe [ManagedPrefixList],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeManagedPrefixListsResponse
newDescribeManagedPrefixListsResponse pHttpStatus_ =
  DescribeManagedPrefixListsResponse'
    { nextToken =
        Prelude.Nothing,
      prefixLists = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeManagedPrefixListsResponse_nextToken :: Lens.Lens' DescribeManagedPrefixListsResponse (Prelude.Maybe Prelude.Text)
describeManagedPrefixListsResponse_nextToken = Lens.lens (\DescribeManagedPrefixListsResponse' {nextToken} -> nextToken) (\s@DescribeManagedPrefixListsResponse' {} a -> s {nextToken = a} :: DescribeManagedPrefixListsResponse)

-- | Information about the prefix lists.
describeManagedPrefixListsResponse_prefixLists :: Lens.Lens' DescribeManagedPrefixListsResponse (Prelude.Maybe [ManagedPrefixList])
describeManagedPrefixListsResponse_prefixLists = Lens.lens (\DescribeManagedPrefixListsResponse' {prefixLists} -> prefixLists) (\s@DescribeManagedPrefixListsResponse' {} a -> s {prefixLists = a} :: DescribeManagedPrefixListsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeManagedPrefixListsResponse_httpStatus :: Lens.Lens' DescribeManagedPrefixListsResponse Prelude.Int
describeManagedPrefixListsResponse_httpStatus = Lens.lens (\DescribeManagedPrefixListsResponse' {httpStatus} -> httpStatus) (\s@DescribeManagedPrefixListsResponse' {} a -> s {httpStatus = a} :: DescribeManagedPrefixListsResponse)

instance
  Prelude.NFData
    DescribeManagedPrefixListsResponse
