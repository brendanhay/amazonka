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
-- Module      : Amazonka.EC2.DescribeManagedPrefixLists
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.EC2.DescribeManagedPrefixLists
  ( -- * Creating a Request
    DescribeManagedPrefixLists (..),
    newDescribeManagedPrefixLists,

    -- * Request Lenses
    describeManagedPrefixLists_dryRun,
    describeManagedPrefixLists_filters,
    describeManagedPrefixLists_maxResults,
    describeManagedPrefixLists_nextToken,
    describeManagedPrefixLists_prefixListIds,

    -- * Destructuring the Response
    DescribeManagedPrefixListsResponse (..),
    newDescribeManagedPrefixListsResponse,

    -- * Response Lenses
    describeManagedPrefixListsResponse_nextToken,
    describeManagedPrefixListsResponse_prefixLists,
    describeManagedPrefixListsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeManagedPrefixLists' smart constructor.
data DescribeManagedPrefixLists = DescribeManagedPrefixLists'
  { -- | Checks whether you have the required permissions for the action, without
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
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more prefix list IDs.
    prefixListIds :: Prelude.Maybe [Prelude.Text]
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
--
-- 'maxResults', 'describeManagedPrefixLists_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeManagedPrefixLists_nextToken' - The token for the next page of results.
--
-- 'prefixListIds', 'describeManagedPrefixLists_prefixListIds' - One or more prefix list IDs.
newDescribeManagedPrefixLists ::
  DescribeManagedPrefixLists
newDescribeManagedPrefixLists =
  DescribeManagedPrefixLists'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      prefixListIds = Prelude.Nothing
    }

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
describeManagedPrefixLists_filters = Lens.lens (\DescribeManagedPrefixLists' {filters} -> filters) (\s@DescribeManagedPrefixLists' {} a -> s {filters = a} :: DescribeManagedPrefixLists) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeManagedPrefixLists_maxResults :: Lens.Lens' DescribeManagedPrefixLists (Prelude.Maybe Prelude.Natural)
describeManagedPrefixLists_maxResults = Lens.lens (\DescribeManagedPrefixLists' {maxResults} -> maxResults) (\s@DescribeManagedPrefixLists' {} a -> s {maxResults = a} :: DescribeManagedPrefixLists)

-- | The token for the next page of results.
describeManagedPrefixLists_nextToken :: Lens.Lens' DescribeManagedPrefixLists (Prelude.Maybe Prelude.Text)
describeManagedPrefixLists_nextToken = Lens.lens (\DescribeManagedPrefixLists' {nextToken} -> nextToken) (\s@DescribeManagedPrefixLists' {} a -> s {nextToken = a} :: DescribeManagedPrefixLists)

-- | One or more prefix list IDs.
describeManagedPrefixLists_prefixListIds :: Lens.Lens' DescribeManagedPrefixLists (Prelude.Maybe [Prelude.Text])
describeManagedPrefixLists_prefixListIds = Lens.lens (\DescribeManagedPrefixLists' {prefixListIds} -> prefixListIds) (\s@DescribeManagedPrefixLists' {} a -> s {prefixListIds = a} :: DescribeManagedPrefixLists) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeManagedPrefixListsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "prefixListSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeManagedPrefixLists where
  hashWithSalt _salt DescribeManagedPrefixLists' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` prefixListIds

instance Prelude.NFData DescribeManagedPrefixLists where
  rnf DescribeManagedPrefixLists' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf filters `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf prefixListIds

instance Data.ToHeaders DescribeManagedPrefixLists where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeManagedPrefixLists where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeManagedPrefixLists where
  toQuery DescribeManagedPrefixLists' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeManagedPrefixLists" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "PrefixListId"
              Prelude.<$> prefixListIds
          )
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
describeManagedPrefixListsResponse_prefixLists = Lens.lens (\DescribeManagedPrefixListsResponse' {prefixLists} -> prefixLists) (\s@DescribeManagedPrefixListsResponse' {} a -> s {prefixLists = a} :: DescribeManagedPrefixListsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeManagedPrefixListsResponse_httpStatus :: Lens.Lens' DescribeManagedPrefixListsResponse Prelude.Int
describeManagedPrefixListsResponse_httpStatus = Lens.lens (\DescribeManagedPrefixListsResponse' {httpStatus} -> httpStatus) (\s@DescribeManagedPrefixListsResponse' {} a -> s {httpStatus = a} :: DescribeManagedPrefixListsResponse)

instance
  Prelude.NFData
    DescribeManagedPrefixListsResponse
  where
  rnf DescribeManagedPrefixListsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf prefixLists `Prelude.seq`
        Prelude.rnf httpStatus
