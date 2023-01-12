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
-- Module      : Amazonka.EC2.DescribePrefixLists
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes available Amazon Web Services services in a prefix list
-- format, which includes the prefix list name and prefix list ID of the
-- service and the IP address range for the service.
--
-- We recommend that you use DescribeManagedPrefixLists instead.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribePrefixLists
  ( -- * Creating a Request
    DescribePrefixLists (..),
    newDescribePrefixLists,

    -- * Request Lenses
    describePrefixLists_dryRun,
    describePrefixLists_filters,
    describePrefixLists_maxResults,
    describePrefixLists_nextToken,
    describePrefixLists_prefixListIds,

    -- * Destructuring the Response
    DescribePrefixListsResponse (..),
    newDescribePrefixListsResponse,

    -- * Response Lenses
    describePrefixListsResponse_nextToken,
    describePrefixListsResponse_prefixLists,
    describePrefixListsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePrefixLists' smart constructor.
data DescribePrefixLists = DescribePrefixLists'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @prefix-list-id@: The ID of a prefix list.
    --
    -- -   @prefix-list-name@: The name of a prefix list.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more prefix list IDs.
    prefixListIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePrefixLists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describePrefixLists_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describePrefixLists_filters' - One or more filters.
--
-- -   @prefix-list-id@: The ID of a prefix list.
--
-- -   @prefix-list-name@: The name of a prefix list.
--
-- 'maxResults', 'describePrefixLists_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describePrefixLists_nextToken' - The token for the next page of results.
--
-- 'prefixListIds', 'describePrefixLists_prefixListIds' - One or more prefix list IDs.
newDescribePrefixLists ::
  DescribePrefixLists
newDescribePrefixLists =
  DescribePrefixLists'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      prefixListIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describePrefixLists_dryRun :: Lens.Lens' DescribePrefixLists (Prelude.Maybe Prelude.Bool)
describePrefixLists_dryRun = Lens.lens (\DescribePrefixLists' {dryRun} -> dryRun) (\s@DescribePrefixLists' {} a -> s {dryRun = a} :: DescribePrefixLists)

-- | One or more filters.
--
-- -   @prefix-list-id@: The ID of a prefix list.
--
-- -   @prefix-list-name@: The name of a prefix list.
describePrefixLists_filters :: Lens.Lens' DescribePrefixLists (Prelude.Maybe [Filter])
describePrefixLists_filters = Lens.lens (\DescribePrefixLists' {filters} -> filters) (\s@DescribePrefixLists' {} a -> s {filters = a} :: DescribePrefixLists) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describePrefixLists_maxResults :: Lens.Lens' DescribePrefixLists (Prelude.Maybe Prelude.Int)
describePrefixLists_maxResults = Lens.lens (\DescribePrefixLists' {maxResults} -> maxResults) (\s@DescribePrefixLists' {} a -> s {maxResults = a} :: DescribePrefixLists)

-- | The token for the next page of results.
describePrefixLists_nextToken :: Lens.Lens' DescribePrefixLists (Prelude.Maybe Prelude.Text)
describePrefixLists_nextToken = Lens.lens (\DescribePrefixLists' {nextToken} -> nextToken) (\s@DescribePrefixLists' {} a -> s {nextToken = a} :: DescribePrefixLists)

-- | One or more prefix list IDs.
describePrefixLists_prefixListIds :: Lens.Lens' DescribePrefixLists (Prelude.Maybe [Prelude.Text])
describePrefixLists_prefixListIds = Lens.lens (\DescribePrefixLists' {prefixListIds} -> prefixListIds) (\s@DescribePrefixLists' {} a -> s {prefixListIds = a} :: DescribePrefixLists) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribePrefixLists where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePrefixListsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describePrefixListsResponse_prefixLists
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describePrefixLists_nextToken
          Lens..~ rs
          Lens.^? describePrefixListsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribePrefixLists where
  type
    AWSResponse DescribePrefixLists =
      DescribePrefixListsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribePrefixListsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "prefixListSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePrefixLists where
  hashWithSalt _salt DescribePrefixLists' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` prefixListIds

instance Prelude.NFData DescribePrefixLists where
  rnf DescribePrefixLists' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf prefixListIds

instance Data.ToHeaders DescribePrefixLists where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribePrefixLists where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePrefixLists where
  toQuery DescribePrefixLists' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribePrefixLists" :: Prelude.ByteString),
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

-- | /See:/ 'newDescribePrefixListsResponse' smart constructor.
data DescribePrefixListsResponse = DescribePrefixListsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | All available prefix lists.
    prefixLists :: Prelude.Maybe [PrefixList],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribePrefixListsResponse
newDescribePrefixListsResponse pHttpStatus_ =
  DescribePrefixListsResponse'
    { nextToken =
        Prelude.Nothing,
      prefixLists = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describePrefixListsResponse_nextToken :: Lens.Lens' DescribePrefixListsResponse (Prelude.Maybe Prelude.Text)
describePrefixListsResponse_nextToken = Lens.lens (\DescribePrefixListsResponse' {nextToken} -> nextToken) (\s@DescribePrefixListsResponse' {} a -> s {nextToken = a} :: DescribePrefixListsResponse)

-- | All available prefix lists.
describePrefixListsResponse_prefixLists :: Lens.Lens' DescribePrefixListsResponse (Prelude.Maybe [PrefixList])
describePrefixListsResponse_prefixLists = Lens.lens (\DescribePrefixListsResponse' {prefixLists} -> prefixLists) (\s@DescribePrefixListsResponse' {} a -> s {prefixLists = a} :: DescribePrefixListsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePrefixListsResponse_httpStatus :: Lens.Lens' DescribePrefixListsResponse Prelude.Int
describePrefixListsResponse_httpStatus = Lens.lens (\DescribePrefixListsResponse' {httpStatus} -> httpStatus) (\s@DescribePrefixListsResponse' {} a -> s {httpStatus = a} :: DescribePrefixListsResponse)

instance Prelude.NFData DescribePrefixListsResponse where
  rnf DescribePrefixListsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf prefixLists
      `Prelude.seq` Prelude.rnf httpStatus
