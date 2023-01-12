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
-- Module      : Amazonka.EC2.DescribeVerifiedAccessGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe details of existing Verified Access groups.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVerifiedAccessGroups
  ( -- * Creating a Request
    DescribeVerifiedAccessGroups (..),
    newDescribeVerifiedAccessGroups,

    -- * Request Lenses
    describeVerifiedAccessGroups_dryRun,
    describeVerifiedAccessGroups_filters,
    describeVerifiedAccessGroups_maxResults,
    describeVerifiedAccessGroups_nextToken,
    describeVerifiedAccessGroups_verifiedAccessGroupIds,
    describeVerifiedAccessGroups_verifiedAccessInstanceId,

    -- * Destructuring the Response
    DescribeVerifiedAccessGroupsResponse (..),
    newDescribeVerifiedAccessGroupsResponse,

    -- * Response Lenses
    describeVerifiedAccessGroupsResponse_nextToken,
    describeVerifiedAccessGroupsResponse_verifiedAccessGroups,
    describeVerifiedAccessGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVerifiedAccessGroups' smart constructor.
data DescribeVerifiedAccessGroups = DescribeVerifiedAccessGroups'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters. Filter names and values are case-sensitive.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services Verified Access groups.
    verifiedAccessGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVerifiedAccessGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVerifiedAccessGroups_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeVerifiedAccessGroups_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- 'maxResults', 'describeVerifiedAccessGroups_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeVerifiedAccessGroups_nextToken' - The token for the next page of results.
--
-- 'verifiedAccessGroupIds', 'describeVerifiedAccessGroups_verifiedAccessGroupIds' - The ID of the Amazon Web Services Verified Access groups.
--
-- 'verifiedAccessInstanceId', 'describeVerifiedAccessGroups_verifiedAccessInstanceId' - The ID of the Amazon Web Services Verified Access instance.
newDescribeVerifiedAccessGroups ::
  DescribeVerifiedAccessGroups
newDescribeVerifiedAccessGroups =
  DescribeVerifiedAccessGroups'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      verifiedAccessGroupIds = Prelude.Nothing,
      verifiedAccessInstanceId = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVerifiedAccessGroups_dryRun :: Lens.Lens' DescribeVerifiedAccessGroups (Prelude.Maybe Prelude.Bool)
describeVerifiedAccessGroups_dryRun = Lens.lens (\DescribeVerifiedAccessGroups' {dryRun} -> dryRun) (\s@DescribeVerifiedAccessGroups' {} a -> s {dryRun = a} :: DescribeVerifiedAccessGroups)

-- | One or more filters. Filter names and values are case-sensitive.
describeVerifiedAccessGroups_filters :: Lens.Lens' DescribeVerifiedAccessGroups (Prelude.Maybe [Filter])
describeVerifiedAccessGroups_filters = Lens.lens (\DescribeVerifiedAccessGroups' {filters} -> filters) (\s@DescribeVerifiedAccessGroups' {} a -> s {filters = a} :: DescribeVerifiedAccessGroups) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeVerifiedAccessGroups_maxResults :: Lens.Lens' DescribeVerifiedAccessGroups (Prelude.Maybe Prelude.Natural)
describeVerifiedAccessGroups_maxResults = Lens.lens (\DescribeVerifiedAccessGroups' {maxResults} -> maxResults) (\s@DescribeVerifiedAccessGroups' {} a -> s {maxResults = a} :: DescribeVerifiedAccessGroups)

-- | The token for the next page of results.
describeVerifiedAccessGroups_nextToken :: Lens.Lens' DescribeVerifiedAccessGroups (Prelude.Maybe Prelude.Text)
describeVerifiedAccessGroups_nextToken = Lens.lens (\DescribeVerifiedAccessGroups' {nextToken} -> nextToken) (\s@DescribeVerifiedAccessGroups' {} a -> s {nextToken = a} :: DescribeVerifiedAccessGroups)

-- | The ID of the Amazon Web Services Verified Access groups.
describeVerifiedAccessGroups_verifiedAccessGroupIds :: Lens.Lens' DescribeVerifiedAccessGroups (Prelude.Maybe [Prelude.Text])
describeVerifiedAccessGroups_verifiedAccessGroupIds = Lens.lens (\DescribeVerifiedAccessGroups' {verifiedAccessGroupIds} -> verifiedAccessGroupIds) (\s@DescribeVerifiedAccessGroups' {} a -> s {verifiedAccessGroupIds = a} :: DescribeVerifiedAccessGroups) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services Verified Access instance.
describeVerifiedAccessGroups_verifiedAccessInstanceId :: Lens.Lens' DescribeVerifiedAccessGroups (Prelude.Maybe Prelude.Text)
describeVerifiedAccessGroups_verifiedAccessInstanceId = Lens.lens (\DescribeVerifiedAccessGroups' {verifiedAccessInstanceId} -> verifiedAccessInstanceId) (\s@DescribeVerifiedAccessGroups' {} a -> s {verifiedAccessInstanceId = a} :: DescribeVerifiedAccessGroups)

instance Core.AWSPager DescribeVerifiedAccessGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVerifiedAccessGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVerifiedAccessGroupsResponse_verifiedAccessGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeVerifiedAccessGroups_nextToken
          Lens..~ rs
          Lens.^? describeVerifiedAccessGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeVerifiedAccessGroups where
  type
    AWSResponse DescribeVerifiedAccessGroups =
      DescribeVerifiedAccessGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVerifiedAccessGroupsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "verifiedAccessGroupSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVerifiedAccessGroups
  where
  hashWithSalt _salt DescribeVerifiedAccessGroups' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` verifiedAccessGroupIds
      `Prelude.hashWithSalt` verifiedAccessInstanceId

instance Prelude.NFData DescribeVerifiedAccessGroups where
  rnf DescribeVerifiedAccessGroups' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf verifiedAccessGroupIds
      `Prelude.seq` Prelude.rnf verifiedAccessInstanceId

instance Data.ToHeaders DescribeVerifiedAccessGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeVerifiedAccessGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVerifiedAccessGroups where
  toQuery DescribeVerifiedAccessGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeVerifiedAccessGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "VerifiedAccessGroupId"
              Prelude.<$> verifiedAccessGroupIds
          ),
        "VerifiedAccessInstanceId"
          Data.=: verifiedAccessInstanceId
      ]

-- | /See:/ 'newDescribeVerifiedAccessGroupsResponse' smart constructor.
data DescribeVerifiedAccessGroupsResponse = DescribeVerifiedAccessGroupsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Verified Access group.
    verifiedAccessGroups :: Prelude.Maybe [VerifiedAccessGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVerifiedAccessGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVerifiedAccessGroupsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'verifiedAccessGroups', 'describeVerifiedAccessGroupsResponse_verifiedAccessGroups' - The ID of the Verified Access group.
--
-- 'httpStatus', 'describeVerifiedAccessGroupsResponse_httpStatus' - The response's http status code.
newDescribeVerifiedAccessGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVerifiedAccessGroupsResponse
newDescribeVerifiedAccessGroupsResponse pHttpStatus_ =
  DescribeVerifiedAccessGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      verifiedAccessGroups =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVerifiedAccessGroupsResponse_nextToken :: Lens.Lens' DescribeVerifiedAccessGroupsResponse (Prelude.Maybe Prelude.Text)
describeVerifiedAccessGroupsResponse_nextToken = Lens.lens (\DescribeVerifiedAccessGroupsResponse' {nextToken} -> nextToken) (\s@DescribeVerifiedAccessGroupsResponse' {} a -> s {nextToken = a} :: DescribeVerifiedAccessGroupsResponse)

-- | The ID of the Verified Access group.
describeVerifiedAccessGroupsResponse_verifiedAccessGroups :: Lens.Lens' DescribeVerifiedAccessGroupsResponse (Prelude.Maybe [VerifiedAccessGroup])
describeVerifiedAccessGroupsResponse_verifiedAccessGroups = Lens.lens (\DescribeVerifiedAccessGroupsResponse' {verifiedAccessGroups} -> verifiedAccessGroups) (\s@DescribeVerifiedAccessGroupsResponse' {} a -> s {verifiedAccessGroups = a} :: DescribeVerifiedAccessGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVerifiedAccessGroupsResponse_httpStatus :: Lens.Lens' DescribeVerifiedAccessGroupsResponse Prelude.Int
describeVerifiedAccessGroupsResponse_httpStatus = Lens.lens (\DescribeVerifiedAccessGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeVerifiedAccessGroupsResponse' {} a -> s {httpStatus = a} :: DescribeVerifiedAccessGroupsResponse)

instance
  Prelude.NFData
    DescribeVerifiedAccessGroupsResponse
  where
  rnf DescribeVerifiedAccessGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf verifiedAccessGroups
      `Prelude.seq` Prelude.rnf httpStatus
