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
-- Module      : Amazonka.EC2.DescribeVerifiedAccessInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe Verified Access instances.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVerifiedAccessInstances
  ( -- * Creating a Request
    DescribeVerifiedAccessInstances (..),
    newDescribeVerifiedAccessInstances,

    -- * Request Lenses
    describeVerifiedAccessInstances_dryRun,
    describeVerifiedAccessInstances_filters,
    describeVerifiedAccessInstances_maxResults,
    describeVerifiedAccessInstances_nextToken,
    describeVerifiedAccessInstances_verifiedAccessInstanceIds,

    -- * Destructuring the Response
    DescribeVerifiedAccessInstancesResponse (..),
    newDescribeVerifiedAccessInstancesResponse,

    -- * Response Lenses
    describeVerifiedAccessInstancesResponse_nextToken,
    describeVerifiedAccessInstancesResponse_verifiedAccessInstances,
    describeVerifiedAccessInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVerifiedAccessInstances' smart constructor.
data DescribeVerifiedAccessInstances = DescribeVerifiedAccessInstances'
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
    -- | The IDs of the Amazon Web Services Verified Access instances.
    verifiedAccessInstanceIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVerifiedAccessInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVerifiedAccessInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeVerifiedAccessInstances_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- 'maxResults', 'describeVerifiedAccessInstances_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeVerifiedAccessInstances_nextToken' - The token for the next page of results.
--
-- 'verifiedAccessInstanceIds', 'describeVerifiedAccessInstances_verifiedAccessInstanceIds' - The IDs of the Amazon Web Services Verified Access instances.
newDescribeVerifiedAccessInstances ::
  DescribeVerifiedAccessInstances
newDescribeVerifiedAccessInstances =
  DescribeVerifiedAccessInstances'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      verifiedAccessInstanceIds =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVerifiedAccessInstances_dryRun :: Lens.Lens' DescribeVerifiedAccessInstances (Prelude.Maybe Prelude.Bool)
describeVerifiedAccessInstances_dryRun = Lens.lens (\DescribeVerifiedAccessInstances' {dryRun} -> dryRun) (\s@DescribeVerifiedAccessInstances' {} a -> s {dryRun = a} :: DescribeVerifiedAccessInstances)

-- | One or more filters. Filter names and values are case-sensitive.
describeVerifiedAccessInstances_filters :: Lens.Lens' DescribeVerifiedAccessInstances (Prelude.Maybe [Filter])
describeVerifiedAccessInstances_filters = Lens.lens (\DescribeVerifiedAccessInstances' {filters} -> filters) (\s@DescribeVerifiedAccessInstances' {} a -> s {filters = a} :: DescribeVerifiedAccessInstances) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeVerifiedAccessInstances_maxResults :: Lens.Lens' DescribeVerifiedAccessInstances (Prelude.Maybe Prelude.Natural)
describeVerifiedAccessInstances_maxResults = Lens.lens (\DescribeVerifiedAccessInstances' {maxResults} -> maxResults) (\s@DescribeVerifiedAccessInstances' {} a -> s {maxResults = a} :: DescribeVerifiedAccessInstances)

-- | The token for the next page of results.
describeVerifiedAccessInstances_nextToken :: Lens.Lens' DescribeVerifiedAccessInstances (Prelude.Maybe Prelude.Text)
describeVerifiedAccessInstances_nextToken = Lens.lens (\DescribeVerifiedAccessInstances' {nextToken} -> nextToken) (\s@DescribeVerifiedAccessInstances' {} a -> s {nextToken = a} :: DescribeVerifiedAccessInstances)

-- | The IDs of the Amazon Web Services Verified Access instances.
describeVerifiedAccessInstances_verifiedAccessInstanceIds :: Lens.Lens' DescribeVerifiedAccessInstances (Prelude.Maybe [Prelude.Text])
describeVerifiedAccessInstances_verifiedAccessInstanceIds = Lens.lens (\DescribeVerifiedAccessInstances' {verifiedAccessInstanceIds} -> verifiedAccessInstanceIds) (\s@DescribeVerifiedAccessInstances' {} a -> s {verifiedAccessInstanceIds = a} :: DescribeVerifiedAccessInstances) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeVerifiedAccessInstances
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVerifiedAccessInstancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVerifiedAccessInstancesResponse_verifiedAccessInstances
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeVerifiedAccessInstances_nextToken
          Lens..~ rs
          Lens.^? describeVerifiedAccessInstancesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeVerifiedAccessInstances
  where
  type
    AWSResponse DescribeVerifiedAccessInstances =
      DescribeVerifiedAccessInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVerifiedAccessInstancesResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "verifiedAccessInstanceSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVerifiedAccessInstances
  where
  hashWithSalt
    _salt
    DescribeVerifiedAccessInstances' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` verifiedAccessInstanceIds

instance
  Prelude.NFData
    DescribeVerifiedAccessInstances
  where
  rnf DescribeVerifiedAccessInstances' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf verifiedAccessInstanceIds

instance
  Data.ToHeaders
    DescribeVerifiedAccessInstances
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeVerifiedAccessInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVerifiedAccessInstances where
  toQuery DescribeVerifiedAccessInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeVerifiedAccessInstances" ::
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
          ( Data.toQueryList "VerifiedAccessInstanceId"
              Prelude.<$> verifiedAccessInstanceIds
          )
      ]

-- | /See:/ 'newDescribeVerifiedAccessInstancesResponse' smart constructor.
data DescribeVerifiedAccessInstancesResponse = DescribeVerifiedAccessInstancesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the Amazon Web Services Verified Access instances.
    verifiedAccessInstances :: Prelude.Maybe [VerifiedAccessInstance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVerifiedAccessInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVerifiedAccessInstancesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'verifiedAccessInstances', 'describeVerifiedAccessInstancesResponse_verifiedAccessInstances' - The IDs of the Amazon Web Services Verified Access instances.
--
-- 'httpStatus', 'describeVerifiedAccessInstancesResponse_httpStatus' - The response's http status code.
newDescribeVerifiedAccessInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVerifiedAccessInstancesResponse
newDescribeVerifiedAccessInstancesResponse
  pHttpStatus_ =
    DescribeVerifiedAccessInstancesResponse'
      { nextToken =
          Prelude.Nothing,
        verifiedAccessInstances =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVerifiedAccessInstancesResponse_nextToken :: Lens.Lens' DescribeVerifiedAccessInstancesResponse (Prelude.Maybe Prelude.Text)
describeVerifiedAccessInstancesResponse_nextToken = Lens.lens (\DescribeVerifiedAccessInstancesResponse' {nextToken} -> nextToken) (\s@DescribeVerifiedAccessInstancesResponse' {} a -> s {nextToken = a} :: DescribeVerifiedAccessInstancesResponse)

-- | The IDs of the Amazon Web Services Verified Access instances.
describeVerifiedAccessInstancesResponse_verifiedAccessInstances :: Lens.Lens' DescribeVerifiedAccessInstancesResponse (Prelude.Maybe [VerifiedAccessInstance])
describeVerifiedAccessInstancesResponse_verifiedAccessInstances = Lens.lens (\DescribeVerifiedAccessInstancesResponse' {verifiedAccessInstances} -> verifiedAccessInstances) (\s@DescribeVerifiedAccessInstancesResponse' {} a -> s {verifiedAccessInstances = a} :: DescribeVerifiedAccessInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVerifiedAccessInstancesResponse_httpStatus :: Lens.Lens' DescribeVerifiedAccessInstancesResponse Prelude.Int
describeVerifiedAccessInstancesResponse_httpStatus = Lens.lens (\DescribeVerifiedAccessInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeVerifiedAccessInstancesResponse' {} a -> s {httpStatus = a} :: DescribeVerifiedAccessInstancesResponse)

instance
  Prelude.NFData
    DescribeVerifiedAccessInstancesResponse
  where
  rnf DescribeVerifiedAccessInstancesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf verifiedAccessInstances
      `Prelude.seq` Prelude.rnf httpStatus
