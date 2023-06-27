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
-- Module      : Amazonka.EC2.DescribeIpamResourceDiscoveryAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes resource discovery association with an Amazon VPC IPAM. An
-- associated resource discovery is a resource discovery that has been
-- associated with an IPAM..
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeIpamResourceDiscoveryAssociations
  ( -- * Creating a Request
    DescribeIpamResourceDiscoveryAssociations (..),
    newDescribeIpamResourceDiscoveryAssociations,

    -- * Request Lenses
    describeIpamResourceDiscoveryAssociations_dryRun,
    describeIpamResourceDiscoveryAssociations_filters,
    describeIpamResourceDiscoveryAssociations_ipamResourceDiscoveryAssociationIds,
    describeIpamResourceDiscoveryAssociations_maxResults,
    describeIpamResourceDiscoveryAssociations_nextToken,

    -- * Destructuring the Response
    DescribeIpamResourceDiscoveryAssociationsResponse (..),
    newDescribeIpamResourceDiscoveryAssociationsResponse,

    -- * Response Lenses
    describeIpamResourceDiscoveryAssociationsResponse_ipamResourceDiscoveryAssociations,
    describeIpamResourceDiscoveryAssociationsResponse_nextToken,
    describeIpamResourceDiscoveryAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIpamResourceDiscoveryAssociations' smart constructor.
data DescribeIpamResourceDiscoveryAssociations = DescribeIpamResourceDiscoveryAssociations'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The resource discovery association filters.
    filters :: Prelude.Maybe [Filter],
    -- | The resource discovery association IDs.
    ipamResourceDiscoveryAssociationIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of resource discovery associations to return in one
    -- page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpamResourceDiscoveryAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeIpamResourceDiscoveryAssociations_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeIpamResourceDiscoveryAssociations_filters' - The resource discovery association filters.
--
-- 'ipamResourceDiscoveryAssociationIds', 'describeIpamResourceDiscoveryAssociations_ipamResourceDiscoveryAssociationIds' - The resource discovery association IDs.
--
-- 'maxResults', 'describeIpamResourceDiscoveryAssociations_maxResults' - The maximum number of resource discovery associations to return in one
-- page of results.
--
-- 'nextToken', 'describeIpamResourceDiscoveryAssociations_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
newDescribeIpamResourceDiscoveryAssociations ::
  DescribeIpamResourceDiscoveryAssociations
newDescribeIpamResourceDiscoveryAssociations =
  DescribeIpamResourceDiscoveryAssociations'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      ipamResourceDiscoveryAssociationIds =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
describeIpamResourceDiscoveryAssociations_dryRun :: Lens.Lens' DescribeIpamResourceDiscoveryAssociations (Prelude.Maybe Prelude.Bool)
describeIpamResourceDiscoveryAssociations_dryRun = Lens.lens (\DescribeIpamResourceDiscoveryAssociations' {dryRun} -> dryRun) (\s@DescribeIpamResourceDiscoveryAssociations' {} a -> s {dryRun = a} :: DescribeIpamResourceDiscoveryAssociations)

-- | The resource discovery association filters.
describeIpamResourceDiscoveryAssociations_filters :: Lens.Lens' DescribeIpamResourceDiscoveryAssociations (Prelude.Maybe [Filter])
describeIpamResourceDiscoveryAssociations_filters = Lens.lens (\DescribeIpamResourceDiscoveryAssociations' {filters} -> filters) (\s@DescribeIpamResourceDiscoveryAssociations' {} a -> s {filters = a} :: DescribeIpamResourceDiscoveryAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The resource discovery association IDs.
describeIpamResourceDiscoveryAssociations_ipamResourceDiscoveryAssociationIds :: Lens.Lens' DescribeIpamResourceDiscoveryAssociations (Prelude.Maybe [Prelude.Text])
describeIpamResourceDiscoveryAssociations_ipamResourceDiscoveryAssociationIds = Lens.lens (\DescribeIpamResourceDiscoveryAssociations' {ipamResourceDiscoveryAssociationIds} -> ipamResourceDiscoveryAssociationIds) (\s@DescribeIpamResourceDiscoveryAssociations' {} a -> s {ipamResourceDiscoveryAssociationIds = a} :: DescribeIpamResourceDiscoveryAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of resource discovery associations to return in one
-- page of results.
describeIpamResourceDiscoveryAssociations_maxResults :: Lens.Lens' DescribeIpamResourceDiscoveryAssociations (Prelude.Maybe Prelude.Natural)
describeIpamResourceDiscoveryAssociations_maxResults = Lens.lens (\DescribeIpamResourceDiscoveryAssociations' {maxResults} -> maxResults) (\s@DescribeIpamResourceDiscoveryAssociations' {} a -> s {maxResults = a} :: DescribeIpamResourceDiscoveryAssociations)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
describeIpamResourceDiscoveryAssociations_nextToken :: Lens.Lens' DescribeIpamResourceDiscoveryAssociations (Prelude.Maybe Prelude.Text)
describeIpamResourceDiscoveryAssociations_nextToken = Lens.lens (\DescribeIpamResourceDiscoveryAssociations' {nextToken} -> nextToken) (\s@DescribeIpamResourceDiscoveryAssociations' {} a -> s {nextToken = a} :: DescribeIpamResourceDiscoveryAssociations)

instance
  Core.AWSPager
    DescribeIpamResourceDiscoveryAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeIpamResourceDiscoveryAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeIpamResourceDiscoveryAssociationsResponse_ipamResourceDiscoveryAssociations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeIpamResourceDiscoveryAssociations_nextToken
          Lens..~ rs
          Lens.^? describeIpamResourceDiscoveryAssociationsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeIpamResourceDiscoveryAssociations
  where
  type
    AWSResponse
      DescribeIpamResourceDiscoveryAssociations =
      DescribeIpamResourceDiscoveryAssociationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeIpamResourceDiscoveryAssociationsResponse'
            Prelude.<$> ( x
                            Data..@? "ipamResourceDiscoveryAssociationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeIpamResourceDiscoveryAssociations
  where
  hashWithSalt
    _salt
    DescribeIpamResourceDiscoveryAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` ipamResourceDiscoveryAssociationIds
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeIpamResourceDiscoveryAssociations
  where
  rnf DescribeIpamResourceDiscoveryAssociations' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryAssociationIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeIpamResourceDiscoveryAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeIpamResourceDiscoveryAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeIpamResourceDiscoveryAssociations
  where
  toQuery
    DescribeIpamResourceDiscoveryAssociations' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DescribeIpamResourceDiscoveryAssociations" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
          Data.toQuery
            ( Data.toQueryList
                "IpamResourceDiscoveryAssociationId"
                Prelude.<$> ipamResourceDiscoveryAssociationIds
            ),
          "MaxResults" Data.=: maxResults,
          "NextToken" Data.=: nextToken
        ]

-- | /See:/ 'newDescribeIpamResourceDiscoveryAssociationsResponse' smart constructor.
data DescribeIpamResourceDiscoveryAssociationsResponse = DescribeIpamResourceDiscoveryAssociationsResponse'
  { -- | The resource discovery associations.
    ipamResourceDiscoveryAssociations :: Prelude.Maybe [IpamResourceDiscoveryAssociation],
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpamResourceDiscoveryAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamResourceDiscoveryAssociations', 'describeIpamResourceDiscoveryAssociationsResponse_ipamResourceDiscoveryAssociations' - The resource discovery associations.
--
-- 'nextToken', 'describeIpamResourceDiscoveryAssociationsResponse_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'httpStatus', 'describeIpamResourceDiscoveryAssociationsResponse_httpStatus' - The response's http status code.
newDescribeIpamResourceDiscoveryAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIpamResourceDiscoveryAssociationsResponse
newDescribeIpamResourceDiscoveryAssociationsResponse
  pHttpStatus_ =
    DescribeIpamResourceDiscoveryAssociationsResponse'
      { ipamResourceDiscoveryAssociations =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The resource discovery associations.
describeIpamResourceDiscoveryAssociationsResponse_ipamResourceDiscoveryAssociations :: Lens.Lens' DescribeIpamResourceDiscoveryAssociationsResponse (Prelude.Maybe [IpamResourceDiscoveryAssociation])
describeIpamResourceDiscoveryAssociationsResponse_ipamResourceDiscoveryAssociations = Lens.lens (\DescribeIpamResourceDiscoveryAssociationsResponse' {ipamResourceDiscoveryAssociations} -> ipamResourceDiscoveryAssociations) (\s@DescribeIpamResourceDiscoveryAssociationsResponse' {} a -> s {ipamResourceDiscoveryAssociations = a} :: DescribeIpamResourceDiscoveryAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
describeIpamResourceDiscoveryAssociationsResponse_nextToken :: Lens.Lens' DescribeIpamResourceDiscoveryAssociationsResponse (Prelude.Maybe Prelude.Text)
describeIpamResourceDiscoveryAssociationsResponse_nextToken = Lens.lens (\DescribeIpamResourceDiscoveryAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeIpamResourceDiscoveryAssociationsResponse' {} a -> s {nextToken = a} :: DescribeIpamResourceDiscoveryAssociationsResponse)

-- | The response's http status code.
describeIpamResourceDiscoveryAssociationsResponse_httpStatus :: Lens.Lens' DescribeIpamResourceDiscoveryAssociationsResponse Prelude.Int
describeIpamResourceDiscoveryAssociationsResponse_httpStatus = Lens.lens (\DescribeIpamResourceDiscoveryAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeIpamResourceDiscoveryAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeIpamResourceDiscoveryAssociationsResponse)

instance
  Prelude.NFData
    DescribeIpamResourceDiscoveryAssociationsResponse
  where
  rnf
    DescribeIpamResourceDiscoveryAssociationsResponse' {..} =
      Prelude.rnf ipamResourceDiscoveryAssociations
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
