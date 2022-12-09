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
-- Module      : Amazonka.EC2.DescribeClassicLinkInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your linked EC2-Classic instances. This request
-- only returns information about EC2-Classic instances linked to a VPC
-- through ClassicLink. You cannot use this request to return information
-- about other instances.
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeClassicLinkInstances
  ( -- * Creating a Request
    DescribeClassicLinkInstances (..),
    newDescribeClassicLinkInstances,

    -- * Request Lenses
    describeClassicLinkInstances_dryRun,
    describeClassicLinkInstances_filters,
    describeClassicLinkInstances_instanceIds,
    describeClassicLinkInstances_maxResults,
    describeClassicLinkInstances_nextToken,

    -- * Destructuring the Response
    DescribeClassicLinkInstancesResponse (..),
    newDescribeClassicLinkInstancesResponse,

    -- * Response Lenses
    describeClassicLinkInstancesResponse_instances,
    describeClassicLinkInstancesResponse_nextToken,
    describeClassicLinkInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeClassicLinkInstances' smart constructor.
data DescribeClassicLinkInstances = DescribeClassicLinkInstances'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @group-id@ - The ID of a VPC security group that\'s associated with
    --     the instance.
    --
    -- -   @instance-id@ - The ID of the instance.
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @vpc-id@ - The ID of the VPC to which the instance is linked.
    --
    --     @vpc-id@ - The ID of the VPC that the instance is linked to.
    filters :: Prelude.Maybe [Filter],
    -- | One or more instance IDs. Must be instances linked to a VPC through
    -- ClassicLink.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    --
    -- Constraint: If the value is greater than 1000, we return only 1000
    -- items.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClassicLinkInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeClassicLinkInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeClassicLinkInstances_filters' - One or more filters.
--
-- -   @group-id@ - The ID of a VPC security group that\'s associated with
--     the instance.
--
-- -   @instance-id@ - The ID of the instance.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @vpc-id@ - The ID of the VPC to which the instance is linked.
--
--     @vpc-id@ - The ID of the VPC that the instance is linked to.
--
-- 'instanceIds', 'describeClassicLinkInstances_instanceIds' - One or more instance IDs. Must be instances linked to a VPC through
-- ClassicLink.
--
-- 'maxResults', 'describeClassicLinkInstances_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- Constraint: If the value is greater than 1000, we return only 1000
-- items.
--
-- 'nextToken', 'describeClassicLinkInstances_nextToken' - The token for the next page of results.
newDescribeClassicLinkInstances ::
  DescribeClassicLinkInstances
newDescribeClassicLinkInstances =
  DescribeClassicLinkInstances'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      instanceIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeClassicLinkInstances_dryRun :: Lens.Lens' DescribeClassicLinkInstances (Prelude.Maybe Prelude.Bool)
describeClassicLinkInstances_dryRun = Lens.lens (\DescribeClassicLinkInstances' {dryRun} -> dryRun) (\s@DescribeClassicLinkInstances' {} a -> s {dryRun = a} :: DescribeClassicLinkInstances)

-- | One or more filters.
--
-- -   @group-id@ - The ID of a VPC security group that\'s associated with
--     the instance.
--
-- -   @instance-id@ - The ID of the instance.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @vpc-id@ - The ID of the VPC to which the instance is linked.
--
--     @vpc-id@ - The ID of the VPC that the instance is linked to.
describeClassicLinkInstances_filters :: Lens.Lens' DescribeClassicLinkInstances (Prelude.Maybe [Filter])
describeClassicLinkInstances_filters = Lens.lens (\DescribeClassicLinkInstances' {filters} -> filters) (\s@DescribeClassicLinkInstances' {} a -> s {filters = a} :: DescribeClassicLinkInstances) Prelude.. Lens.mapping Lens.coerced

-- | One or more instance IDs. Must be instances linked to a VPC through
-- ClassicLink.
describeClassicLinkInstances_instanceIds :: Lens.Lens' DescribeClassicLinkInstances (Prelude.Maybe [Prelude.Text])
describeClassicLinkInstances_instanceIds = Lens.lens (\DescribeClassicLinkInstances' {instanceIds} -> instanceIds) (\s@DescribeClassicLinkInstances' {} a -> s {instanceIds = a} :: DescribeClassicLinkInstances) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- Constraint: If the value is greater than 1000, we return only 1000
-- items.
describeClassicLinkInstances_maxResults :: Lens.Lens' DescribeClassicLinkInstances (Prelude.Maybe Prelude.Natural)
describeClassicLinkInstances_maxResults = Lens.lens (\DescribeClassicLinkInstances' {maxResults} -> maxResults) (\s@DescribeClassicLinkInstances' {} a -> s {maxResults = a} :: DescribeClassicLinkInstances)

-- | The token for the next page of results.
describeClassicLinkInstances_nextToken :: Lens.Lens' DescribeClassicLinkInstances (Prelude.Maybe Prelude.Text)
describeClassicLinkInstances_nextToken = Lens.lens (\DescribeClassicLinkInstances' {nextToken} -> nextToken) (\s@DescribeClassicLinkInstances' {} a -> s {nextToken = a} :: DescribeClassicLinkInstances)

instance Core.AWSPager DescribeClassicLinkInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClassicLinkInstancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClassicLinkInstancesResponse_instances
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeClassicLinkInstances_nextToken
          Lens..~ rs
          Lens.^? describeClassicLinkInstancesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeClassicLinkInstances where
  type
    AWSResponse DescribeClassicLinkInstances =
      DescribeClassicLinkInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClassicLinkInstancesResponse'
            Prelude.<$> ( x Data..@? "instancesSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeClassicLinkInstances
  where
  hashWithSalt _salt DescribeClassicLinkInstances' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeClassicLinkInstances where
  rnf DescribeClassicLinkInstances' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf instanceIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeClassicLinkInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeClassicLinkInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeClassicLinkInstances where
  toQuery DescribeClassicLinkInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeClassicLinkInstances" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          ( Data.toQueryList "InstanceId"
              Prelude.<$> instanceIds
          ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeClassicLinkInstancesResponse' smart constructor.
data DescribeClassicLinkInstancesResponse = DescribeClassicLinkInstancesResponse'
  { -- | Information about one or more linked EC2-Classic instances.
    instances :: Prelude.Maybe [ClassicLinkInstance],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClassicLinkInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'describeClassicLinkInstancesResponse_instances' - Information about one or more linked EC2-Classic instances.
--
-- 'nextToken', 'describeClassicLinkInstancesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeClassicLinkInstancesResponse_httpStatus' - The response's http status code.
newDescribeClassicLinkInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClassicLinkInstancesResponse
newDescribeClassicLinkInstancesResponse pHttpStatus_ =
  DescribeClassicLinkInstancesResponse'
    { instances =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about one or more linked EC2-Classic instances.
describeClassicLinkInstancesResponse_instances :: Lens.Lens' DescribeClassicLinkInstancesResponse (Prelude.Maybe [ClassicLinkInstance])
describeClassicLinkInstancesResponse_instances = Lens.lens (\DescribeClassicLinkInstancesResponse' {instances} -> instances) (\s@DescribeClassicLinkInstancesResponse' {} a -> s {instances = a} :: DescribeClassicLinkInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeClassicLinkInstancesResponse_nextToken :: Lens.Lens' DescribeClassicLinkInstancesResponse (Prelude.Maybe Prelude.Text)
describeClassicLinkInstancesResponse_nextToken = Lens.lens (\DescribeClassicLinkInstancesResponse' {nextToken} -> nextToken) (\s@DescribeClassicLinkInstancesResponse' {} a -> s {nextToken = a} :: DescribeClassicLinkInstancesResponse)

-- | The response's http status code.
describeClassicLinkInstancesResponse_httpStatus :: Lens.Lens' DescribeClassicLinkInstancesResponse Prelude.Int
describeClassicLinkInstancesResponse_httpStatus = Lens.lens (\DescribeClassicLinkInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeClassicLinkInstancesResponse' {} a -> s {httpStatus = a} :: DescribeClassicLinkInstancesResponse)

instance
  Prelude.NFData
    DescribeClassicLinkInstancesResponse
  where
  rnf DescribeClassicLinkInstancesResponse' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
