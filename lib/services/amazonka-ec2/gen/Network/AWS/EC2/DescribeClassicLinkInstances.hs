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
-- Module      : Network.AWS.EC2.DescribeClassicLinkInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClassicLinkInstances
  ( -- * Creating a Request
    DescribeClassicLinkInstances (..),
    newDescribeClassicLinkInstances,

    -- * Request Lenses
    describeClassicLinkInstances_filters,
    describeClassicLinkInstances_nextToken,
    describeClassicLinkInstances_instanceIds,
    describeClassicLinkInstances_dryRun,
    describeClassicLinkInstances_maxResults,

    -- * Destructuring the Response
    DescribeClassicLinkInstancesResponse (..),
    newDescribeClassicLinkInstancesResponse,

    -- * Response Lenses
    describeClassicLinkInstancesResponse_nextToken,
    describeClassicLinkInstancesResponse_instances,
    describeClassicLinkInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeClassicLinkInstances' smart constructor.
data DescribeClassicLinkInstances = DescribeClassicLinkInstances'
  { -- | One or more filters.
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
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more instance IDs. Must be instances linked to a VPC through
    -- ClassicLink.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    --
    -- Constraint: If the value is greater than 1000, we return only 1000
    -- items.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'describeClassicLinkInstances_nextToken' - The token for the next page of results.
--
-- 'instanceIds', 'describeClassicLinkInstances_instanceIds' - One or more instance IDs. Must be instances linked to a VPC through
-- ClassicLink.
--
-- 'dryRun', 'describeClassicLinkInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeClassicLinkInstances_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- Constraint: If the value is greater than 1000, we return only 1000
-- items.
newDescribeClassicLinkInstances ::
  DescribeClassicLinkInstances
newDescribeClassicLinkInstances =
  DescribeClassicLinkInstances'
    { filters =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceIds = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

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

-- | The token for the next page of results.
describeClassicLinkInstances_nextToken :: Lens.Lens' DescribeClassicLinkInstances (Prelude.Maybe Prelude.Text)
describeClassicLinkInstances_nextToken = Lens.lens (\DescribeClassicLinkInstances' {nextToken} -> nextToken) (\s@DescribeClassicLinkInstances' {} a -> s {nextToken = a} :: DescribeClassicLinkInstances)

-- | One or more instance IDs. Must be instances linked to a VPC through
-- ClassicLink.
describeClassicLinkInstances_instanceIds :: Lens.Lens' DescribeClassicLinkInstances (Prelude.Maybe [Prelude.Text])
describeClassicLinkInstances_instanceIds = Lens.lens (\DescribeClassicLinkInstances' {instanceIds} -> instanceIds) (\s@DescribeClassicLinkInstances' {} a -> s {instanceIds = a} :: DescribeClassicLinkInstances) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeClassicLinkInstances_dryRun :: Lens.Lens' DescribeClassicLinkInstances (Prelude.Maybe Prelude.Bool)
describeClassicLinkInstances_dryRun = Lens.lens (\DescribeClassicLinkInstances' {dryRun} -> dryRun) (\s@DescribeClassicLinkInstances' {} a -> s {dryRun = a} :: DescribeClassicLinkInstances)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- Constraint: If the value is greater than 1000, we return only 1000
-- items.
describeClassicLinkInstances_maxResults :: Lens.Lens' DescribeClassicLinkInstances (Prelude.Maybe Prelude.Natural)
describeClassicLinkInstances_maxResults = Lens.lens (\DescribeClassicLinkInstances' {maxResults} -> maxResults) (\s@DescribeClassicLinkInstances' {} a -> s {maxResults = a} :: DescribeClassicLinkInstances)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClassicLinkInstancesResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "instancesSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeClassicLinkInstances

instance Prelude.NFData DescribeClassicLinkInstances

instance Core.ToHeaders DescribeClassicLinkInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeClassicLinkInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeClassicLinkInstances where
  toQuery DescribeClassicLinkInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeClassicLinkInstances" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "InstanceId"
              Prelude.<$> instanceIds
          ),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeClassicLinkInstancesResponse' smart constructor.
data DescribeClassicLinkInstancesResponse = DescribeClassicLinkInstancesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about one or more linked EC2-Classic instances.
    instances :: Prelude.Maybe [ClassicLinkInstance],
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
-- 'nextToken', 'describeClassicLinkInstancesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'instances', 'describeClassicLinkInstancesResponse_instances' - Information about one or more linked EC2-Classic instances.
--
-- 'httpStatus', 'describeClassicLinkInstancesResponse_httpStatus' - The response's http status code.
newDescribeClassicLinkInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClassicLinkInstancesResponse
newDescribeClassicLinkInstancesResponse pHttpStatus_ =
  DescribeClassicLinkInstancesResponse'
    { nextToken =
        Prelude.Nothing,
      instances = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeClassicLinkInstancesResponse_nextToken :: Lens.Lens' DescribeClassicLinkInstancesResponse (Prelude.Maybe Prelude.Text)
describeClassicLinkInstancesResponse_nextToken = Lens.lens (\DescribeClassicLinkInstancesResponse' {nextToken} -> nextToken) (\s@DescribeClassicLinkInstancesResponse' {} a -> s {nextToken = a} :: DescribeClassicLinkInstancesResponse)

-- | Information about one or more linked EC2-Classic instances.
describeClassicLinkInstancesResponse_instances :: Lens.Lens' DescribeClassicLinkInstancesResponse (Prelude.Maybe [ClassicLinkInstance])
describeClassicLinkInstancesResponse_instances = Lens.lens (\DescribeClassicLinkInstancesResponse' {instances} -> instances) (\s@DescribeClassicLinkInstancesResponse' {} a -> s {instances = a} :: DescribeClassicLinkInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeClassicLinkInstancesResponse_httpStatus :: Lens.Lens' DescribeClassicLinkInstancesResponse Prelude.Int
describeClassicLinkInstancesResponse_httpStatus = Lens.lens (\DescribeClassicLinkInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeClassicLinkInstancesResponse' {} a -> s {httpStatus = a} :: DescribeClassicLinkInstancesResponse)

instance
  Prelude.NFData
    DescribeClassicLinkInstancesResponse
