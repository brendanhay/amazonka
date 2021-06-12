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
    describeClassicLinkInstances_instanceIds,
    describeClassicLinkInstances_nextToken,
    describeClassicLinkInstances_dryRun,
    describeClassicLinkInstances_maxResults,
    describeClassicLinkInstances_filters,

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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeClassicLinkInstances' smart constructor.
data DescribeClassicLinkInstances = DescribeClassicLinkInstances'
  { -- | One or more instance IDs. Must be instances linked to a VPC through
    -- ClassicLink.
    instanceIds :: Core.Maybe [Core.Text],
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    --
    -- Constraint: If the value is greater than 1000, we return only 1000
    -- items.
    maxResults :: Core.Maybe Core.Natural,
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
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClassicLinkInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'describeClassicLinkInstances_instanceIds' - One or more instance IDs. Must be instances linked to a VPC through
-- ClassicLink.
--
-- 'nextToken', 'describeClassicLinkInstances_nextToken' - The token for the next page of results.
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
newDescribeClassicLinkInstances ::
  DescribeClassicLinkInstances
newDescribeClassicLinkInstances =
  DescribeClassicLinkInstances'
    { instanceIds =
        Core.Nothing,
      nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | One or more instance IDs. Must be instances linked to a VPC through
-- ClassicLink.
describeClassicLinkInstances_instanceIds :: Lens.Lens' DescribeClassicLinkInstances (Core.Maybe [Core.Text])
describeClassicLinkInstances_instanceIds = Lens.lens (\DescribeClassicLinkInstances' {instanceIds} -> instanceIds) (\s@DescribeClassicLinkInstances' {} a -> s {instanceIds = a} :: DescribeClassicLinkInstances) Core.. Lens.mapping Lens._Coerce

-- | The token for the next page of results.
describeClassicLinkInstances_nextToken :: Lens.Lens' DescribeClassicLinkInstances (Core.Maybe Core.Text)
describeClassicLinkInstances_nextToken = Lens.lens (\DescribeClassicLinkInstances' {nextToken} -> nextToken) (\s@DescribeClassicLinkInstances' {} a -> s {nextToken = a} :: DescribeClassicLinkInstances)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeClassicLinkInstances_dryRun :: Lens.Lens' DescribeClassicLinkInstances (Core.Maybe Core.Bool)
describeClassicLinkInstances_dryRun = Lens.lens (\DescribeClassicLinkInstances' {dryRun} -> dryRun) (\s@DescribeClassicLinkInstances' {} a -> s {dryRun = a} :: DescribeClassicLinkInstances)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- Constraint: If the value is greater than 1000, we return only 1000
-- items.
describeClassicLinkInstances_maxResults :: Lens.Lens' DescribeClassicLinkInstances (Core.Maybe Core.Natural)
describeClassicLinkInstances_maxResults = Lens.lens (\DescribeClassicLinkInstances' {maxResults} -> maxResults) (\s@DescribeClassicLinkInstances' {} a -> s {maxResults = a} :: DescribeClassicLinkInstances)

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
describeClassicLinkInstances_filters :: Lens.Lens' DescribeClassicLinkInstances (Core.Maybe [Filter])
describeClassicLinkInstances_filters = Lens.lens (\DescribeClassicLinkInstances' {filters} -> filters) (\s@DescribeClassicLinkInstances' {} a -> s {filters = a} :: DescribeClassicLinkInstances) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeClassicLinkInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClassicLinkInstancesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClassicLinkInstancesResponse_instances
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeClassicLinkInstances_nextToken
          Lens..~ rs
          Lens.^? describeClassicLinkInstancesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeClassicLinkInstances where
  type
    AWSResponse DescribeClassicLinkInstances =
      DescribeClassicLinkInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClassicLinkInstancesResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "instancesSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeClassicLinkInstances

instance Core.NFData DescribeClassicLinkInstances

instance Core.ToHeaders DescribeClassicLinkInstances where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeClassicLinkInstances where
  toPath = Core.const "/"

instance Core.ToQuery DescribeClassicLinkInstances where
  toQuery DescribeClassicLinkInstances' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeClassicLinkInstances" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          (Core.toQueryList "InstanceId" Core.<$> instanceIds),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeClassicLinkInstancesResponse' smart constructor.
data DescribeClassicLinkInstancesResponse = DescribeClassicLinkInstancesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about one or more linked EC2-Classic instances.
    instances :: Core.Maybe [ClassicLinkInstance],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeClassicLinkInstancesResponse
newDescribeClassicLinkInstancesResponse pHttpStatus_ =
  DescribeClassicLinkInstancesResponse'
    { nextToken =
        Core.Nothing,
      instances = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeClassicLinkInstancesResponse_nextToken :: Lens.Lens' DescribeClassicLinkInstancesResponse (Core.Maybe Core.Text)
describeClassicLinkInstancesResponse_nextToken = Lens.lens (\DescribeClassicLinkInstancesResponse' {nextToken} -> nextToken) (\s@DescribeClassicLinkInstancesResponse' {} a -> s {nextToken = a} :: DescribeClassicLinkInstancesResponse)

-- | Information about one or more linked EC2-Classic instances.
describeClassicLinkInstancesResponse_instances :: Lens.Lens' DescribeClassicLinkInstancesResponse (Core.Maybe [ClassicLinkInstance])
describeClassicLinkInstancesResponse_instances = Lens.lens (\DescribeClassicLinkInstancesResponse' {instances} -> instances) (\s@DescribeClassicLinkInstancesResponse' {} a -> s {instances = a} :: DescribeClassicLinkInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeClassicLinkInstancesResponse_httpStatus :: Lens.Lens' DescribeClassicLinkInstancesResponse Core.Int
describeClassicLinkInstancesResponse_httpStatus = Lens.lens (\DescribeClassicLinkInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeClassicLinkInstancesResponse' {} a -> s {httpStatus = a} :: DescribeClassicLinkInstancesResponse)

instance
  Core.NFData
    DescribeClassicLinkInstancesResponse
