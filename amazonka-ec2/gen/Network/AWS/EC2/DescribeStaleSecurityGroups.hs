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
-- Module      : Network.AWS.EC2.DescribeStaleSecurityGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Describes the stale security group rules for security groups
-- in a specified VPC. Rules are stale when they reference a deleted
-- security group in a peer VPC, or a security group in a peer VPC for
-- which the VPC peering connection has been deleted.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeStaleSecurityGroups
  ( -- * Creating a Request
    DescribeStaleSecurityGroups (..),
    newDescribeStaleSecurityGroups,

    -- * Request Lenses
    describeStaleSecurityGroups_nextToken,
    describeStaleSecurityGroups_dryRun,
    describeStaleSecurityGroups_maxResults,
    describeStaleSecurityGroups_vpcId,

    -- * Destructuring the Response
    DescribeStaleSecurityGroupsResponse (..),
    newDescribeStaleSecurityGroupsResponse,

    -- * Response Lenses
    describeStaleSecurityGroupsResponse_nextToken,
    describeStaleSecurityGroupsResponse_staleSecurityGroupSet,
    describeStaleSecurityGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStaleSecurityGroups' smart constructor.
data DescribeStaleSecurityGroups = DescribeStaleSecurityGroups'
  { -- | The token for the next set of items to return. (You received this token
    -- from a prior call.)
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of items to return for this request. The request
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the VPC.
    vpcId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStaleSecurityGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeStaleSecurityGroups_nextToken' - The token for the next set of items to return. (You received this token
-- from a prior call.)
--
-- 'dryRun', 'describeStaleSecurityGroups_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeStaleSecurityGroups_maxResults' - The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'vpcId', 'describeStaleSecurityGroups_vpcId' - The ID of the VPC.
newDescribeStaleSecurityGroups ::
  -- | 'vpcId'
  Core.Text ->
  DescribeStaleSecurityGroups
newDescribeStaleSecurityGroups pVpcId_ =
  DescribeStaleSecurityGroups'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      vpcId = pVpcId_
    }

-- | The token for the next set of items to return. (You received this token
-- from a prior call.)
describeStaleSecurityGroups_nextToken :: Lens.Lens' DescribeStaleSecurityGroups (Core.Maybe Core.Text)
describeStaleSecurityGroups_nextToken = Lens.lens (\DescribeStaleSecurityGroups' {nextToken} -> nextToken) (\s@DescribeStaleSecurityGroups' {} a -> s {nextToken = a} :: DescribeStaleSecurityGroups)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeStaleSecurityGroups_dryRun :: Lens.Lens' DescribeStaleSecurityGroups (Core.Maybe Core.Bool)
describeStaleSecurityGroups_dryRun = Lens.lens (\DescribeStaleSecurityGroups' {dryRun} -> dryRun) (\s@DescribeStaleSecurityGroups' {} a -> s {dryRun = a} :: DescribeStaleSecurityGroups)

-- | The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeStaleSecurityGroups_maxResults :: Lens.Lens' DescribeStaleSecurityGroups (Core.Maybe Core.Natural)
describeStaleSecurityGroups_maxResults = Lens.lens (\DescribeStaleSecurityGroups' {maxResults} -> maxResults) (\s@DescribeStaleSecurityGroups' {} a -> s {maxResults = a} :: DescribeStaleSecurityGroups)

-- | The ID of the VPC.
describeStaleSecurityGroups_vpcId :: Lens.Lens' DescribeStaleSecurityGroups Core.Text
describeStaleSecurityGroups_vpcId = Lens.lens (\DescribeStaleSecurityGroups' {vpcId} -> vpcId) (\s@DescribeStaleSecurityGroups' {} a -> s {vpcId = a} :: DescribeStaleSecurityGroups)

instance Core.AWSPager DescribeStaleSecurityGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeStaleSecurityGroupsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeStaleSecurityGroupsResponse_staleSecurityGroupSet
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeStaleSecurityGroups_nextToken
          Lens..~ rs
          Lens.^? describeStaleSecurityGroupsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeStaleSecurityGroups where
  type
    AWSResponse DescribeStaleSecurityGroups =
      DescribeStaleSecurityGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeStaleSecurityGroupsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "staleSecurityGroupSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeStaleSecurityGroups

instance Core.NFData DescribeStaleSecurityGroups

instance Core.ToHeaders DescribeStaleSecurityGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeStaleSecurityGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeStaleSecurityGroups where
  toQuery DescribeStaleSecurityGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeStaleSecurityGroups" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newDescribeStaleSecurityGroupsResponse' smart constructor.
data DescribeStaleSecurityGroupsResponse = DescribeStaleSecurityGroupsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the stale security groups.
    staleSecurityGroupSet :: Core.Maybe [StaleSecurityGroup],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStaleSecurityGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeStaleSecurityGroupsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'staleSecurityGroupSet', 'describeStaleSecurityGroupsResponse_staleSecurityGroupSet' - Information about the stale security groups.
--
-- 'httpStatus', 'describeStaleSecurityGroupsResponse_httpStatus' - The response's http status code.
newDescribeStaleSecurityGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeStaleSecurityGroupsResponse
newDescribeStaleSecurityGroupsResponse pHttpStatus_ =
  DescribeStaleSecurityGroupsResponse'
    { nextToken =
        Core.Nothing,
      staleSecurityGroupSet = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeStaleSecurityGroupsResponse_nextToken :: Lens.Lens' DescribeStaleSecurityGroupsResponse (Core.Maybe Core.Text)
describeStaleSecurityGroupsResponse_nextToken = Lens.lens (\DescribeStaleSecurityGroupsResponse' {nextToken} -> nextToken) (\s@DescribeStaleSecurityGroupsResponse' {} a -> s {nextToken = a} :: DescribeStaleSecurityGroupsResponse)

-- | Information about the stale security groups.
describeStaleSecurityGroupsResponse_staleSecurityGroupSet :: Lens.Lens' DescribeStaleSecurityGroupsResponse (Core.Maybe [StaleSecurityGroup])
describeStaleSecurityGroupsResponse_staleSecurityGroupSet = Lens.lens (\DescribeStaleSecurityGroupsResponse' {staleSecurityGroupSet} -> staleSecurityGroupSet) (\s@DescribeStaleSecurityGroupsResponse' {} a -> s {staleSecurityGroupSet = a} :: DescribeStaleSecurityGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeStaleSecurityGroupsResponse_httpStatus :: Lens.Lens' DescribeStaleSecurityGroupsResponse Core.Int
describeStaleSecurityGroupsResponse_httpStatus = Lens.lens (\DescribeStaleSecurityGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeStaleSecurityGroupsResponse' {} a -> s {httpStatus = a} :: DescribeStaleSecurityGroupsResponse)

instance
  Core.NFData
    DescribeStaleSecurityGroupsResponse
