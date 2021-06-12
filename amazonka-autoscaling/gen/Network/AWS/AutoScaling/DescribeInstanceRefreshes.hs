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
-- Module      : Network.AWS.AutoScaling.DescribeInstanceRefreshes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more instance refreshes.
--
-- You can determine the status of a request by looking at the @Status@
-- parameter. The following are the possible statuses:
--
-- -   @Pending@ - The request was created, but the operation has not
--     started.
--
-- -   @InProgress@ - The operation is in progress.
--
-- -   @Successful@ - The operation completed successfully.
--
-- -   @Failed@ - The operation failed to complete. You can troubleshoot
--     using the status reason and the scaling activities.
--
-- -   @Cancelling@ - An ongoing operation is being cancelled. Cancellation
--     does not roll back any replacements that have already been
--     completed, but it prevents new replacements from being started.
--
-- -   @Cancelled@ - The operation is cancelled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh.html Replacing Auto Scaling Instances Based on an Instance Refresh>.
module Network.AWS.AutoScaling.DescribeInstanceRefreshes
  ( -- * Creating a Request
    DescribeInstanceRefreshes (..),
    newDescribeInstanceRefreshes,

    -- * Request Lenses
    describeInstanceRefreshes_nextToken,
    describeInstanceRefreshes_instanceRefreshIds,
    describeInstanceRefreshes_maxRecords,
    describeInstanceRefreshes_autoScalingGroupName,

    -- * Destructuring the Response
    DescribeInstanceRefreshesResponse (..),
    newDescribeInstanceRefreshesResponse,

    -- * Response Lenses
    describeInstanceRefreshesResponse_nextToken,
    describeInstanceRefreshesResponse_instanceRefreshes,
    describeInstanceRefreshesResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeInstanceRefreshes' smart constructor.
data DescribeInstanceRefreshes = DescribeInstanceRefreshes'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | One or more instance refresh IDs.
    instanceRefreshIds :: Core.Maybe [Core.Text],
    -- | The maximum number of items to return with this call. The default value
    -- is @50@ and the maximum value is @100@.
    maxRecords :: Core.Maybe Core.Int,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstanceRefreshes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceRefreshes_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'instanceRefreshIds', 'describeInstanceRefreshes_instanceRefreshIds' - One or more instance refresh IDs.
--
-- 'maxRecords', 'describeInstanceRefreshes_maxRecords' - The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
--
-- 'autoScalingGroupName', 'describeInstanceRefreshes_autoScalingGroupName' - The name of the Auto Scaling group.
newDescribeInstanceRefreshes ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  DescribeInstanceRefreshes
newDescribeInstanceRefreshes pAutoScalingGroupName_ =
  DescribeInstanceRefreshes'
    { nextToken =
        Core.Nothing,
      instanceRefreshIds = Core.Nothing,
      maxRecords = Core.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstanceRefreshes_nextToken :: Lens.Lens' DescribeInstanceRefreshes (Core.Maybe Core.Text)
describeInstanceRefreshes_nextToken = Lens.lens (\DescribeInstanceRefreshes' {nextToken} -> nextToken) (\s@DescribeInstanceRefreshes' {} a -> s {nextToken = a} :: DescribeInstanceRefreshes)

-- | One or more instance refresh IDs.
describeInstanceRefreshes_instanceRefreshIds :: Lens.Lens' DescribeInstanceRefreshes (Core.Maybe [Core.Text])
describeInstanceRefreshes_instanceRefreshIds = Lens.lens (\DescribeInstanceRefreshes' {instanceRefreshIds} -> instanceRefreshIds) (\s@DescribeInstanceRefreshes' {} a -> s {instanceRefreshIds = a} :: DescribeInstanceRefreshes) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
describeInstanceRefreshes_maxRecords :: Lens.Lens' DescribeInstanceRefreshes (Core.Maybe Core.Int)
describeInstanceRefreshes_maxRecords = Lens.lens (\DescribeInstanceRefreshes' {maxRecords} -> maxRecords) (\s@DescribeInstanceRefreshes' {} a -> s {maxRecords = a} :: DescribeInstanceRefreshes)

-- | The name of the Auto Scaling group.
describeInstanceRefreshes_autoScalingGroupName :: Lens.Lens' DescribeInstanceRefreshes Core.Text
describeInstanceRefreshes_autoScalingGroupName = Lens.lens (\DescribeInstanceRefreshes' {autoScalingGroupName} -> autoScalingGroupName) (\s@DescribeInstanceRefreshes' {} a -> s {autoScalingGroupName = a} :: DescribeInstanceRefreshes)

instance Core.AWSRequest DescribeInstanceRefreshes where
  type
    AWSResponse DescribeInstanceRefreshes =
      DescribeInstanceRefreshesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeInstanceRefreshesResult"
      ( \s h x ->
          DescribeInstanceRefreshesResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "InstanceRefreshes" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeInstanceRefreshes

instance Core.NFData DescribeInstanceRefreshes

instance Core.ToHeaders DescribeInstanceRefreshes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeInstanceRefreshes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeInstanceRefreshes where
  toQuery DescribeInstanceRefreshes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeInstanceRefreshes" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "InstanceRefreshIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> instanceRefreshIds
            ),
        "MaxRecords" Core.=: maxRecords,
        "AutoScalingGroupName" Core.=: autoScalingGroupName
      ]

-- | /See:/ 'newDescribeInstanceRefreshesResponse' smart constructor.
data DescribeInstanceRefreshesResponse = DescribeInstanceRefreshesResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The instance refreshes for the specified group.
    instanceRefreshes :: Core.Maybe [InstanceRefresh],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstanceRefreshesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceRefreshesResponse_nextToken' - A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
--
-- 'instanceRefreshes', 'describeInstanceRefreshesResponse_instanceRefreshes' - The instance refreshes for the specified group.
--
-- 'httpStatus', 'describeInstanceRefreshesResponse_httpStatus' - The response's http status code.
newDescribeInstanceRefreshesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeInstanceRefreshesResponse
newDescribeInstanceRefreshesResponse pHttpStatus_ =
  DescribeInstanceRefreshesResponse'
    { nextToken =
        Core.Nothing,
      instanceRefreshes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeInstanceRefreshesResponse_nextToken :: Lens.Lens' DescribeInstanceRefreshesResponse (Core.Maybe Core.Text)
describeInstanceRefreshesResponse_nextToken = Lens.lens (\DescribeInstanceRefreshesResponse' {nextToken} -> nextToken) (\s@DescribeInstanceRefreshesResponse' {} a -> s {nextToken = a} :: DescribeInstanceRefreshesResponse)

-- | The instance refreshes for the specified group.
describeInstanceRefreshesResponse_instanceRefreshes :: Lens.Lens' DescribeInstanceRefreshesResponse (Core.Maybe [InstanceRefresh])
describeInstanceRefreshesResponse_instanceRefreshes = Lens.lens (\DescribeInstanceRefreshesResponse' {instanceRefreshes} -> instanceRefreshes) (\s@DescribeInstanceRefreshesResponse' {} a -> s {instanceRefreshes = a} :: DescribeInstanceRefreshesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInstanceRefreshesResponse_httpStatus :: Lens.Lens' DescribeInstanceRefreshesResponse Core.Int
describeInstanceRefreshesResponse_httpStatus = Lens.lens (\DescribeInstanceRefreshesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceRefreshesResponse' {} a -> s {httpStatus = a} :: DescribeInstanceRefreshesResponse)

instance
  Core.NFData
    DescribeInstanceRefreshesResponse
