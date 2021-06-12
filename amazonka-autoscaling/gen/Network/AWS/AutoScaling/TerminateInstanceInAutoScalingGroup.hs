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
-- Module      : Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified instance and optionally adjusts the desired
-- group size.
--
-- This call simply makes a termination request. The instance is not
-- terminated immediately. When an instance is terminated, the instance
-- status changes to @terminated@. You can\'t connect to or start an
-- instance after you\'ve terminated it.
--
-- If you do not specify the option to decrement the desired capacity,
-- Amazon EC2 Auto Scaling launches instances to replace the ones that are
-- terminated.
--
-- By default, Amazon EC2 Auto Scaling balances instances across all
-- Availability Zones. If you decrement the desired capacity, your Auto
-- Scaling group can become unbalanced between Availability Zones. Amazon
-- EC2 Auto Scaling tries to rebalance the group, and rebalancing might
-- terminate instances in other zones. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-benefits.html#AutoScalingBehavior.InstanceUsage Rebalancing activities>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
  ( -- * Creating a Request
    TerminateInstanceInAutoScalingGroup (..),
    newTerminateInstanceInAutoScalingGroup,

    -- * Request Lenses
    terminateInstanceInAutoScalingGroup_instanceId,
    terminateInstanceInAutoScalingGroup_shouldDecrementDesiredCapacity,

    -- * Destructuring the Response
    TerminateInstanceInAutoScalingGroupResponse (..),
    newTerminateInstanceInAutoScalingGroupResponse,

    -- * Response Lenses
    terminateInstanceInAutoScalingGroupResponse_activity,
    terminateInstanceInAutoScalingGroupResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTerminateInstanceInAutoScalingGroup' smart constructor.
data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup'
  { -- | The ID of the instance.
    instanceId :: Core.Text,
    -- | Indicates whether terminating the instance also decrements the size of
    -- the Auto Scaling group.
    shouldDecrementDesiredCapacity :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminateInstanceInAutoScalingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'terminateInstanceInAutoScalingGroup_instanceId' - The ID of the instance.
--
-- 'shouldDecrementDesiredCapacity', 'terminateInstanceInAutoScalingGroup_shouldDecrementDesiredCapacity' - Indicates whether terminating the instance also decrements the size of
-- the Auto Scaling group.
newTerminateInstanceInAutoScalingGroup ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'shouldDecrementDesiredCapacity'
  Core.Bool ->
  TerminateInstanceInAutoScalingGroup
newTerminateInstanceInAutoScalingGroup
  pInstanceId_
  pShouldDecrementDesiredCapacity_ =
    TerminateInstanceInAutoScalingGroup'
      { instanceId =
          pInstanceId_,
        shouldDecrementDesiredCapacity =
          pShouldDecrementDesiredCapacity_
      }

-- | The ID of the instance.
terminateInstanceInAutoScalingGroup_instanceId :: Lens.Lens' TerminateInstanceInAutoScalingGroup Core.Text
terminateInstanceInAutoScalingGroup_instanceId = Lens.lens (\TerminateInstanceInAutoScalingGroup' {instanceId} -> instanceId) (\s@TerminateInstanceInAutoScalingGroup' {} a -> s {instanceId = a} :: TerminateInstanceInAutoScalingGroup)

-- | Indicates whether terminating the instance also decrements the size of
-- the Auto Scaling group.
terminateInstanceInAutoScalingGroup_shouldDecrementDesiredCapacity :: Lens.Lens' TerminateInstanceInAutoScalingGroup Core.Bool
terminateInstanceInAutoScalingGroup_shouldDecrementDesiredCapacity = Lens.lens (\TerminateInstanceInAutoScalingGroup' {shouldDecrementDesiredCapacity} -> shouldDecrementDesiredCapacity) (\s@TerminateInstanceInAutoScalingGroup' {} a -> s {shouldDecrementDesiredCapacity = a} :: TerminateInstanceInAutoScalingGroup)

instance
  Core.AWSRequest
    TerminateInstanceInAutoScalingGroup
  where
  type
    AWSResponse TerminateInstanceInAutoScalingGroup =
      TerminateInstanceInAutoScalingGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "TerminateInstanceInAutoScalingGroupResult"
      ( \s h x ->
          TerminateInstanceInAutoScalingGroupResponse'
            Core.<$> (x Core..@? "Activity")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    TerminateInstanceInAutoScalingGroup

instance
  Core.NFData
    TerminateInstanceInAutoScalingGroup

instance
  Core.ToHeaders
    TerminateInstanceInAutoScalingGroup
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    TerminateInstanceInAutoScalingGroup
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    TerminateInstanceInAutoScalingGroup
  where
  toQuery TerminateInstanceInAutoScalingGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "TerminateInstanceInAutoScalingGroup" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "InstanceId" Core.=: instanceId,
        "ShouldDecrementDesiredCapacity"
          Core.=: shouldDecrementDesiredCapacity
      ]

-- | /See:/ 'newTerminateInstanceInAutoScalingGroupResponse' smart constructor.
data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse'
  { -- | A scaling activity.
    activity :: Core.Maybe Activity,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminateInstanceInAutoScalingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activity', 'terminateInstanceInAutoScalingGroupResponse_activity' - A scaling activity.
--
-- 'httpStatus', 'terminateInstanceInAutoScalingGroupResponse_httpStatus' - The response's http status code.
newTerminateInstanceInAutoScalingGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  TerminateInstanceInAutoScalingGroupResponse
newTerminateInstanceInAutoScalingGroupResponse
  pHttpStatus_ =
    TerminateInstanceInAutoScalingGroupResponse'
      { activity =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A scaling activity.
terminateInstanceInAutoScalingGroupResponse_activity :: Lens.Lens' TerminateInstanceInAutoScalingGroupResponse (Core.Maybe Activity)
terminateInstanceInAutoScalingGroupResponse_activity = Lens.lens (\TerminateInstanceInAutoScalingGroupResponse' {activity} -> activity) (\s@TerminateInstanceInAutoScalingGroupResponse' {} a -> s {activity = a} :: TerminateInstanceInAutoScalingGroupResponse)

-- | The response's http status code.
terminateInstanceInAutoScalingGroupResponse_httpStatus :: Lens.Lens' TerminateInstanceInAutoScalingGroupResponse Core.Int
terminateInstanceInAutoScalingGroupResponse_httpStatus = Lens.lens (\TerminateInstanceInAutoScalingGroupResponse' {httpStatus} -> httpStatus) (\s@TerminateInstanceInAutoScalingGroupResponse' {} a -> s {httpStatus = a} :: TerminateInstanceInAutoScalingGroupResponse)

instance
  Core.NFData
    TerminateInstanceInAutoScalingGroupResponse
