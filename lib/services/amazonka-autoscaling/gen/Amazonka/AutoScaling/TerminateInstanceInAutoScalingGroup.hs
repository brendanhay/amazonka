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
-- Module      : Amazonka.AutoScaling.TerminateInstanceInAutoScalingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified instance and optionally adjusts the desired
-- group size. This operation cannot be called on instances in a warm pool.
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
module Amazonka.AutoScaling.TerminateInstanceInAutoScalingGroup
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

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTerminateInstanceInAutoScalingGroup' smart constructor.
data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup'
  { -- | The ID of the instance.
    instanceId :: Prelude.Text,
    -- | Indicates whether terminating the instance also decrements the size of
    -- the Auto Scaling group.
    shouldDecrementDesiredCapacity :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'shouldDecrementDesiredCapacity'
  Prelude.Bool ->
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
terminateInstanceInAutoScalingGroup_instanceId :: Lens.Lens' TerminateInstanceInAutoScalingGroup Prelude.Text
terminateInstanceInAutoScalingGroup_instanceId = Lens.lens (\TerminateInstanceInAutoScalingGroup' {instanceId} -> instanceId) (\s@TerminateInstanceInAutoScalingGroup' {} a -> s {instanceId = a} :: TerminateInstanceInAutoScalingGroup)

-- | Indicates whether terminating the instance also decrements the size of
-- the Auto Scaling group.
terminateInstanceInAutoScalingGroup_shouldDecrementDesiredCapacity :: Lens.Lens' TerminateInstanceInAutoScalingGroup Prelude.Bool
terminateInstanceInAutoScalingGroup_shouldDecrementDesiredCapacity = Lens.lens (\TerminateInstanceInAutoScalingGroup' {shouldDecrementDesiredCapacity} -> shouldDecrementDesiredCapacity) (\s@TerminateInstanceInAutoScalingGroup' {} a -> s {shouldDecrementDesiredCapacity = a} :: TerminateInstanceInAutoScalingGroup)

instance
  Core.AWSRequest
    TerminateInstanceInAutoScalingGroup
  where
  type
    AWSResponse TerminateInstanceInAutoScalingGroup =
      TerminateInstanceInAutoScalingGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "TerminateInstanceInAutoScalingGroupResult"
      ( \s h x ->
          TerminateInstanceInAutoScalingGroupResponse'
            Prelude.<$> (x Data..@? "Activity")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    TerminateInstanceInAutoScalingGroup
  where
  hashWithSalt
    _salt
    TerminateInstanceInAutoScalingGroup' {..} =
      _salt `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` shouldDecrementDesiredCapacity

instance
  Prelude.NFData
    TerminateInstanceInAutoScalingGroup
  where
  rnf TerminateInstanceInAutoScalingGroup' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf shouldDecrementDesiredCapacity

instance
  Data.ToHeaders
    TerminateInstanceInAutoScalingGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    TerminateInstanceInAutoScalingGroup
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    TerminateInstanceInAutoScalingGroup
  where
  toQuery TerminateInstanceInAutoScalingGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "TerminateInstanceInAutoScalingGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "InstanceId" Data.=: instanceId,
        "ShouldDecrementDesiredCapacity"
          Data.=: shouldDecrementDesiredCapacity
      ]

-- | /See:/ 'newTerminateInstanceInAutoScalingGroupResponse' smart constructor.
data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse'
  { -- | A scaling activity.
    activity :: Prelude.Maybe Activity,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  TerminateInstanceInAutoScalingGroupResponse
newTerminateInstanceInAutoScalingGroupResponse
  pHttpStatus_ =
    TerminateInstanceInAutoScalingGroupResponse'
      { activity =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A scaling activity.
terminateInstanceInAutoScalingGroupResponse_activity :: Lens.Lens' TerminateInstanceInAutoScalingGroupResponse (Prelude.Maybe Activity)
terminateInstanceInAutoScalingGroupResponse_activity = Lens.lens (\TerminateInstanceInAutoScalingGroupResponse' {activity} -> activity) (\s@TerminateInstanceInAutoScalingGroupResponse' {} a -> s {activity = a} :: TerminateInstanceInAutoScalingGroupResponse)

-- | The response's http status code.
terminateInstanceInAutoScalingGroupResponse_httpStatus :: Lens.Lens' TerminateInstanceInAutoScalingGroupResponse Prelude.Int
terminateInstanceInAutoScalingGroupResponse_httpStatus = Lens.lens (\TerminateInstanceInAutoScalingGroupResponse' {httpStatus} -> httpStatus) (\s@TerminateInstanceInAutoScalingGroupResponse' {} a -> s {httpStatus = a} :: TerminateInstanceInAutoScalingGroupResponse)

instance
  Prelude.NFData
    TerminateInstanceInAutoScalingGroupResponse
  where
  rnf TerminateInstanceInAutoScalingGroupResponse' {..} =
    Prelude.rnf activity
      `Prelude.seq` Prelude.rnf httpStatus
