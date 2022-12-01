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
-- Module      : Amazonka.AutoScaling.DetachInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more instances from the specified Auto Scaling group.
--
-- After the instances are detached, you can manage them independent of the
-- Auto Scaling group.
--
-- If you do not specify the option to decrement the desired capacity,
-- Amazon EC2 Auto Scaling launches instances to replace the ones that are
-- detached.
--
-- If there is a Classic Load Balancer attached to the Auto Scaling group,
-- the instances are deregistered from the load balancer. If there are
-- target groups attached to the Auto Scaling group, the instances are
-- deregistered from the target groups.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/detach-instance-asg.html Detach EC2 instances from your Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.DetachInstances
  ( -- * Creating a Request
    DetachInstances (..),
    newDetachInstances,

    -- * Request Lenses
    detachInstances_instanceIds,
    detachInstances_autoScalingGroupName,
    detachInstances_shouldDecrementDesiredCapacity,

    -- * Destructuring the Response
    DetachInstancesResponse (..),
    newDetachInstancesResponse,

    -- * Response Lenses
    detachInstancesResponse_activities,
    detachInstancesResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetachInstances' smart constructor.
data DetachInstances = DetachInstances'
  { -- | The IDs of the instances. You can specify up to 20 instances.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | Indicates whether the Auto Scaling group decrements the desired capacity
    -- value by the number of instances detached.
    shouldDecrementDesiredCapacity :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'detachInstances_instanceIds' - The IDs of the instances. You can specify up to 20 instances.
--
-- 'autoScalingGroupName', 'detachInstances_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'shouldDecrementDesiredCapacity', 'detachInstances_shouldDecrementDesiredCapacity' - Indicates whether the Auto Scaling group decrements the desired capacity
-- value by the number of instances detached.
newDetachInstances ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  -- | 'shouldDecrementDesiredCapacity'
  Prelude.Bool ->
  DetachInstances
newDetachInstances
  pAutoScalingGroupName_
  pShouldDecrementDesiredCapacity_ =
    DetachInstances'
      { instanceIds = Prelude.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        shouldDecrementDesiredCapacity =
          pShouldDecrementDesiredCapacity_
      }

-- | The IDs of the instances. You can specify up to 20 instances.
detachInstances_instanceIds :: Lens.Lens' DetachInstances (Prelude.Maybe [Prelude.Text])
detachInstances_instanceIds = Lens.lens (\DetachInstances' {instanceIds} -> instanceIds) (\s@DetachInstances' {} a -> s {instanceIds = a} :: DetachInstances) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Auto Scaling group.
detachInstances_autoScalingGroupName :: Lens.Lens' DetachInstances Prelude.Text
detachInstances_autoScalingGroupName = Lens.lens (\DetachInstances' {autoScalingGroupName} -> autoScalingGroupName) (\s@DetachInstances' {} a -> s {autoScalingGroupName = a} :: DetachInstances)

-- | Indicates whether the Auto Scaling group decrements the desired capacity
-- value by the number of instances detached.
detachInstances_shouldDecrementDesiredCapacity :: Lens.Lens' DetachInstances Prelude.Bool
detachInstances_shouldDecrementDesiredCapacity = Lens.lens (\DetachInstances' {shouldDecrementDesiredCapacity} -> shouldDecrementDesiredCapacity) (\s@DetachInstances' {} a -> s {shouldDecrementDesiredCapacity = a} :: DetachInstances)

instance Core.AWSRequest DetachInstances where
  type
    AWSResponse DetachInstances =
      DetachInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DetachInstancesResult"
      ( \s h x ->
          DetachInstancesResponse'
            Prelude.<$> ( x Core..@? "Activities" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetachInstances where
  hashWithSalt _salt DetachInstances' {..} =
    _salt `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` shouldDecrementDesiredCapacity

instance Prelude.NFData DetachInstances where
  rnf DetachInstances' {..} =
    Prelude.rnf instanceIds
      `Prelude.seq` Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf shouldDecrementDesiredCapacity

instance Core.ToHeaders DetachInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DetachInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery DetachInstances where
  toQuery DetachInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DetachInstances" :: Prelude.ByteString),
        "Version"
          Core.=: ("2011-01-01" :: Prelude.ByteString),
        "InstanceIds"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> instanceIds),
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "ShouldDecrementDesiredCapacity"
          Core.=: shouldDecrementDesiredCapacity
      ]

-- | /See:/ 'newDetachInstancesResponse' smart constructor.
data DetachInstancesResponse = DetachInstancesResponse'
  { -- | The activities related to detaching the instances from the Auto Scaling
    -- group.
    activities :: Prelude.Maybe [Activity],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activities', 'detachInstancesResponse_activities' - The activities related to detaching the instances from the Auto Scaling
-- group.
--
-- 'httpStatus', 'detachInstancesResponse_httpStatus' - The response's http status code.
newDetachInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetachInstancesResponse
newDetachInstancesResponse pHttpStatus_ =
  DetachInstancesResponse'
    { activities =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The activities related to detaching the instances from the Auto Scaling
-- group.
detachInstancesResponse_activities :: Lens.Lens' DetachInstancesResponse (Prelude.Maybe [Activity])
detachInstancesResponse_activities = Lens.lens (\DetachInstancesResponse' {activities} -> activities) (\s@DetachInstancesResponse' {} a -> s {activities = a} :: DetachInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
detachInstancesResponse_httpStatus :: Lens.Lens' DetachInstancesResponse Prelude.Int
detachInstancesResponse_httpStatus = Lens.lens (\DetachInstancesResponse' {httpStatus} -> httpStatus) (\s@DetachInstancesResponse' {} a -> s {httpStatus = a} :: DetachInstancesResponse)

instance Prelude.NFData DetachInstancesResponse where
  rnf DetachInstancesResponse' {..} =
    Prelude.rnf activities
      `Prelude.seq` Prelude.rnf httpStatus
