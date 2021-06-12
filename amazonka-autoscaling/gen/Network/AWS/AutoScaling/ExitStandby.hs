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
-- Module      : Network.AWS.AutoScaling.ExitStandby
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves the specified instances out of the standby state.
--
-- After you put the instances back in service, the desired capacity is
-- incremented.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enter-exit-standby.html Temporarily removing instances from your Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Network.AWS.AutoScaling.ExitStandby
  ( -- * Creating a Request
    ExitStandby (..),
    newExitStandby,

    -- * Request Lenses
    exitStandby_instanceIds,
    exitStandby_autoScalingGroupName,

    -- * Destructuring the Response
    ExitStandbyResponse (..),
    newExitStandbyResponse,

    -- * Response Lenses
    exitStandbyResponse_activities,
    exitStandbyResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExitStandby' smart constructor.
data ExitStandby = ExitStandby'
  { -- | The IDs of the instances. You can specify up to 20 instances.
    instanceIds :: Core.Maybe [Core.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExitStandby' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'exitStandby_instanceIds' - The IDs of the instances. You can specify up to 20 instances.
--
-- 'autoScalingGroupName', 'exitStandby_autoScalingGroupName' - The name of the Auto Scaling group.
newExitStandby ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  ExitStandby
newExitStandby pAutoScalingGroupName_ =
  ExitStandby'
    { instanceIds = Core.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The IDs of the instances. You can specify up to 20 instances.
exitStandby_instanceIds :: Lens.Lens' ExitStandby (Core.Maybe [Core.Text])
exitStandby_instanceIds = Lens.lens (\ExitStandby' {instanceIds} -> instanceIds) (\s@ExitStandby' {} a -> s {instanceIds = a} :: ExitStandby) Core.. Lens.mapping Lens._Coerce

-- | The name of the Auto Scaling group.
exitStandby_autoScalingGroupName :: Lens.Lens' ExitStandby Core.Text
exitStandby_autoScalingGroupName = Lens.lens (\ExitStandby' {autoScalingGroupName} -> autoScalingGroupName) (\s@ExitStandby' {} a -> s {autoScalingGroupName = a} :: ExitStandby)

instance Core.AWSRequest ExitStandby where
  type AWSResponse ExitStandby = ExitStandbyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ExitStandbyResult"
      ( \s h x ->
          ExitStandbyResponse'
            Core.<$> ( x Core..@? "Activities" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ExitStandby

instance Core.NFData ExitStandby

instance Core.ToHeaders ExitStandby where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ExitStandby where
  toPath = Core.const "/"

instance Core.ToQuery ExitStandby where
  toQuery ExitStandby' {..} =
    Core.mconcat
      [ "Action" Core.=: ("ExitStandby" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "InstanceIds"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> instanceIds),
        "AutoScalingGroupName" Core.=: autoScalingGroupName
      ]

-- | /See:/ 'newExitStandbyResponse' smart constructor.
data ExitStandbyResponse = ExitStandbyResponse'
  { -- | The activities related to moving instances out of @Standby@ mode.
    activities :: Core.Maybe [Activity],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExitStandbyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activities', 'exitStandbyResponse_activities' - The activities related to moving instances out of @Standby@ mode.
--
-- 'httpStatus', 'exitStandbyResponse_httpStatus' - The response's http status code.
newExitStandbyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ExitStandbyResponse
newExitStandbyResponse pHttpStatus_ =
  ExitStandbyResponse'
    { activities = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The activities related to moving instances out of @Standby@ mode.
exitStandbyResponse_activities :: Lens.Lens' ExitStandbyResponse (Core.Maybe [Activity])
exitStandbyResponse_activities = Lens.lens (\ExitStandbyResponse' {activities} -> activities) (\s@ExitStandbyResponse' {} a -> s {activities = a} :: ExitStandbyResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
exitStandbyResponse_httpStatus :: Lens.Lens' ExitStandbyResponse Core.Int
exitStandbyResponse_httpStatus = Lens.lens (\ExitStandbyResponse' {httpStatus} -> httpStatus) (\s@ExitStandbyResponse' {} a -> s {httpStatus = a} :: ExitStandbyResponse)

instance Core.NFData ExitStandbyResponse
