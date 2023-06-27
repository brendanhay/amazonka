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
-- Module      : Amazonka.AutoScaling.EnterStandby
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves the specified instances into the standby state.
--
-- If you choose to decrement the desired capacity of the Auto Scaling
-- group, the instances can enter standby as long as the desired capacity
-- of the Auto Scaling group after the instances are placed into standby is
-- equal to or greater than the minimum capacity of the group.
--
-- If you choose not to decrement the desired capacity of the Auto Scaling
-- group, the Auto Scaling group launches new instances to replace the
-- instances on standby.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enter-exit-standby.html Temporarily removing instances from your Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.EnterStandby
  ( -- * Creating a Request
    EnterStandby (..),
    newEnterStandby,

    -- * Request Lenses
    enterStandby_instanceIds,
    enterStandby_autoScalingGroupName,
    enterStandby_shouldDecrementDesiredCapacity,

    -- * Destructuring the Response
    EnterStandbyResponse (..),
    newEnterStandbyResponse,

    -- * Response Lenses
    enterStandbyResponse_activities,
    enterStandbyResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnterStandby' smart constructor.
data EnterStandby = EnterStandby'
  { -- | The IDs of the instances. You can specify up to 20 instances.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | Indicates whether to decrement the desired capacity of the Auto Scaling
    -- group by the number of instances moved to @Standby@ mode.
    shouldDecrementDesiredCapacity :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnterStandby' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'enterStandby_instanceIds' - The IDs of the instances. You can specify up to 20 instances.
--
-- 'autoScalingGroupName', 'enterStandby_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'shouldDecrementDesiredCapacity', 'enterStandby_shouldDecrementDesiredCapacity' - Indicates whether to decrement the desired capacity of the Auto Scaling
-- group by the number of instances moved to @Standby@ mode.
newEnterStandby ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  -- | 'shouldDecrementDesiredCapacity'
  Prelude.Bool ->
  EnterStandby
newEnterStandby
  pAutoScalingGroupName_
  pShouldDecrementDesiredCapacity_ =
    EnterStandby'
      { instanceIds = Prelude.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        shouldDecrementDesiredCapacity =
          pShouldDecrementDesiredCapacity_
      }

-- | The IDs of the instances. You can specify up to 20 instances.
enterStandby_instanceIds :: Lens.Lens' EnterStandby (Prelude.Maybe [Prelude.Text])
enterStandby_instanceIds = Lens.lens (\EnterStandby' {instanceIds} -> instanceIds) (\s@EnterStandby' {} a -> s {instanceIds = a} :: EnterStandby) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Auto Scaling group.
enterStandby_autoScalingGroupName :: Lens.Lens' EnterStandby Prelude.Text
enterStandby_autoScalingGroupName = Lens.lens (\EnterStandby' {autoScalingGroupName} -> autoScalingGroupName) (\s@EnterStandby' {} a -> s {autoScalingGroupName = a} :: EnterStandby)

-- | Indicates whether to decrement the desired capacity of the Auto Scaling
-- group by the number of instances moved to @Standby@ mode.
enterStandby_shouldDecrementDesiredCapacity :: Lens.Lens' EnterStandby Prelude.Bool
enterStandby_shouldDecrementDesiredCapacity = Lens.lens (\EnterStandby' {shouldDecrementDesiredCapacity} -> shouldDecrementDesiredCapacity) (\s@EnterStandby' {} a -> s {shouldDecrementDesiredCapacity = a} :: EnterStandby)

instance Core.AWSRequest EnterStandby where
  type AWSResponse EnterStandby = EnterStandbyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "EnterStandbyResult"
      ( \s h x ->
          EnterStandbyResponse'
            Prelude.<$> ( x
                            Data..@? "Activities"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnterStandby where
  hashWithSalt _salt EnterStandby' {..} =
    _salt
      `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` shouldDecrementDesiredCapacity

instance Prelude.NFData EnterStandby where
  rnf EnterStandby' {..} =
    Prelude.rnf instanceIds
      `Prelude.seq` Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf shouldDecrementDesiredCapacity

instance Data.ToHeaders EnterStandby where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnterStandby where
  toPath = Prelude.const "/"

instance Data.ToQuery EnterStandby where
  toQuery EnterStandby' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EnterStandby" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "InstanceIds"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> instanceIds),
        "AutoScalingGroupName" Data.=: autoScalingGroupName,
        "ShouldDecrementDesiredCapacity"
          Data.=: shouldDecrementDesiredCapacity
      ]

-- | /See:/ 'newEnterStandbyResponse' smart constructor.
data EnterStandbyResponse = EnterStandbyResponse'
  { -- | The activities related to moving instances into @Standby@ mode.
    activities :: Prelude.Maybe [Activity],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnterStandbyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activities', 'enterStandbyResponse_activities' - The activities related to moving instances into @Standby@ mode.
--
-- 'httpStatus', 'enterStandbyResponse_httpStatus' - The response's http status code.
newEnterStandbyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnterStandbyResponse
newEnterStandbyResponse pHttpStatus_ =
  EnterStandbyResponse'
    { activities = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The activities related to moving instances into @Standby@ mode.
enterStandbyResponse_activities :: Lens.Lens' EnterStandbyResponse (Prelude.Maybe [Activity])
enterStandbyResponse_activities = Lens.lens (\EnterStandbyResponse' {activities} -> activities) (\s@EnterStandbyResponse' {} a -> s {activities = a} :: EnterStandbyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
enterStandbyResponse_httpStatus :: Lens.Lens' EnterStandbyResponse Prelude.Int
enterStandbyResponse_httpStatus = Lens.lens (\EnterStandbyResponse' {httpStatus} -> httpStatus) (\s@EnterStandbyResponse' {} a -> s {httpStatus = a} :: EnterStandbyResponse)

instance Prelude.NFData EnterStandbyResponse where
  rnf EnterStandbyResponse' {..} =
    Prelude.rnf activities
      `Prelude.seq` Prelude.rnf httpStatus
