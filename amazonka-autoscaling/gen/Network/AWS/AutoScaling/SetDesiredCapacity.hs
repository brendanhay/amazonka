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
-- Module      : Network.AWS.AutoScaling.SetDesiredCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the size of the specified Auto Scaling group.
--
-- If a scale-in activity occurs as a result of a new @DesiredCapacity@
-- value that is lower than the current size of the group, the Auto Scaling
-- group uses its termination policy to determine which instances to
-- terminate.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-manual-scaling.html Manual scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Network.AWS.AutoScaling.SetDesiredCapacity
  ( -- * Creating a Request
    SetDesiredCapacity (..),
    newSetDesiredCapacity,

    -- * Request Lenses
    setDesiredCapacity_honorCooldown,
    setDesiredCapacity_autoScalingGroupName,
    setDesiredCapacity_desiredCapacity,

    -- * Destructuring the Response
    SetDesiredCapacityResponse (..),
    newSetDesiredCapacityResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetDesiredCapacity' smart constructor.
data SetDesiredCapacity = SetDesiredCapacity'
  { -- | Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period
    -- to complete before initiating a scaling activity to set your Auto
    -- Scaling group to its new capacity. By default, Amazon EC2 Auto Scaling
    -- does not honor the cooldown period during manual scaling activities.
    honorCooldown :: Core.Maybe Core.Bool,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text,
    -- | The desired capacity is the initial capacity of the Auto Scaling group
    -- after this operation completes and the capacity it attempts to maintain.
    desiredCapacity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetDesiredCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'honorCooldown', 'setDesiredCapacity_honorCooldown' - Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period
-- to complete before initiating a scaling activity to set your Auto
-- Scaling group to its new capacity. By default, Amazon EC2 Auto Scaling
-- does not honor the cooldown period during manual scaling activities.
--
-- 'autoScalingGroupName', 'setDesiredCapacity_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'desiredCapacity', 'setDesiredCapacity_desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group
-- after this operation completes and the capacity it attempts to maintain.
newSetDesiredCapacity ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  -- | 'desiredCapacity'
  Core.Int ->
  SetDesiredCapacity
newSetDesiredCapacity
  pAutoScalingGroupName_
  pDesiredCapacity_ =
    SetDesiredCapacity'
      { honorCooldown = Core.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        desiredCapacity = pDesiredCapacity_
      }

-- | Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period
-- to complete before initiating a scaling activity to set your Auto
-- Scaling group to its new capacity. By default, Amazon EC2 Auto Scaling
-- does not honor the cooldown period during manual scaling activities.
setDesiredCapacity_honorCooldown :: Lens.Lens' SetDesiredCapacity (Core.Maybe Core.Bool)
setDesiredCapacity_honorCooldown = Lens.lens (\SetDesiredCapacity' {honorCooldown} -> honorCooldown) (\s@SetDesiredCapacity' {} a -> s {honorCooldown = a} :: SetDesiredCapacity)

-- | The name of the Auto Scaling group.
setDesiredCapacity_autoScalingGroupName :: Lens.Lens' SetDesiredCapacity Core.Text
setDesiredCapacity_autoScalingGroupName = Lens.lens (\SetDesiredCapacity' {autoScalingGroupName} -> autoScalingGroupName) (\s@SetDesiredCapacity' {} a -> s {autoScalingGroupName = a} :: SetDesiredCapacity)

-- | The desired capacity is the initial capacity of the Auto Scaling group
-- after this operation completes and the capacity it attempts to maintain.
setDesiredCapacity_desiredCapacity :: Lens.Lens' SetDesiredCapacity Core.Int
setDesiredCapacity_desiredCapacity = Lens.lens (\SetDesiredCapacity' {desiredCapacity} -> desiredCapacity) (\s@SetDesiredCapacity' {} a -> s {desiredCapacity = a} :: SetDesiredCapacity)

instance Core.AWSRequest SetDesiredCapacity where
  type
    AWSResponse SetDesiredCapacity =
      SetDesiredCapacityResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull SetDesiredCapacityResponse'

instance Core.Hashable SetDesiredCapacity

instance Core.NFData SetDesiredCapacity

instance Core.ToHeaders SetDesiredCapacity where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SetDesiredCapacity where
  toPath = Core.const "/"

instance Core.ToQuery SetDesiredCapacity where
  toQuery SetDesiredCapacity' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("SetDesiredCapacity" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "HonorCooldown" Core.=: honorCooldown,
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "DesiredCapacity" Core.=: desiredCapacity
      ]

-- | /See:/ 'newSetDesiredCapacityResponse' smart constructor.
data SetDesiredCapacityResponse = SetDesiredCapacityResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetDesiredCapacityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetDesiredCapacityResponse ::
  SetDesiredCapacityResponse
newSetDesiredCapacityResponse =
  SetDesiredCapacityResponse'

instance Core.NFData SetDesiredCapacityResponse
