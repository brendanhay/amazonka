{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.SetLoadBasedAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the load-based auto scaling configuration for a specified layer. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html Managing Load with Time-based and Load-based Instances> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.SetLoadBasedAutoScaling
  ( -- * Creating a request
    SetLoadBasedAutoScaling (..),
    mkSetLoadBasedAutoScaling,

    -- ** Request lenses
    slbasUpScaling,
    slbasEnable,
    slbasDownScaling,
    slbasLayerId,

    -- * Destructuring the response
    SetLoadBasedAutoScalingResponse (..),
    mkSetLoadBasedAutoScalingResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetLoadBasedAutoScaling' smart constructor.
data SetLoadBasedAutoScaling = SetLoadBasedAutoScaling'
  { -- | An @AutoScalingThresholds@ object with the upscaling threshold configuration. If the load exceeds these thresholds for a specified amount of time, AWS OpsWorks Stacks starts a specified number of instances.
    upScaling :: Lude.Maybe AutoScalingThresholds,
    -- | Enables load-based auto scaling for the layer.
    enable :: Lude.Maybe Lude.Bool,
    -- | An @AutoScalingThresholds@ object with the downscaling threshold configuration. If the load falls below these thresholds for a specified amount of time, AWS OpsWorks Stacks stops a specified number of instances.
    downScaling :: Lude.Maybe AutoScalingThresholds,
    -- | The layer ID.
    layerId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetLoadBasedAutoScaling' with the minimum fields required to make a request.
--
-- * 'upScaling' - An @AutoScalingThresholds@ object with the upscaling threshold configuration. If the load exceeds these thresholds for a specified amount of time, AWS OpsWorks Stacks starts a specified number of instances.
-- * 'enable' - Enables load-based auto scaling for the layer.
-- * 'downScaling' - An @AutoScalingThresholds@ object with the downscaling threshold configuration. If the load falls below these thresholds for a specified amount of time, AWS OpsWorks Stacks stops a specified number of instances.
-- * 'layerId' - The layer ID.
mkSetLoadBasedAutoScaling ::
  -- | 'layerId'
  Lude.Text ->
  SetLoadBasedAutoScaling
mkSetLoadBasedAutoScaling pLayerId_ =
  SetLoadBasedAutoScaling'
    { upScaling = Lude.Nothing,
      enable = Lude.Nothing,
      downScaling = Lude.Nothing,
      layerId = pLayerId_
    }

-- | An @AutoScalingThresholds@ object with the upscaling threshold configuration. If the load exceeds these thresholds for a specified amount of time, AWS OpsWorks Stacks starts a specified number of instances.
--
-- /Note:/ Consider using 'upScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbasUpScaling :: Lens.Lens' SetLoadBasedAutoScaling (Lude.Maybe AutoScalingThresholds)
slbasUpScaling = Lens.lens (upScaling :: SetLoadBasedAutoScaling -> Lude.Maybe AutoScalingThresholds) (\s a -> s {upScaling = a} :: SetLoadBasedAutoScaling)
{-# DEPRECATED slbasUpScaling "Use generic-lens or generic-optics with 'upScaling' instead." #-}

-- | Enables load-based auto scaling for the layer.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbasEnable :: Lens.Lens' SetLoadBasedAutoScaling (Lude.Maybe Lude.Bool)
slbasEnable = Lens.lens (enable :: SetLoadBasedAutoScaling -> Lude.Maybe Lude.Bool) (\s a -> s {enable = a} :: SetLoadBasedAutoScaling)
{-# DEPRECATED slbasEnable "Use generic-lens or generic-optics with 'enable' instead." #-}

-- | An @AutoScalingThresholds@ object with the downscaling threshold configuration. If the load falls below these thresholds for a specified amount of time, AWS OpsWorks Stacks stops a specified number of instances.
--
-- /Note:/ Consider using 'downScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbasDownScaling :: Lens.Lens' SetLoadBasedAutoScaling (Lude.Maybe AutoScalingThresholds)
slbasDownScaling = Lens.lens (downScaling :: SetLoadBasedAutoScaling -> Lude.Maybe AutoScalingThresholds) (\s a -> s {downScaling = a} :: SetLoadBasedAutoScaling)
{-# DEPRECATED slbasDownScaling "Use generic-lens or generic-optics with 'downScaling' instead." #-}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbasLayerId :: Lens.Lens' SetLoadBasedAutoScaling Lude.Text
slbasLayerId = Lens.lens (layerId :: SetLoadBasedAutoScaling -> Lude.Text) (\s a -> s {layerId = a} :: SetLoadBasedAutoScaling)
{-# DEPRECATED slbasLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

instance Lude.AWSRequest SetLoadBasedAutoScaling where
  type Rs SetLoadBasedAutoScaling = SetLoadBasedAutoScalingResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull SetLoadBasedAutoScalingResponse'

instance Lude.ToHeaders SetLoadBasedAutoScaling where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.SetLoadBasedAutoScaling" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetLoadBasedAutoScaling where
  toJSON SetLoadBasedAutoScaling' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UpScaling" Lude..=) Lude.<$> upScaling,
            ("Enable" Lude..=) Lude.<$> enable,
            ("DownScaling" Lude..=) Lude.<$> downScaling,
            Lude.Just ("LayerId" Lude..= layerId)
          ]
      )

instance Lude.ToPath SetLoadBasedAutoScaling where
  toPath = Lude.const "/"

instance Lude.ToQuery SetLoadBasedAutoScaling where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetLoadBasedAutoScalingResponse' smart constructor.
data SetLoadBasedAutoScalingResponse = SetLoadBasedAutoScalingResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetLoadBasedAutoScalingResponse' with the minimum fields required to make a request.
mkSetLoadBasedAutoScalingResponse ::
  SetLoadBasedAutoScalingResponse
mkSetLoadBasedAutoScalingResponse =
  SetLoadBasedAutoScalingResponse'
