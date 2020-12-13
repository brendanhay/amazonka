{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.StartInstanceRefresh
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new instance refresh operation, which triggers a rolling replacement of all previously launched instances in the Auto Scaling group with a new group of instances.
--
-- If successful, this call creates a new instance refresh request with a unique ID that you can use to track its progress. To query its status, call the 'DescribeInstanceRefreshes' API. To describe the instance refreshes that have already run, call the 'DescribeInstanceRefreshes' API. To cancel an instance refresh operation in progress, use the 'CancelInstanceRefresh' API.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh.html Replacing Auto Scaling Instances Based on an Instance Refresh> .
module Network.AWS.AutoScaling.StartInstanceRefresh
  ( -- * Creating a request
    StartInstanceRefresh (..),
    mkStartInstanceRefresh,

    -- ** Request lenses
    sirPreferences,
    sirStrategy,
    sirAutoScalingGroupName,

    -- * Destructuring the response
    StartInstanceRefreshResponse (..),
    mkStartInstanceRefreshResponse,

    -- ** Response lenses
    sirrsInstanceRefreshId,
    sirrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartInstanceRefresh' smart constructor.
data StartInstanceRefresh = StartInstanceRefresh'
  { -- | Set of preferences associated with the instance refresh request.
    --
    -- If not provided, the default values are used. For @MinHealthyPercentage@ , the default value is @90@ . For @InstanceWarmup@ , the default is to use the value specified for the health check grace period for the Auto Scaling group.
    -- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_RefreshPreferences.html RefreshPreferences> in the /Amazon EC2 Auto Scaling API Reference/ .
    preferences :: Lude.Maybe RefreshPreferences,
    -- | The strategy to use for the instance refresh. The only valid value is @Rolling@ .
    --
    -- A rolling update is an update that is applied to all instances in an Auto Scaling group until all instances have been updated. A rolling update can fail due to failed health checks or if instances are on standby or are protected from scale in. If the rolling update process fails, any instances that were already replaced are not rolled back to their previous configuration.
    strategy :: Lude.Maybe RefreshStrategy,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartInstanceRefresh' with the minimum fields required to make a request.
--
-- * 'preferences' - Set of preferences associated with the instance refresh request.
--
-- If not provided, the default values are used. For @MinHealthyPercentage@ , the default value is @90@ . For @InstanceWarmup@ , the default is to use the value specified for the health check grace period for the Auto Scaling group.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_RefreshPreferences.html RefreshPreferences> in the /Amazon EC2 Auto Scaling API Reference/ .
-- * 'strategy' - The strategy to use for the instance refresh. The only valid value is @Rolling@ .
--
-- A rolling update is an update that is applied to all instances in an Auto Scaling group until all instances have been updated. A rolling update can fail due to failed health checks or if instances are on standby or are protected from scale in. If the rolling update process fails, any instances that were already replaced are not rolled back to their previous configuration.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
mkStartInstanceRefresh ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  StartInstanceRefresh
mkStartInstanceRefresh pAutoScalingGroupName_ =
  StartInstanceRefresh'
    { preferences = Lude.Nothing,
      strategy = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | Set of preferences associated with the instance refresh request.
--
-- If not provided, the default values are used. For @MinHealthyPercentage@ , the default value is @90@ . For @InstanceWarmup@ , the default is to use the value specified for the health check grace period for the Auto Scaling group.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_RefreshPreferences.html RefreshPreferences> in the /Amazon EC2 Auto Scaling API Reference/ .
--
-- /Note:/ Consider using 'preferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirPreferences :: Lens.Lens' StartInstanceRefresh (Lude.Maybe RefreshPreferences)
sirPreferences = Lens.lens (preferences :: StartInstanceRefresh -> Lude.Maybe RefreshPreferences) (\s a -> s {preferences = a} :: StartInstanceRefresh)
{-# DEPRECATED sirPreferences "Use generic-lens or generic-optics with 'preferences' instead." #-}

-- | The strategy to use for the instance refresh. The only valid value is @Rolling@ .
--
-- A rolling update is an update that is applied to all instances in an Auto Scaling group until all instances have been updated. A rolling update can fail due to failed health checks or if instances are on standby or are protected from scale in. If the rolling update process fails, any instances that were already replaced are not rolled back to their previous configuration.
--
-- /Note:/ Consider using 'strategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirStrategy :: Lens.Lens' StartInstanceRefresh (Lude.Maybe RefreshStrategy)
sirStrategy = Lens.lens (strategy :: StartInstanceRefresh -> Lude.Maybe RefreshStrategy) (\s a -> s {strategy = a} :: StartInstanceRefresh)
{-# DEPRECATED sirStrategy "Use generic-lens or generic-optics with 'strategy' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirAutoScalingGroupName :: Lens.Lens' StartInstanceRefresh Lude.Text
sirAutoScalingGroupName = Lens.lens (autoScalingGroupName :: StartInstanceRefresh -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: StartInstanceRefresh)
{-# DEPRECATED sirAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest StartInstanceRefresh where
  type Rs StartInstanceRefresh = StartInstanceRefreshResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "StartInstanceRefreshResult"
      ( \s h x ->
          StartInstanceRefreshResponse'
            Lude.<$> (x Lude..@? "InstanceRefreshId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartInstanceRefresh where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath StartInstanceRefresh where
  toPath = Lude.const "/"

instance Lude.ToQuery StartInstanceRefresh where
  toQuery StartInstanceRefresh' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("StartInstanceRefresh" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "Preferences" Lude.=: preferences,
        "Strategy" Lude.=: strategy,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkStartInstanceRefreshResponse' smart constructor.
data StartInstanceRefreshResponse = StartInstanceRefreshResponse'
  { -- | A unique ID for tracking the progress of the request.
    instanceRefreshId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartInstanceRefreshResponse' with the minimum fields required to make a request.
--
-- * 'instanceRefreshId' - A unique ID for tracking the progress of the request.
-- * 'responseStatus' - The response status code.
mkStartInstanceRefreshResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartInstanceRefreshResponse
mkStartInstanceRefreshResponse pResponseStatus_ =
  StartInstanceRefreshResponse'
    { instanceRefreshId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique ID for tracking the progress of the request.
--
-- /Note:/ Consider using 'instanceRefreshId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsInstanceRefreshId :: Lens.Lens' StartInstanceRefreshResponse (Lude.Maybe Lude.Text)
sirrsInstanceRefreshId = Lens.lens (instanceRefreshId :: StartInstanceRefreshResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceRefreshId = a} :: StartInstanceRefreshResponse)
{-# DEPRECATED sirrsInstanceRefreshId "Use generic-lens or generic-optics with 'instanceRefreshId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsResponseStatus :: Lens.Lens' StartInstanceRefreshResponse Lude.Int
sirrsResponseStatus = Lens.lens (responseStatus :: StartInstanceRefreshResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartInstanceRefreshResponse)
{-# DEPRECATED sirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
