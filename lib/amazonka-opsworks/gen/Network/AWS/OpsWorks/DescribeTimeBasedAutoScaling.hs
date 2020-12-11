{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes time-based auto scaling configurations for specified instances.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
  ( -- * Creating a request
    DescribeTimeBasedAutoScaling (..),
    mkDescribeTimeBasedAutoScaling,

    -- ** Request lenses
    dtbasInstanceIds,

    -- * Destructuring the response
    DescribeTimeBasedAutoScalingResponse (..),
    mkDescribeTimeBasedAutoScalingResponse,

    -- ** Response lenses
    dtbasrsTimeBasedAutoScalingConfigurations,
    dtbasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTimeBasedAutoScaling' smart constructor.
newtype DescribeTimeBasedAutoScaling = DescribeTimeBasedAutoScaling'
  { instanceIds ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTimeBasedAutoScaling' with the minimum fields required to make a request.
--
-- * 'instanceIds' - An array of instance IDs.
mkDescribeTimeBasedAutoScaling ::
  DescribeTimeBasedAutoScaling
mkDescribeTimeBasedAutoScaling =
  DescribeTimeBasedAutoScaling' {instanceIds = Lude.mempty}

-- | An array of instance IDs.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtbasInstanceIds :: Lens.Lens' DescribeTimeBasedAutoScaling [Lude.Text]
dtbasInstanceIds = Lens.lens (instanceIds :: DescribeTimeBasedAutoScaling -> [Lude.Text]) (\s a -> s {instanceIds = a} :: DescribeTimeBasedAutoScaling)
{-# DEPRECATED dtbasInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

instance Lude.AWSRequest DescribeTimeBasedAutoScaling where
  type
    Rs DescribeTimeBasedAutoScaling =
      DescribeTimeBasedAutoScalingResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTimeBasedAutoScalingResponse'
            Lude.<$> ( x Lude..?> "TimeBasedAutoScalingConfigurations"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTimeBasedAutoScaling where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OpsWorks_20130218.DescribeTimeBasedAutoScaling" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTimeBasedAutoScaling where
  toJSON DescribeTimeBasedAutoScaling' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("InstanceIds" Lude..= instanceIds)])

instance Lude.ToPath DescribeTimeBasedAutoScaling where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTimeBasedAutoScaling where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeTimeBasedAutoScaling@ request.
--
-- /See:/ 'mkDescribeTimeBasedAutoScalingResponse' smart constructor.
data DescribeTimeBasedAutoScalingResponse = DescribeTimeBasedAutoScalingResponse'
  { timeBasedAutoScalingConfigurations ::
      Lude.Maybe
        [TimeBasedAutoScalingConfiguration],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTimeBasedAutoScalingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'timeBasedAutoScalingConfigurations' - An array of @TimeBasedAutoScalingConfiguration@ objects that describe the configuration for the specified instances.
mkDescribeTimeBasedAutoScalingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTimeBasedAutoScalingResponse
mkDescribeTimeBasedAutoScalingResponse pResponseStatus_ =
  DescribeTimeBasedAutoScalingResponse'
    { timeBasedAutoScalingConfigurations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @TimeBasedAutoScalingConfiguration@ objects that describe the configuration for the specified instances.
--
-- /Note:/ Consider using 'timeBasedAutoScalingConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtbasrsTimeBasedAutoScalingConfigurations :: Lens.Lens' DescribeTimeBasedAutoScalingResponse (Lude.Maybe [TimeBasedAutoScalingConfiguration])
dtbasrsTimeBasedAutoScalingConfigurations = Lens.lens (timeBasedAutoScalingConfigurations :: DescribeTimeBasedAutoScalingResponse -> Lude.Maybe [TimeBasedAutoScalingConfiguration]) (\s a -> s {timeBasedAutoScalingConfigurations = a} :: DescribeTimeBasedAutoScalingResponse)
{-# DEPRECATED dtbasrsTimeBasedAutoScalingConfigurations "Use generic-lens or generic-optics with 'timeBasedAutoScalingConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtbasrsResponseStatus :: Lens.Lens' DescribeTimeBasedAutoScalingResponse Lude.Int
dtbasrsResponseStatus = Lens.lens (responseStatus :: DescribeTimeBasedAutoScalingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTimeBasedAutoScalingResponse)
{-# DEPRECATED dtbasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
