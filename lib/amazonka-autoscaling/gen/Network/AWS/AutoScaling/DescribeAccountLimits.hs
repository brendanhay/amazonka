{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAccountLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current Amazon EC2 Auto Scaling resource quotas for your AWS account.
--
-- For information about requesting an increase, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-account-limits.html Amazon EC2 Auto Scaling service quotas> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.DescribeAccountLimits
  ( -- * Creating a request
    DescribeAccountLimits (..),
    mkDescribeAccountLimits,

    -- * Destructuring the response
    DescribeAccountLimitsResponse (..),
    mkDescribeAccountLimitsResponse,

    -- ** Response lenses
    dalrsNumberOfLaunchConfigurations,
    dalrsNumberOfAutoScalingGroups,
    dalrsMaxNumberOfAutoScalingGroups,
    dalrsMaxNumberOfLaunchConfigurations,
    dalrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAccountLimits' smart constructor.
data DescribeAccountLimits = DescribeAccountLimits'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountLimits' with the minimum fields required to make a request.
mkDescribeAccountLimits ::
  DescribeAccountLimits
mkDescribeAccountLimits = DescribeAccountLimits'

instance Lude.AWSRequest DescribeAccountLimits where
  type Rs DescribeAccountLimits = DescribeAccountLimitsResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeAccountLimitsResult"
      ( \s h x ->
          DescribeAccountLimitsResponse'
            Lude.<$> (x Lude..@? "NumberOfLaunchConfigurations")
            Lude.<*> (x Lude..@? "NumberOfAutoScalingGroups")
            Lude.<*> (x Lude..@? "MaxNumberOfAutoScalingGroups")
            Lude.<*> (x Lude..@? "MaxNumberOfLaunchConfigurations")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccountLimits where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAccountLimits where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAccountLimits where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("DescribeAccountLimits" :: Lude.ByteString),
            "Version" Lude.=: ("2011-01-01" :: Lude.ByteString)
          ]
      )

-- | /See:/ 'mkDescribeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
  { numberOfLaunchConfigurations ::
      Lude.Maybe Lude.Int,
    numberOfAutoScalingGroups ::
      Lude.Maybe Lude.Int,
    maxNumberOfAutoScalingGroups ::
      Lude.Maybe Lude.Int,
    maxNumberOfLaunchConfigurations ::
      Lude.Maybe Lude.Int,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountLimitsResponse' with the minimum fields required to make a request.
--
-- * 'maxNumberOfAutoScalingGroups' - The maximum number of groups allowed for your AWS account. The default is 200 groups per AWS Region.
-- * 'maxNumberOfLaunchConfigurations' - The maximum number of launch configurations allowed for your AWS account. The default is 200 launch configurations per AWS Region.
-- * 'numberOfAutoScalingGroups' - The current number of groups for your AWS account.
-- * 'numberOfLaunchConfigurations' - The current number of launch configurations for your AWS account.
-- * 'responseStatus' - The response status code.
mkDescribeAccountLimitsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccountLimitsResponse
mkDescribeAccountLimitsResponse pResponseStatus_ =
  DescribeAccountLimitsResponse'
    { numberOfLaunchConfigurations =
        Lude.Nothing,
      numberOfAutoScalingGroups = Lude.Nothing,
      maxNumberOfAutoScalingGroups = Lude.Nothing,
      maxNumberOfLaunchConfigurations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current number of launch configurations for your AWS account.
--
-- /Note:/ Consider using 'numberOfLaunchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrsNumberOfLaunchConfigurations :: Lens.Lens' DescribeAccountLimitsResponse (Lude.Maybe Lude.Int)
dalrsNumberOfLaunchConfigurations = Lens.lens (numberOfLaunchConfigurations :: DescribeAccountLimitsResponse -> Lude.Maybe Lude.Int) (\s a -> s {numberOfLaunchConfigurations = a} :: DescribeAccountLimitsResponse)
{-# DEPRECATED dalrsNumberOfLaunchConfigurations "Use generic-lens or generic-optics with 'numberOfLaunchConfigurations' instead." #-}

-- | The current number of groups for your AWS account.
--
-- /Note:/ Consider using 'numberOfAutoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrsNumberOfAutoScalingGroups :: Lens.Lens' DescribeAccountLimitsResponse (Lude.Maybe Lude.Int)
dalrsNumberOfAutoScalingGroups = Lens.lens (numberOfAutoScalingGroups :: DescribeAccountLimitsResponse -> Lude.Maybe Lude.Int) (\s a -> s {numberOfAutoScalingGroups = a} :: DescribeAccountLimitsResponse)
{-# DEPRECATED dalrsNumberOfAutoScalingGroups "Use generic-lens or generic-optics with 'numberOfAutoScalingGroups' instead." #-}

-- | The maximum number of groups allowed for your AWS account. The default is 200 groups per AWS Region.
--
-- /Note:/ Consider using 'maxNumberOfAutoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrsMaxNumberOfAutoScalingGroups :: Lens.Lens' DescribeAccountLimitsResponse (Lude.Maybe Lude.Int)
dalrsMaxNumberOfAutoScalingGroups = Lens.lens (maxNumberOfAutoScalingGroups :: DescribeAccountLimitsResponse -> Lude.Maybe Lude.Int) (\s a -> s {maxNumberOfAutoScalingGroups = a} :: DescribeAccountLimitsResponse)
{-# DEPRECATED dalrsMaxNumberOfAutoScalingGroups "Use generic-lens or generic-optics with 'maxNumberOfAutoScalingGroups' instead." #-}

-- | The maximum number of launch configurations allowed for your AWS account. The default is 200 launch configurations per AWS Region.
--
-- /Note:/ Consider using 'maxNumberOfLaunchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrsMaxNumberOfLaunchConfigurations :: Lens.Lens' DescribeAccountLimitsResponse (Lude.Maybe Lude.Int)
dalrsMaxNumberOfLaunchConfigurations = Lens.lens (maxNumberOfLaunchConfigurations :: DescribeAccountLimitsResponse -> Lude.Maybe Lude.Int) (\s a -> s {maxNumberOfLaunchConfigurations = a} :: DescribeAccountLimitsResponse)
{-# DEPRECATED dalrsMaxNumberOfLaunchConfigurations "Use generic-lens or generic-optics with 'maxNumberOfLaunchConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrsResponseStatus :: Lens.Lens' DescribeAccountLimitsResponse Lude.Int
dalrsResponseStatus = Lens.lens (responseStatus :: DescribeAccountLimitsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccountLimitsResponse)
{-# DEPRECATED dalrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
