{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dalrrsMaxNumberOfAutoScalingGroups,
    dalrrsMaxNumberOfLaunchConfigurations,
    dalrrsNumberOfAutoScalingGroups,
    dalrrsNumberOfLaunchConfigurations,
    dalrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAccountLimits' smart constructor.
data DescribeAccountLimits = DescribeAccountLimits'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountLimits' value with any optional fields omitted.
mkDescribeAccountLimits ::
  DescribeAccountLimits
mkDescribeAccountLimits = DescribeAccountLimits'

instance Core.AWSRequest DescribeAccountLimits where
  type Rs DescribeAccountLimits = DescribeAccountLimitsResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeAccountLimits")
                Core.<> (Core.pure ("Version", "2011-01-01"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeAccountLimitsResult"
      ( \s h x ->
          DescribeAccountLimitsResponse'
            Core.<$> (x Core..@? "MaxNumberOfAutoScalingGroups")
            Core.<*> (x Core..@? "MaxNumberOfLaunchConfigurations")
            Core.<*> (x Core..@? "NumberOfAutoScalingGroups")
            Core.<*> (x Core..@? "NumberOfLaunchConfigurations")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
  { -- | The maximum number of groups allowed for your AWS account. The default is 200 groups per AWS Region.
    maxNumberOfAutoScalingGroups :: Core.Maybe Core.Int,
    -- | The maximum number of launch configurations allowed for your AWS account. The default is 200 launch configurations per AWS Region.
    maxNumberOfLaunchConfigurations :: Core.Maybe Core.Int,
    -- | The current number of groups for your AWS account.
    numberOfAutoScalingGroups :: Core.Maybe Core.Int,
    -- | The current number of launch configurations for your AWS account.
    numberOfLaunchConfigurations :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountLimitsResponse' value with any optional fields omitted.
mkDescribeAccountLimitsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAccountLimitsResponse
mkDescribeAccountLimitsResponse responseStatus =
  DescribeAccountLimitsResponse'
    { maxNumberOfAutoScalingGroups =
        Core.Nothing,
      maxNumberOfLaunchConfigurations = Core.Nothing,
      numberOfAutoScalingGroups = Core.Nothing,
      numberOfLaunchConfigurations = Core.Nothing,
      responseStatus
    }

-- | The maximum number of groups allowed for your AWS account. The default is 200 groups per AWS Region.
--
-- /Note:/ Consider using 'maxNumberOfAutoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrrsMaxNumberOfAutoScalingGroups :: Lens.Lens' DescribeAccountLimitsResponse (Core.Maybe Core.Int)
dalrrsMaxNumberOfAutoScalingGroups = Lens.field @"maxNumberOfAutoScalingGroups"
{-# DEPRECATED dalrrsMaxNumberOfAutoScalingGroups "Use generic-lens or generic-optics with 'maxNumberOfAutoScalingGroups' instead." #-}

-- | The maximum number of launch configurations allowed for your AWS account. The default is 200 launch configurations per AWS Region.
--
-- /Note:/ Consider using 'maxNumberOfLaunchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrrsMaxNumberOfLaunchConfigurations :: Lens.Lens' DescribeAccountLimitsResponse (Core.Maybe Core.Int)
dalrrsMaxNumberOfLaunchConfigurations = Lens.field @"maxNumberOfLaunchConfigurations"
{-# DEPRECATED dalrrsMaxNumberOfLaunchConfigurations "Use generic-lens or generic-optics with 'maxNumberOfLaunchConfigurations' instead." #-}

-- | The current number of groups for your AWS account.
--
-- /Note:/ Consider using 'numberOfAutoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrrsNumberOfAutoScalingGroups :: Lens.Lens' DescribeAccountLimitsResponse (Core.Maybe Core.Int)
dalrrsNumberOfAutoScalingGroups = Lens.field @"numberOfAutoScalingGroups"
{-# DEPRECATED dalrrsNumberOfAutoScalingGroups "Use generic-lens or generic-optics with 'numberOfAutoScalingGroups' instead." #-}

-- | The current number of launch configurations for your AWS account.
--
-- /Note:/ Consider using 'numberOfLaunchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrrsNumberOfLaunchConfigurations :: Lens.Lens' DescribeAccountLimitsResponse (Core.Maybe Core.Int)
dalrrsNumberOfLaunchConfigurations = Lens.field @"numberOfLaunchConfigurations"
{-# DEPRECATED dalrrsNumberOfLaunchConfigurations "Use generic-lens or generic-optics with 'numberOfLaunchConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrrsResponseStatus :: Lens.Lens' DescribeAccountLimitsResponse Core.Int
dalrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dalrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
