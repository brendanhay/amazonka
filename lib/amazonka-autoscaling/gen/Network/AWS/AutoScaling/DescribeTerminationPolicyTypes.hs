{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the termination policies supported by Amazon EC2 Auto Scaling.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
  ( -- * Creating a request
    DescribeTerminationPolicyTypes (..),
    mkDescribeTerminationPolicyTypes,

    -- * Destructuring the response
    DescribeTerminationPolicyTypesResponse (..),
    mkDescribeTerminationPolicyTypesResponse,

    -- ** Response lenses
    dtptrrsTerminationPolicyTypes,
    dtptrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTerminationPolicyTypes' smart constructor.
data DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTerminationPolicyTypes' value with any optional fields omitted.
mkDescribeTerminationPolicyTypes ::
  DescribeTerminationPolicyTypes
mkDescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes'

instance Core.AWSRequest DescribeTerminationPolicyTypes where
  type
    Rs DescribeTerminationPolicyTypes =
      DescribeTerminationPolicyTypesResponse
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
            ( Core.pure ("Action", "DescribeTerminationPolicyTypes")
                Core.<> (Core.pure ("Version", "2011-01-01"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeTerminationPolicyTypesResult"
      ( \s h x ->
          DescribeTerminationPolicyTypesResponse'
            Core.<$> ( x Core..@? "TerminationPolicyTypes"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeTerminationPolicyTypesResponse' smart constructor.
data DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse'
  { -- | The termination policies supported by Amazon EC2 Auto Scaling: @OldestInstance@ , @OldestLaunchConfiguration@ , @NewestInstance@ , @ClosestToNextInstanceHour@ , @Default@ , @OldestLaunchTemplate@ , and @AllocationStrategy@ .
    terminationPolicyTypes :: Core.Maybe [Types.XmlStringMaxLen1600],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTerminationPolicyTypesResponse' value with any optional fields omitted.
mkDescribeTerminationPolicyTypesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTerminationPolicyTypesResponse
mkDescribeTerminationPolicyTypesResponse responseStatus =
  DescribeTerminationPolicyTypesResponse'
    { terminationPolicyTypes =
        Core.Nothing,
      responseStatus
    }

-- | The termination policies supported by Amazon EC2 Auto Scaling: @OldestInstance@ , @OldestLaunchConfiguration@ , @NewestInstance@ , @ClosestToNextInstanceHour@ , @Default@ , @OldestLaunchTemplate@ , and @AllocationStrategy@ .
--
-- /Note:/ Consider using 'terminationPolicyTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtptrrsTerminationPolicyTypes :: Lens.Lens' DescribeTerminationPolicyTypesResponse (Core.Maybe [Types.XmlStringMaxLen1600])
dtptrrsTerminationPolicyTypes = Lens.field @"terminationPolicyTypes"
{-# DEPRECATED dtptrrsTerminationPolicyTypes "Use generic-lens or generic-optics with 'terminationPolicyTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtptrrsResponseStatus :: Lens.Lens' DescribeTerminationPolicyTypesResponse Core.Int
dtptrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtptrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
