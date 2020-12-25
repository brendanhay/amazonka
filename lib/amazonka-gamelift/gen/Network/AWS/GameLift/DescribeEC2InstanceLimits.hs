{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeEC2InstanceLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the following information for the specified EC2 instance type:
--
--
--     * Maximum number of instances allowed per AWS account (service limit).
--
--
--     * Current usage for the AWS account.
--
--
-- To learn more about the capabilities of each instance type, see <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> . Note that the instance types offered may vary depending on the region.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
-- __Related operations__
--
--     * 'CreateFleet'
--
--
--     * 'ListFleets'
--
--
--     * 'DeleteFleet'
--
--
--     * 'DescribeFleetAttributes'
--
--
--     * 'UpdateFleetAttributes'
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
module Network.AWS.GameLift.DescribeEC2InstanceLimits
  ( -- * Creating a request
    DescribeEC2InstanceLimits (..),
    mkDescribeEC2InstanceLimits,

    -- ** Request lenses
    decilEC2InstanceType,

    -- * Destructuring the response
    DescribeEC2InstanceLimitsResponse (..),
    mkDescribeEC2InstanceLimitsResponse,

    -- ** Response lenses
    decilrrsEC2InstanceLimits,
    decilrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeEC2InstanceLimits' smart constructor.
newtype DescribeEC2InstanceLimits = DescribeEC2InstanceLimits'
  { -- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions. Leave this parameter blank to retrieve limits for all types.
    eC2InstanceType :: Core.Maybe Types.EC2InstanceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEC2InstanceLimits' value with any optional fields omitted.
mkDescribeEC2InstanceLimits ::
  DescribeEC2InstanceLimits
mkDescribeEC2InstanceLimits =
  DescribeEC2InstanceLimits' {eC2InstanceType = Core.Nothing}

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions. Leave this parameter blank to retrieve limits for all types.
--
-- /Note:/ Consider using 'eC2InstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decilEC2InstanceType :: Lens.Lens' DescribeEC2InstanceLimits (Core.Maybe Types.EC2InstanceType)
decilEC2InstanceType = Lens.field @"eC2InstanceType"
{-# DEPRECATED decilEC2InstanceType "Use generic-lens or generic-optics with 'eC2InstanceType' instead." #-}

instance Core.FromJSON DescribeEC2InstanceLimits where
  toJSON DescribeEC2InstanceLimits {..} =
    Core.object
      ( Core.catMaybes
          [("EC2InstanceType" Core..=) Core.<$> eC2InstanceType]
      )

instance Core.AWSRequest DescribeEC2InstanceLimits where
  type
    Rs DescribeEC2InstanceLimits =
      DescribeEC2InstanceLimitsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DescribeEC2InstanceLimits")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEC2InstanceLimitsResponse'
            Core.<$> (x Core..:? "EC2InstanceLimits")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeEC2InstanceLimitsResponse' smart constructor.
data DescribeEC2InstanceLimitsResponse = DescribeEC2InstanceLimitsResponse'
  { -- | The maximum number of instances for the specified instance type.
    eC2InstanceLimits :: Core.Maybe [Types.EC2InstanceLimit],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEC2InstanceLimitsResponse' value with any optional fields omitted.
mkDescribeEC2InstanceLimitsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEC2InstanceLimitsResponse
mkDescribeEC2InstanceLimitsResponse responseStatus =
  DescribeEC2InstanceLimitsResponse'
    { eC2InstanceLimits =
        Core.Nothing,
      responseStatus
    }

-- | The maximum number of instances for the specified instance type.
--
-- /Note:/ Consider using 'eC2InstanceLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decilrrsEC2InstanceLimits :: Lens.Lens' DescribeEC2InstanceLimitsResponse (Core.Maybe [Types.EC2InstanceLimit])
decilrrsEC2InstanceLimits = Lens.field @"eC2InstanceLimits"
{-# DEPRECATED decilrrsEC2InstanceLimits "Use generic-lens or generic-optics with 'eC2InstanceLimits' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decilrrsResponseStatus :: Lens.Lens' DescribeEC2InstanceLimitsResponse Core.Int
decilrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED decilrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
