{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches an EC2 Fleet.
--
-- You can create a single EC2 Fleet that includes multiple launch specifications that vary by instance type, AMI, Availability Zone, or subnet.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet.html Launching an EC2 Fleet> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateFleet
  ( -- * Creating a request
    CreateFleet (..),
    mkCreateFleet,

    -- ** Request lenses
    cfLaunchTemplateConfigs,
    cfTargetCapacitySpecification,
    cfClientToken,
    cfDryRun,
    cfExcessCapacityTerminationPolicy,
    cfOnDemandOptions,
    cfReplaceUnhealthyInstances,
    cfSpotOptions,
    cfTagSpecifications,
    cfTerminateInstancesWithExpiration,
    cfType,
    cfValidFrom,
    cfValidUntil,

    -- * Destructuring the response
    CreateFleetResponse (..),
    mkCreateFleetResponse,

    -- ** Response lenses
    cfrrsErrors,
    cfrrsFleetId,
    cfrrsInstances,
    cfrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { -- | The configuration for the EC2 Fleet.
    launchTemplateConfigs :: [Types.FleetLaunchTemplateConfigRequest],
    -- | The number of units to request.
    targetCapacitySpecification :: Types.TargetCapacitySpecificationRequest,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    clientToken :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
    excessCapacityTerminationPolicy :: Core.Maybe Types.FleetExcessCapacityTerminationPolicy,
    -- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
    onDemandOptions :: Core.Maybe Types.OnDemandOptionsRequest,
    -- | Indicates whether EC2 Fleet should replace unhealthy instances.
    replaceUnhealthyInstances :: Core.Maybe Core.Bool,
    -- | Describes the configuration of Spot Instances in an EC2 Fleet.
    spotOptions :: Core.Maybe Types.SpotOptionsRequest,
    -- | The key-value pair for tagging the EC2 Fleet request on creation. The value for @ResourceType@ must be @fleet@ , otherwise the fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> . For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging your resources> .
    tagSpecifications :: Core.Maybe [Types.TagSpecification],
    -- | Indicates whether running instances should be terminated when the EC2 Fleet expires.
    terminateInstancesWithExpiration :: Core.Maybe Core.Bool,
    -- | The type of request. The default value is @maintain@ .
    --
    --
    --     * @maintain@ - The EC2 Fleet plaees an asynchronous request for your desired capacity, and continues to maintain your desired Spot capacity by replenishing interrupted Spot Instances.
    --
    --
    --     * @request@ - The EC2 Fleet places an asynchronous one-time request for your desired capacity, but does submit Spot requests in alternative capacity pools if Spot capacity is unavailable, and does not maintain Spot capacity if Spot Instances are interrupted.
    --
    --
    --     * @instant@ - The EC2 Fleet places a synchronous one-time request for your desired capacity, and returns errors for any instances that could not be launched.
    --
    --
    -- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#ec2-fleet-request-type EC2 Fleet request types> in the /Amazon Elastic Compute Cloud User Guide/ .
    type' :: Core.Maybe Types.FleetType,
    -- | The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
    validFrom :: Core.Maybe Core.UTCTime,
    -- | The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new EC2 Fleet requests are placed or able to fulfill the request. If no value is specified, the request remains until you cancel it.
    validUntil :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateFleet' value with any optional fields omitted.
mkCreateFleet ::
  -- | 'targetCapacitySpecification'
  Types.TargetCapacitySpecificationRequest ->
  CreateFleet
mkCreateFleet targetCapacitySpecification =
  CreateFleet'
    { launchTemplateConfigs = Core.mempty,
      targetCapacitySpecification,
      clientToken = Core.Nothing,
      dryRun = Core.Nothing,
      excessCapacityTerminationPolicy = Core.Nothing,
      onDemandOptions = Core.Nothing,
      replaceUnhealthyInstances = Core.Nothing,
      spotOptions = Core.Nothing,
      tagSpecifications = Core.Nothing,
      terminateInstancesWithExpiration = Core.Nothing,
      type' = Core.Nothing,
      validFrom = Core.Nothing,
      validUntil = Core.Nothing
    }

-- | The configuration for the EC2 Fleet.
--
-- /Note:/ Consider using 'launchTemplateConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLaunchTemplateConfigs :: Lens.Lens' CreateFleet [Types.FleetLaunchTemplateConfigRequest]
cfLaunchTemplateConfigs = Lens.field @"launchTemplateConfigs"
{-# DEPRECATED cfLaunchTemplateConfigs "Use generic-lens or generic-optics with 'launchTemplateConfigs' instead." #-}

-- | The number of units to request.
--
-- /Note:/ Consider using 'targetCapacitySpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTargetCapacitySpecification :: Lens.Lens' CreateFleet Types.TargetCapacitySpecificationRequest
cfTargetCapacitySpecification = Lens.field @"targetCapacitySpecification"
{-# DEPRECATED cfTargetCapacitySpecification "Use generic-lens or generic-optics with 'targetCapacitySpecification' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfClientToken :: Lens.Lens' CreateFleet (Core.Maybe Types.String)
cfClientToken = Lens.field @"clientToken"
{-# DEPRECATED cfClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDryRun :: Lens.Lens' CreateFleet (Core.Maybe Core.Bool)
cfDryRun = Lens.field @"dryRun"
{-# DEPRECATED cfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
--
-- /Note:/ Consider using 'excessCapacityTerminationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfExcessCapacityTerminationPolicy :: Lens.Lens' CreateFleet (Core.Maybe Types.FleetExcessCapacityTerminationPolicy)
cfExcessCapacityTerminationPolicy = Lens.field @"excessCapacityTerminationPolicy"
{-# DEPRECATED cfExcessCapacityTerminationPolicy "Use generic-lens or generic-optics with 'excessCapacityTerminationPolicy' instead." #-}

-- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
--
-- /Note:/ Consider using 'onDemandOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfOnDemandOptions :: Lens.Lens' CreateFleet (Core.Maybe Types.OnDemandOptionsRequest)
cfOnDemandOptions = Lens.field @"onDemandOptions"
{-# DEPRECATED cfOnDemandOptions "Use generic-lens or generic-optics with 'onDemandOptions' instead." #-}

-- | Indicates whether EC2 Fleet should replace unhealthy instances.
--
-- /Note:/ Consider using 'replaceUnhealthyInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfReplaceUnhealthyInstances :: Lens.Lens' CreateFleet (Core.Maybe Core.Bool)
cfReplaceUnhealthyInstances = Lens.field @"replaceUnhealthyInstances"
{-# DEPRECATED cfReplaceUnhealthyInstances "Use generic-lens or generic-optics with 'replaceUnhealthyInstances' instead." #-}

-- | Describes the configuration of Spot Instances in an EC2 Fleet.
--
-- /Note:/ Consider using 'spotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSpotOptions :: Lens.Lens' CreateFleet (Core.Maybe Types.SpotOptionsRequest)
cfSpotOptions = Lens.field @"spotOptions"
{-# DEPRECATED cfSpotOptions "Use generic-lens or generic-optics with 'spotOptions' instead." #-}

-- | The key-value pair for tagging the EC2 Fleet request on creation. The value for @ResourceType@ must be @fleet@ , otherwise the fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> . For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging your resources> .
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTagSpecifications :: Lens.Lens' CreateFleet (Core.Maybe [Types.TagSpecification])
cfTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED cfTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Indicates whether running instances should be terminated when the EC2 Fleet expires.
--
-- /Note:/ Consider using 'terminateInstancesWithExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTerminateInstancesWithExpiration :: Lens.Lens' CreateFleet (Core.Maybe Core.Bool)
cfTerminateInstancesWithExpiration = Lens.field @"terminateInstancesWithExpiration"
{-# DEPRECATED cfTerminateInstancesWithExpiration "Use generic-lens or generic-optics with 'terminateInstancesWithExpiration' instead." #-}

-- | The type of request. The default value is @maintain@ .
--
--
--     * @maintain@ - The EC2 Fleet plaees an asynchronous request for your desired capacity, and continues to maintain your desired Spot capacity by replenishing interrupted Spot Instances.
--
--
--     * @request@ - The EC2 Fleet places an asynchronous one-time request for your desired capacity, but does submit Spot requests in alternative capacity pools if Spot capacity is unavailable, and does not maintain Spot capacity if Spot Instances are interrupted.
--
--
--     * @instant@ - The EC2 Fleet places a synchronous one-time request for your desired capacity, and returns errors for any instances that could not be launched.
--
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#ec2-fleet-request-type EC2 Fleet request types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfType :: Lens.Lens' CreateFleet (Core.Maybe Types.FleetType)
cfType = Lens.field @"type'"
{-# DEPRECATED cfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
--
-- /Note:/ Consider using 'validFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfValidFrom :: Lens.Lens' CreateFleet (Core.Maybe Core.UTCTime)
cfValidFrom = Lens.field @"validFrom"
{-# DEPRECATED cfValidFrom "Use generic-lens or generic-optics with 'validFrom' instead." #-}

-- | The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new EC2 Fleet requests are placed or able to fulfill the request. If no value is specified, the request remains until you cancel it.
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfValidUntil :: Lens.Lens' CreateFleet (Core.Maybe Core.UTCTime)
cfValidUntil = Lens.field @"validUntil"
{-# DEPRECATED cfValidUntil "Use generic-lens or generic-optics with 'validUntil' instead." #-}

instance Core.AWSRequest CreateFleet where
  type Rs CreateFleet = CreateFleetResponse
  request x@Core.Request {..} =
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
            ( Core.pure ("Action", "CreateFleet")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "LaunchTemplateConfigs" launchTemplateConfigs)
                Core.<> ( Core.toQueryValue
                            "TargetCapacitySpecification"
                            targetCapacitySpecification
                        )
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> ( Core.toQueryValue "ExcessCapacityTerminationPolicy"
                            Core.<$> excessCapacityTerminationPolicy
                        )
                Core.<> (Core.toQueryValue "OnDemandOptions" Core.<$> onDemandOptions)
                Core.<> ( Core.toQueryValue "ReplaceUnhealthyInstances"
                            Core.<$> replaceUnhealthyInstances
                        )
                Core.<> (Core.toQueryValue "SpotOptions" Core.<$> spotOptions)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
                Core.<> ( Core.toQueryValue "TerminateInstancesWithExpiration"
                            Core.<$> terminateInstancesWithExpiration
                        )
                Core.<> (Core.toQueryValue "Type" Core.<$> type')
                Core.<> (Core.toQueryValue "ValidFrom" Core.<$> validFrom)
                Core.<> (Core.toQueryValue "ValidUntil" Core.<$> validUntil)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFleetResponse'
            Core.<$> (x Core..@? "errorSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "fleetId")
            Core.<*> (x Core..@? "fleetInstanceSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { -- | Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
    errors :: Core.Maybe [Types.CreateFleetError],
    -- | The ID of the EC2 Fleet.
    fleetId :: Core.Maybe Types.FleetId,
    -- | Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
    instances :: Core.Maybe [Types.CreateFleetInstance],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFleetResponse' value with any optional fields omitted.
mkCreateFleetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateFleetResponse
mkCreateFleetResponse responseStatus =
  CreateFleetResponse'
    { errors = Core.Nothing,
      fleetId = Core.Nothing,
      instances = Core.Nothing,
      responseStatus
    }

-- | Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsErrors :: Lens.Lens' CreateFleetResponse (Core.Maybe [Types.CreateFleetError])
cfrrsErrors = Lens.field @"errors"
{-# DEPRECATED cfrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsFleetId :: Lens.Lens' CreateFleetResponse (Core.Maybe Types.FleetId)
cfrrsFleetId = Lens.field @"fleetId"
{-# DEPRECATED cfrrsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsInstances :: Lens.Lens' CreateFleetResponse (Core.Maybe [Types.CreateFleetInstance])
cfrrsInstances = Lens.field @"instances"
{-# DEPRECATED cfrrsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsResponseStatus :: Lens.Lens' CreateFleetResponse Core.Int
cfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
