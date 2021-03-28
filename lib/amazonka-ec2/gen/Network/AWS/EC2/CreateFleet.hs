{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateFleet (..)
    , mkCreateFleet
    -- ** Request lenses
    , cfLaunchTemplateConfigs
    , cfTargetCapacitySpecification
    , cfClientToken
    , cfDryRun
    , cfExcessCapacityTerminationPolicy
    , cfOnDemandOptions
    , cfReplaceUnhealthyInstances
    , cfSpotOptions
    , cfTagSpecifications
    , cfTerminateInstancesWithExpiration
    , cfType
    , cfValidFrom
    , cfValidUntil

    -- * Destructuring the response
    , CreateFleetResponse (..)
    , mkCreateFleetResponse
    -- ** Response lenses
    , cfrrsErrors
    , cfrrsFleetId
    , cfrrsInstances
    , cfrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { launchTemplateConfigs :: [Types.FleetLaunchTemplateConfigRequest]
    -- ^ The configuration for the EC2 Fleet.
  , targetCapacitySpecification :: Types.TargetCapacitySpecificationRequest
    -- ^ The number of units to request.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , excessCapacityTerminationPolicy :: Core.Maybe Types.FleetExcessCapacityTerminationPolicy
    -- ^ Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
  , onDemandOptions :: Core.Maybe Types.OnDemandOptionsRequest
    -- ^ Describes the configuration of On-Demand Instances in an EC2 Fleet.
  , replaceUnhealthyInstances :: Core.Maybe Core.Bool
    -- ^ Indicates whether EC2 Fleet should replace unhealthy instances.
  , spotOptions :: Core.Maybe Types.SpotOptionsRequest
    -- ^ Describes the configuration of Spot Instances in an EC2 Fleet.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The key-value pair for tagging the EC2 Fleet request on creation. The value for @ResourceType@ must be @fleet@ , otherwise the fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> . For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging your resources> .
  , terminateInstancesWithExpiration :: Core.Maybe Core.Bool
    -- ^ Indicates whether running instances should be terminated when the EC2 Fleet expires.
  , type' :: Core.Maybe Types.FleetType
    -- ^ The type of request. The default value is @maintain@ .
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
  , validFrom :: Core.Maybe Core.UTCTime
    -- ^ The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
  , validUntil :: Core.Maybe Core.UTCTime
    -- ^ The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new EC2 Fleet requests are placed or able to fulfill the request. If no value is specified, the request remains until you cancel it.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateFleet' value with any optional fields omitted.
mkCreateFleet
    :: Types.TargetCapacitySpecificationRequest -- ^ 'targetCapacitySpecification'
    -> CreateFleet
mkCreateFleet targetCapacitySpecification
  = CreateFleet'{launchTemplateConfigs = Core.mempty,
                 targetCapacitySpecification, clientToken = Core.Nothing,
                 dryRun = Core.Nothing,
                 excessCapacityTerminationPolicy = Core.Nothing,
                 onDemandOptions = Core.Nothing,
                 replaceUnhealthyInstances = Core.Nothing,
                 spotOptions = Core.Nothing, tagSpecifications = Core.Nothing,
                 terminateInstancesWithExpiration = Core.Nothing,
                 type' = Core.Nothing, validFrom = Core.Nothing,
                 validUntil = Core.Nothing}

-- | The configuration for the EC2 Fleet.
--
-- /Note:/ Consider using 'launchTemplateConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLaunchTemplateConfigs :: Lens.Lens' CreateFleet [Types.FleetLaunchTemplateConfigRequest]
cfLaunchTemplateConfigs = Lens.field @"launchTemplateConfigs"
{-# INLINEABLE cfLaunchTemplateConfigs #-}
{-# DEPRECATED launchTemplateConfigs "Use generic-lens or generic-optics with 'launchTemplateConfigs' instead"  #-}

-- | The number of units to request.
--
-- /Note:/ Consider using 'targetCapacitySpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTargetCapacitySpecification :: Lens.Lens' CreateFleet Types.TargetCapacitySpecificationRequest
cfTargetCapacitySpecification = Lens.field @"targetCapacitySpecification"
{-# INLINEABLE cfTargetCapacitySpecification #-}
{-# DEPRECATED targetCapacitySpecification "Use generic-lens or generic-optics with 'targetCapacitySpecification' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfClientToken :: Lens.Lens' CreateFleet (Core.Maybe Core.Text)
cfClientToken = Lens.field @"clientToken"
{-# INLINEABLE cfClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDryRun :: Lens.Lens' CreateFleet (Core.Maybe Core.Bool)
cfDryRun = Lens.field @"dryRun"
{-# INLINEABLE cfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
--
-- /Note:/ Consider using 'excessCapacityTerminationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfExcessCapacityTerminationPolicy :: Lens.Lens' CreateFleet (Core.Maybe Types.FleetExcessCapacityTerminationPolicy)
cfExcessCapacityTerminationPolicy = Lens.field @"excessCapacityTerminationPolicy"
{-# INLINEABLE cfExcessCapacityTerminationPolicy #-}
{-# DEPRECATED excessCapacityTerminationPolicy "Use generic-lens or generic-optics with 'excessCapacityTerminationPolicy' instead"  #-}

-- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
--
-- /Note:/ Consider using 'onDemandOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfOnDemandOptions :: Lens.Lens' CreateFleet (Core.Maybe Types.OnDemandOptionsRequest)
cfOnDemandOptions = Lens.field @"onDemandOptions"
{-# INLINEABLE cfOnDemandOptions #-}
{-# DEPRECATED onDemandOptions "Use generic-lens or generic-optics with 'onDemandOptions' instead"  #-}

-- | Indicates whether EC2 Fleet should replace unhealthy instances.
--
-- /Note:/ Consider using 'replaceUnhealthyInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfReplaceUnhealthyInstances :: Lens.Lens' CreateFleet (Core.Maybe Core.Bool)
cfReplaceUnhealthyInstances = Lens.field @"replaceUnhealthyInstances"
{-# INLINEABLE cfReplaceUnhealthyInstances #-}
{-# DEPRECATED replaceUnhealthyInstances "Use generic-lens or generic-optics with 'replaceUnhealthyInstances' instead"  #-}

-- | Describes the configuration of Spot Instances in an EC2 Fleet.
--
-- /Note:/ Consider using 'spotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSpotOptions :: Lens.Lens' CreateFleet (Core.Maybe Types.SpotOptionsRequest)
cfSpotOptions = Lens.field @"spotOptions"
{-# INLINEABLE cfSpotOptions #-}
{-# DEPRECATED spotOptions "Use generic-lens or generic-optics with 'spotOptions' instead"  #-}

-- | The key-value pair for tagging the EC2 Fleet request on creation. The value for @ResourceType@ must be @fleet@ , otherwise the fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> . For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging your resources> .
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTagSpecifications :: Lens.Lens' CreateFleet (Core.Maybe [Types.TagSpecification])
cfTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cfTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

-- | Indicates whether running instances should be terminated when the EC2 Fleet expires.
--
-- /Note:/ Consider using 'terminateInstancesWithExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTerminateInstancesWithExpiration :: Lens.Lens' CreateFleet (Core.Maybe Core.Bool)
cfTerminateInstancesWithExpiration = Lens.field @"terminateInstancesWithExpiration"
{-# INLINEABLE cfTerminateInstancesWithExpiration #-}
{-# DEPRECATED terminateInstancesWithExpiration "Use generic-lens or generic-optics with 'terminateInstancesWithExpiration' instead"  #-}

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
{-# INLINEABLE cfType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
--
-- /Note:/ Consider using 'validFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfValidFrom :: Lens.Lens' CreateFleet (Core.Maybe Core.UTCTime)
cfValidFrom = Lens.field @"validFrom"
{-# INLINEABLE cfValidFrom #-}
{-# DEPRECATED validFrom "Use generic-lens or generic-optics with 'validFrom' instead"  #-}

-- | The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new EC2 Fleet requests are placed or able to fulfill the request. If no value is specified, the request remains until you cancel it.
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfValidUntil :: Lens.Lens' CreateFleet (Core.Maybe Core.UTCTime)
cfValidUntil = Lens.field @"validUntil"
{-# INLINEABLE cfValidUntil #-}
{-# DEPRECATED validUntil "Use generic-lens or generic-optics with 'validUntil' instead"  #-}

instance Core.ToQuery CreateFleet where
        toQuery CreateFleet{..}
          = Core.toQueryPair "Action" ("CreateFleet" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryList "LaunchTemplateConfigs" launchTemplateConfigs
              Core.<>
              Core.toQueryPair "TargetCapacitySpecification"
                targetCapacitySpecification
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ExcessCapacityTerminationPolicy")
                excessCapacityTerminationPolicy
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OnDemandOptions")
                onDemandOptions
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ReplaceUnhealthyInstances")
                replaceUnhealthyInstances
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SpotOptions") spotOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "TerminateInstancesWithExpiration")
                terminateInstancesWithExpiration
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Type") type'
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ValidFrom") validFrom
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ValidUntil") validUntil

instance Core.ToHeaders CreateFleet where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateFleet where
        type Rs CreateFleet = CreateFleetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateFleetResponse' Core.<$>
                   (x Core..@? "errorSet" Core..<@> Core.parseXMLList "item") Core.<*>
                     x Core..@? "fleetId"
                     Core.<*>
                     x Core..@? "fleetInstanceSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { errors :: Core.Maybe [Types.CreateFleetError]
    -- ^ Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
  , fleetId :: Core.Maybe Types.FleetId
    -- ^ The ID of the EC2 Fleet.
  , instances :: Core.Maybe [Types.CreateFleetInstance]
    -- ^ Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFleetResponse' value with any optional fields omitted.
mkCreateFleetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateFleetResponse
mkCreateFleetResponse responseStatus
  = CreateFleetResponse'{errors = Core.Nothing,
                         fleetId = Core.Nothing, instances = Core.Nothing, responseStatus}

-- | Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsErrors :: Lens.Lens' CreateFleetResponse (Core.Maybe [Types.CreateFleetError])
cfrrsErrors = Lens.field @"errors"
{-# INLINEABLE cfrrsErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsFleetId :: Lens.Lens' CreateFleetResponse (Core.Maybe Types.FleetId)
cfrrsFleetId = Lens.field @"fleetId"
{-# INLINEABLE cfrrsFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsInstances :: Lens.Lens' CreateFleetResponse (Core.Maybe [Types.CreateFleetInstance])
cfrrsInstances = Lens.field @"instances"
{-# INLINEABLE cfrrsInstances #-}
{-# DEPRECATED instances "Use generic-lens or generic-optics with 'instances' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsResponseStatus :: Lens.Lens' CreateFleetResponse Core.Int
cfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
