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
    cfClientToken,
    cfTargetCapacitySpecification,
    cfSpotOptions,
    cfExcessCapacityTerminationPolicy,
    cfOnDemandOptions,
    cfLaunchTemplateConfigs,
    cfTagSpecifications,
    cfValidUntil,
    cfTerminateInstancesWithExpiration,
    cfType,
    cfValidFrom,
    cfReplaceUnhealthyInstances,
    cfDryRun,

    -- * Destructuring the response
    CreateFleetResponse (..),
    mkCreateFleetResponse,

    -- ** Response lenses
    cfrsInstances,
    cfrsFleetId,
    cfrsErrors,
    cfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    clientToken :: Lude.Maybe Lude.Text,
    -- | The number of units to request.
    targetCapacitySpecification :: TargetCapacitySpecificationRequest,
    -- | Describes the configuration of Spot Instances in an EC2 Fleet.
    spotOptions :: Lude.Maybe SpotOptionsRequest,
    -- | Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
    excessCapacityTerminationPolicy :: Lude.Maybe FleetExcessCapacityTerminationPolicy,
    -- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
    onDemandOptions :: Lude.Maybe OnDemandOptionsRequest,
    -- | The configuration for the EC2 Fleet.
    launchTemplateConfigs :: [FleetLaunchTemplateConfigRequest],
    -- | The key-value pair for tagging the EC2 Fleet request on creation. The value for @ResourceType@ must be @fleet@ , otherwise the fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> . For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging your resources> .
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new EC2 Fleet requests are placed or able to fulfill the request. If no value is specified, the request remains until you cancel it.
    validUntil :: Lude.Maybe Lude.DateTime,
    -- | Indicates whether running instances should be terminated when the EC2 Fleet expires.
    terminateInstancesWithExpiration :: Lude.Maybe Lude.Bool,
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
    type' :: Lude.Maybe FleetType,
    -- | The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
    validFrom :: Lude.Maybe Lude.DateTime,
    -- | Indicates whether EC2 Fleet should replace unhealthy instances.
    replaceUnhealthyInstances :: Lude.Maybe Lude.Bool,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFleet' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
-- * 'targetCapacitySpecification' - The number of units to request.
-- * 'spotOptions' - Describes the configuration of Spot Instances in an EC2 Fleet.
-- * 'excessCapacityTerminationPolicy' - Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
-- * 'onDemandOptions' - Describes the configuration of On-Demand Instances in an EC2 Fleet.
-- * 'launchTemplateConfigs' - The configuration for the EC2 Fleet.
-- * 'tagSpecifications' - The key-value pair for tagging the EC2 Fleet request on creation. The value for @ResourceType@ must be @fleet@ , otherwise the fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> . For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging your resources> .
-- * 'validUntil' - The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new EC2 Fleet requests are placed or able to fulfill the request. If no value is specified, the request remains until you cancel it.
-- * 'terminateInstancesWithExpiration' - Indicates whether running instances should be terminated when the EC2 Fleet expires.
-- * 'type'' - The type of request. The default value is @maintain@ .
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
-- * 'validFrom' - The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
-- * 'replaceUnhealthyInstances' - Indicates whether EC2 Fleet should replace unhealthy instances.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateFleet ::
  -- | 'targetCapacitySpecification'
  TargetCapacitySpecificationRequest ->
  CreateFleet
mkCreateFleet pTargetCapacitySpecification_ =
  CreateFleet'
    { clientToken = Lude.Nothing,
      targetCapacitySpecification = pTargetCapacitySpecification_,
      spotOptions = Lude.Nothing,
      excessCapacityTerminationPolicy = Lude.Nothing,
      onDemandOptions = Lude.Nothing,
      launchTemplateConfigs = Lude.mempty,
      tagSpecifications = Lude.Nothing,
      validUntil = Lude.Nothing,
      terminateInstancesWithExpiration = Lude.Nothing,
      type' = Lude.Nothing,
      validFrom = Lude.Nothing,
      replaceUnhealthyInstances = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfClientToken :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfClientToken = Lens.lens (clientToken :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateFleet)
{-# DEPRECATED cfClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The number of units to request.
--
-- /Note:/ Consider using 'targetCapacitySpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTargetCapacitySpecification :: Lens.Lens' CreateFleet TargetCapacitySpecificationRequest
cfTargetCapacitySpecification = Lens.lens (targetCapacitySpecification :: CreateFleet -> TargetCapacitySpecificationRequest) (\s a -> s {targetCapacitySpecification = a} :: CreateFleet)
{-# DEPRECATED cfTargetCapacitySpecification "Use generic-lens or generic-optics with 'targetCapacitySpecification' instead." #-}

-- | Describes the configuration of Spot Instances in an EC2 Fleet.
--
-- /Note:/ Consider using 'spotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSpotOptions :: Lens.Lens' CreateFleet (Lude.Maybe SpotOptionsRequest)
cfSpotOptions = Lens.lens (spotOptions :: CreateFleet -> Lude.Maybe SpotOptionsRequest) (\s a -> s {spotOptions = a} :: CreateFleet)
{-# DEPRECATED cfSpotOptions "Use generic-lens or generic-optics with 'spotOptions' instead." #-}

-- | Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
--
-- /Note:/ Consider using 'excessCapacityTerminationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfExcessCapacityTerminationPolicy :: Lens.Lens' CreateFleet (Lude.Maybe FleetExcessCapacityTerminationPolicy)
cfExcessCapacityTerminationPolicy = Lens.lens (excessCapacityTerminationPolicy :: CreateFleet -> Lude.Maybe FleetExcessCapacityTerminationPolicy) (\s a -> s {excessCapacityTerminationPolicy = a} :: CreateFleet)
{-# DEPRECATED cfExcessCapacityTerminationPolicy "Use generic-lens or generic-optics with 'excessCapacityTerminationPolicy' instead." #-}

-- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
--
-- /Note:/ Consider using 'onDemandOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfOnDemandOptions :: Lens.Lens' CreateFleet (Lude.Maybe OnDemandOptionsRequest)
cfOnDemandOptions = Lens.lens (onDemandOptions :: CreateFleet -> Lude.Maybe OnDemandOptionsRequest) (\s a -> s {onDemandOptions = a} :: CreateFleet)
{-# DEPRECATED cfOnDemandOptions "Use generic-lens or generic-optics with 'onDemandOptions' instead." #-}

-- | The configuration for the EC2 Fleet.
--
-- /Note:/ Consider using 'launchTemplateConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLaunchTemplateConfigs :: Lens.Lens' CreateFleet [FleetLaunchTemplateConfigRequest]
cfLaunchTemplateConfigs = Lens.lens (launchTemplateConfigs :: CreateFleet -> [FleetLaunchTemplateConfigRequest]) (\s a -> s {launchTemplateConfigs = a} :: CreateFleet)
{-# DEPRECATED cfLaunchTemplateConfigs "Use generic-lens or generic-optics with 'launchTemplateConfigs' instead." #-}

-- | The key-value pair for tagging the EC2 Fleet request on creation. The value for @ResourceType@ must be @fleet@ , otherwise the fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> . For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging your resources> .
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTagSpecifications :: Lens.Lens' CreateFleet (Lude.Maybe [TagSpecification])
cfTagSpecifications = Lens.lens (tagSpecifications :: CreateFleet -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateFleet)
{-# DEPRECATED cfTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new EC2 Fleet requests are placed or able to fulfill the request. If no value is specified, the request remains until you cancel it.
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfValidUntil :: Lens.Lens' CreateFleet (Lude.Maybe Lude.DateTime)
cfValidUntil = Lens.lens (validUntil :: CreateFleet -> Lude.Maybe Lude.DateTime) (\s a -> s {validUntil = a} :: CreateFleet)
{-# DEPRECATED cfValidUntil "Use generic-lens or generic-optics with 'validUntil' instead." #-}

-- | Indicates whether running instances should be terminated when the EC2 Fleet expires.
--
-- /Note:/ Consider using 'terminateInstancesWithExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTerminateInstancesWithExpiration :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Bool)
cfTerminateInstancesWithExpiration = Lens.lens (terminateInstancesWithExpiration :: CreateFleet -> Lude.Maybe Lude.Bool) (\s a -> s {terminateInstancesWithExpiration = a} :: CreateFleet)
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
cfType :: Lens.Lens' CreateFleet (Lude.Maybe FleetType)
cfType = Lens.lens (type' :: CreateFleet -> Lude.Maybe FleetType) (\s a -> s {type' = a} :: CreateFleet)
{-# DEPRECATED cfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
--
-- /Note:/ Consider using 'validFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfValidFrom :: Lens.Lens' CreateFleet (Lude.Maybe Lude.DateTime)
cfValidFrom = Lens.lens (validFrom :: CreateFleet -> Lude.Maybe Lude.DateTime) (\s a -> s {validFrom = a} :: CreateFleet)
{-# DEPRECATED cfValidFrom "Use generic-lens or generic-optics with 'validFrom' instead." #-}

-- | Indicates whether EC2 Fleet should replace unhealthy instances.
--
-- /Note:/ Consider using 'replaceUnhealthyInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfReplaceUnhealthyInstances :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Bool)
cfReplaceUnhealthyInstances = Lens.lens (replaceUnhealthyInstances :: CreateFleet -> Lude.Maybe Lude.Bool) (\s a -> s {replaceUnhealthyInstances = a} :: CreateFleet)
{-# DEPRECATED cfReplaceUnhealthyInstances "Use generic-lens or generic-optics with 'replaceUnhealthyInstances' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDryRun :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Bool)
cfDryRun = Lens.lens (dryRun :: CreateFleet -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateFleet)
{-# DEPRECATED cfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateFleet where
  type Rs CreateFleet = CreateFleetResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateFleetResponse'
            Lude.<$> ( x Lude..@? "fleetInstanceSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "fleetId")
            Lude.<*> ( x Lude..@? "errorSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateFleet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateFleet where
  toQuery CreateFleet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateFleet" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "TargetCapacitySpecification" Lude.=: targetCapacitySpecification,
        "SpotOptions" Lude.=: spotOptions,
        "ExcessCapacityTerminationPolicy"
          Lude.=: excessCapacityTerminationPolicy,
        "OnDemandOptions" Lude.=: onDemandOptions,
        Lude.toQueryList "LaunchTemplateConfigs" launchTemplateConfigs,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "ValidUntil" Lude.=: validUntil,
        "TerminateInstancesWithExpiration"
          Lude.=: terminateInstancesWithExpiration,
        "Type" Lude.=: type',
        "ValidFrom" Lude.=: validFrom,
        "ReplaceUnhealthyInstances" Lude.=: replaceUnhealthyInstances,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { -- | Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
    instances :: Lude.Maybe [CreateFleetInstance],
    -- | The ID of the EC2 Fleet.
    fleetId :: Lude.Maybe Lude.Text,
    -- | Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
    errors :: Lude.Maybe [CreateFleetError],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFleetResponse' with the minimum fields required to make a request.
--
-- * 'instances' - Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
-- * 'fleetId' - The ID of the EC2 Fleet.
-- * 'errors' - Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
-- * 'responseStatus' - The response status code.
mkCreateFleetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateFleetResponse
mkCreateFleetResponse pResponseStatus_ =
  CreateFleetResponse'
    { instances = Lude.Nothing,
      fleetId = Lude.Nothing,
      errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsInstances :: Lens.Lens' CreateFleetResponse (Lude.Maybe [CreateFleetInstance])
cfrsInstances = Lens.lens (instances :: CreateFleetResponse -> Lude.Maybe [CreateFleetInstance]) (\s a -> s {instances = a} :: CreateFleetResponse)
{-# DEPRECATED cfrsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsFleetId :: Lens.Lens' CreateFleetResponse (Lude.Maybe Lude.Text)
cfrsFleetId = Lens.lens (fleetId :: CreateFleetResponse -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: CreateFleetResponse)
{-# DEPRECATED cfrsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsErrors :: Lens.Lens' CreateFleetResponse (Lude.Maybe [CreateFleetError])
cfrsErrors = Lens.lens (errors :: CreateFleetResponse -> Lude.Maybe [CreateFleetError]) (\s a -> s {errors = a} :: CreateFleetResponse)
{-# DEPRECATED cfrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsResponseStatus :: Lens.Lens' CreateFleetResponse Lude.Int
cfrsResponseStatus = Lens.lens (responseStatus :: CreateFleetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateFleetResponse)
{-# DEPRECATED cfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
