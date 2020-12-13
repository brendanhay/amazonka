{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotInstanceRequest
  ( SpotInstanceRequest (..),

    -- * Smart constructor
    mkSpotInstanceRequest,

    -- * Lenses
    sirInstanceId,
    sirStatus,
    sirState,
    sirActualBlockHourlyPrice,
    sirBlockDurationMinutes,
    sirInstanceInterruptionBehavior,
    sirProductDescription,
    sirSpotPrice,
    sirLaunchSpecification,
    sirAvailabilityZoneGroup,
    sirLaunchedAvailabilityZone,
    sirValidUntil,
    sirLaunchGroup,
    sirFault,
    sirSpotInstanceRequestId,
    sirType,
    sirValidFrom,
    sirCreateTime,
    sirTags,
  )
where

import Network.AWS.EC2.Types.InstanceInterruptionBehavior
import Network.AWS.EC2.Types.LaunchSpecification
import Network.AWS.EC2.Types.RIProductDescription
import Network.AWS.EC2.Types.SpotInstanceState
import Network.AWS.EC2.Types.SpotInstanceStateFault
import Network.AWS.EC2.Types.SpotInstanceStatus
import Network.AWS.EC2.Types.SpotInstanceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Spot Instance request.
--
-- /See:/ 'mkSpotInstanceRequest' smart constructor.
data SpotInstanceRequest = SpotInstanceRequest'
  { -- | The instance ID, if an instance has been launched to fulfill the Spot Instance request.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The status code and status message describing the Spot Instance request.
    status :: Lude.Maybe SpotInstanceStatus,
    -- | The state of the Spot Instance request. Spot status information helps track your Spot Instance requests. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot status> in the /Amazon EC2 User Guide for Linux Instances/ .
    state :: Lude.Maybe SpotInstanceState,
    -- | If you specified a duration and your Spot Instance request was fulfilled, this is the fixed hourly price in effect for the Spot Instance while it runs.
    actualBlockHourlyPrice :: Lude.Maybe Lude.Text,
    -- | The duration for the Spot Instance, in minutes.
    blockDurationMinutes :: Lude.Maybe Lude.Int,
    -- | The behavior when a Spot Instance is interrupted.
    instanceInterruptionBehavior :: Lude.Maybe InstanceInterruptionBehavior,
    -- | The product description associated with the Spot Instance.
    productDescription :: Lude.Maybe RIProductDescription,
    -- | The maximum price per hour that you are willing to pay for a Spot Instance.
    spotPrice :: Lude.Maybe Lude.Text,
    -- | Additional information for launching instances.
    launchSpecification :: Lude.Maybe LaunchSpecification,
    -- | The Availability Zone group. If you specify the same Availability Zone group for all Spot Instance requests, all Spot Instances are launched in the same Availability Zone.
    availabilityZoneGroup :: Lude.Maybe Lude.Text,
    -- | The Availability Zone in which the request is launched.
    launchedAvailabilityZone :: Lude.Maybe Lude.Text,
    -- | The end date of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
    --
    --
    --     * For a persistent request, the request remains active until the @validUntil@ date and time is reached. Otherwise, the request remains active until you cancel it.
    --
    --
    --     * For a one-time request, the request remains active until all instances launch, the request is canceled, or the @validUntil@ date and time is reached. By default, the request is valid for 7 days from the date the request was created.
    validUntil :: Lude.Maybe Lude.DateTime,
    -- | The instance launch group. Launch groups are Spot Instances that launch together and terminate together.
    launchGroup :: Lude.Maybe Lude.Text,
    -- | The fault codes for the Spot Instance request, if any.
    fault :: Lude.Maybe SpotInstanceStateFault,
    -- | The ID of the Spot Instance request.
    spotInstanceRequestId :: Lude.Maybe Lude.Text,
    -- | The Spot Instance request type.
    type' :: Lude.Maybe SpotInstanceType,
    -- | The start date of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The request becomes active at this date and time.
    validFrom :: Lude.Maybe Lude.DateTime,
    -- | The date and time when the Spot Instance request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
    createTime :: Lude.Maybe Lude.DateTime,
    -- | Any tags assigned to the resource.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotInstanceRequest' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID, if an instance has been launched to fulfill the Spot Instance request.
-- * 'status' - The status code and status message describing the Spot Instance request.
-- * 'state' - The state of the Spot Instance request. Spot status information helps track your Spot Instance requests. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot status> in the /Amazon EC2 User Guide for Linux Instances/ .
-- * 'actualBlockHourlyPrice' - If you specified a duration and your Spot Instance request was fulfilled, this is the fixed hourly price in effect for the Spot Instance while it runs.
-- * 'blockDurationMinutes' - The duration for the Spot Instance, in minutes.
-- * 'instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted.
-- * 'productDescription' - The product description associated with the Spot Instance.
-- * 'spotPrice' - The maximum price per hour that you are willing to pay for a Spot Instance.
-- * 'launchSpecification' - Additional information for launching instances.
-- * 'availabilityZoneGroup' - The Availability Zone group. If you specify the same Availability Zone group for all Spot Instance requests, all Spot Instances are launched in the same Availability Zone.
-- * 'launchedAvailabilityZone' - The Availability Zone in which the request is launched.
-- * 'validUntil' - The end date of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
--
--     * For a persistent request, the request remains active until the @validUntil@ date and time is reached. Otherwise, the request remains active until you cancel it.
--
--
--     * For a one-time request, the request remains active until all instances launch, the request is canceled, or the @validUntil@ date and time is reached. By default, the request is valid for 7 days from the date the request was created.
--
--
-- * 'launchGroup' - The instance launch group. Launch groups are Spot Instances that launch together and terminate together.
-- * 'fault' - The fault codes for the Spot Instance request, if any.
-- * 'spotInstanceRequestId' - The ID of the Spot Instance request.
-- * 'type'' - The Spot Instance request type.
-- * 'validFrom' - The start date of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The request becomes active at this date and time.
-- * 'createTime' - The date and time when the Spot Instance request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
-- * 'tags' - Any tags assigned to the resource.
mkSpotInstanceRequest ::
  SpotInstanceRequest
mkSpotInstanceRequest =
  SpotInstanceRequest'
    { instanceId = Lude.Nothing,
      status = Lude.Nothing,
      state = Lude.Nothing,
      actualBlockHourlyPrice = Lude.Nothing,
      blockDurationMinutes = Lude.Nothing,
      instanceInterruptionBehavior = Lude.Nothing,
      productDescription = Lude.Nothing,
      spotPrice = Lude.Nothing,
      launchSpecification = Lude.Nothing,
      availabilityZoneGroup = Lude.Nothing,
      launchedAvailabilityZone = Lude.Nothing,
      validUntil = Lude.Nothing,
      launchGroup = Lude.Nothing,
      fault = Lude.Nothing,
      spotInstanceRequestId = Lude.Nothing,
      type' = Lude.Nothing,
      validFrom = Lude.Nothing,
      createTime = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The instance ID, if an instance has been launched to fulfill the Spot Instance request.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirInstanceId :: Lens.Lens' SpotInstanceRequest (Lude.Maybe Lude.Text)
sirInstanceId = Lens.lens (instanceId :: SpotInstanceRequest -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: SpotInstanceRequest)
{-# DEPRECATED sirInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The status code and status message describing the Spot Instance request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirStatus :: Lens.Lens' SpotInstanceRequest (Lude.Maybe SpotInstanceStatus)
sirStatus = Lens.lens (status :: SpotInstanceRequest -> Lude.Maybe SpotInstanceStatus) (\s a -> s {status = a} :: SpotInstanceRequest)
{-# DEPRECATED sirStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The state of the Spot Instance request. Spot status information helps track your Spot Instance requests. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot status> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirState :: Lens.Lens' SpotInstanceRequest (Lude.Maybe SpotInstanceState)
sirState = Lens.lens (state :: SpotInstanceRequest -> Lude.Maybe SpotInstanceState) (\s a -> s {state = a} :: SpotInstanceRequest)
{-# DEPRECATED sirState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | If you specified a duration and your Spot Instance request was fulfilled, this is the fixed hourly price in effect for the Spot Instance while it runs.
--
-- /Note:/ Consider using 'actualBlockHourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirActualBlockHourlyPrice :: Lens.Lens' SpotInstanceRequest (Lude.Maybe Lude.Text)
sirActualBlockHourlyPrice = Lens.lens (actualBlockHourlyPrice :: SpotInstanceRequest -> Lude.Maybe Lude.Text) (\s a -> s {actualBlockHourlyPrice = a} :: SpotInstanceRequest)
{-# DEPRECATED sirActualBlockHourlyPrice "Use generic-lens or generic-optics with 'actualBlockHourlyPrice' instead." #-}

-- | The duration for the Spot Instance, in minutes.
--
-- /Note:/ Consider using 'blockDurationMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirBlockDurationMinutes :: Lens.Lens' SpotInstanceRequest (Lude.Maybe Lude.Int)
sirBlockDurationMinutes = Lens.lens (blockDurationMinutes :: SpotInstanceRequest -> Lude.Maybe Lude.Int) (\s a -> s {blockDurationMinutes = a} :: SpotInstanceRequest)
{-# DEPRECATED sirBlockDurationMinutes "Use generic-lens or generic-optics with 'blockDurationMinutes' instead." #-}

-- | The behavior when a Spot Instance is interrupted.
--
-- /Note:/ Consider using 'instanceInterruptionBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirInstanceInterruptionBehavior :: Lens.Lens' SpotInstanceRequest (Lude.Maybe InstanceInterruptionBehavior)
sirInstanceInterruptionBehavior = Lens.lens (instanceInterruptionBehavior :: SpotInstanceRequest -> Lude.Maybe InstanceInterruptionBehavior) (\s a -> s {instanceInterruptionBehavior = a} :: SpotInstanceRequest)
{-# DEPRECATED sirInstanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead." #-}

-- | The product description associated with the Spot Instance.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirProductDescription :: Lens.Lens' SpotInstanceRequest (Lude.Maybe RIProductDescription)
sirProductDescription = Lens.lens (productDescription :: SpotInstanceRequest -> Lude.Maybe RIProductDescription) (\s a -> s {productDescription = a} :: SpotInstanceRequest)
{-# DEPRECATED sirProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The maximum price per hour that you are willing to pay for a Spot Instance.
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirSpotPrice :: Lens.Lens' SpotInstanceRequest (Lude.Maybe Lude.Text)
sirSpotPrice = Lens.lens (spotPrice :: SpotInstanceRequest -> Lude.Maybe Lude.Text) (\s a -> s {spotPrice = a} :: SpotInstanceRequest)
{-# DEPRECATED sirSpotPrice "Use generic-lens or generic-optics with 'spotPrice' instead." #-}

-- | Additional information for launching instances.
--
-- /Note:/ Consider using 'launchSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirLaunchSpecification :: Lens.Lens' SpotInstanceRequest (Lude.Maybe LaunchSpecification)
sirLaunchSpecification = Lens.lens (launchSpecification :: SpotInstanceRequest -> Lude.Maybe LaunchSpecification) (\s a -> s {launchSpecification = a} :: SpotInstanceRequest)
{-# DEPRECATED sirLaunchSpecification "Use generic-lens or generic-optics with 'launchSpecification' instead." #-}

-- | The Availability Zone group. If you specify the same Availability Zone group for all Spot Instance requests, all Spot Instances are launched in the same Availability Zone.
--
-- /Note:/ Consider using 'availabilityZoneGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirAvailabilityZoneGroup :: Lens.Lens' SpotInstanceRequest (Lude.Maybe Lude.Text)
sirAvailabilityZoneGroup = Lens.lens (availabilityZoneGroup :: SpotInstanceRequest -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZoneGroup = a} :: SpotInstanceRequest)
{-# DEPRECATED sirAvailabilityZoneGroup "Use generic-lens or generic-optics with 'availabilityZoneGroup' instead." #-}

-- | The Availability Zone in which the request is launched.
--
-- /Note:/ Consider using 'launchedAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirLaunchedAvailabilityZone :: Lens.Lens' SpotInstanceRequest (Lude.Maybe Lude.Text)
sirLaunchedAvailabilityZone = Lens.lens (launchedAvailabilityZone :: SpotInstanceRequest -> Lude.Maybe Lude.Text) (\s a -> s {launchedAvailabilityZone = a} :: SpotInstanceRequest)
{-# DEPRECATED sirLaunchedAvailabilityZone "Use generic-lens or generic-optics with 'launchedAvailabilityZone' instead." #-}

-- | The end date of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
--
--     * For a persistent request, the request remains active until the @validUntil@ date and time is reached. Otherwise, the request remains active until you cancel it.
--
--
--     * For a one-time request, the request remains active until all instances launch, the request is canceled, or the @validUntil@ date and time is reached. By default, the request is valid for 7 days from the date the request was created.
--
--
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirValidUntil :: Lens.Lens' SpotInstanceRequest (Lude.Maybe Lude.DateTime)
sirValidUntil = Lens.lens (validUntil :: SpotInstanceRequest -> Lude.Maybe Lude.DateTime) (\s a -> s {validUntil = a} :: SpotInstanceRequest)
{-# DEPRECATED sirValidUntil "Use generic-lens or generic-optics with 'validUntil' instead." #-}

-- | The instance launch group. Launch groups are Spot Instances that launch together and terminate together.
--
-- /Note:/ Consider using 'launchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirLaunchGroup :: Lens.Lens' SpotInstanceRequest (Lude.Maybe Lude.Text)
sirLaunchGroup = Lens.lens (launchGroup :: SpotInstanceRequest -> Lude.Maybe Lude.Text) (\s a -> s {launchGroup = a} :: SpotInstanceRequest)
{-# DEPRECATED sirLaunchGroup "Use generic-lens or generic-optics with 'launchGroup' instead." #-}

-- | The fault codes for the Spot Instance request, if any.
--
-- /Note:/ Consider using 'fault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirFault :: Lens.Lens' SpotInstanceRequest (Lude.Maybe SpotInstanceStateFault)
sirFault = Lens.lens (fault :: SpotInstanceRequest -> Lude.Maybe SpotInstanceStateFault) (\s a -> s {fault = a} :: SpotInstanceRequest)
{-# DEPRECATED sirFault "Use generic-lens or generic-optics with 'fault' instead." #-}

-- | The ID of the Spot Instance request.
--
-- /Note:/ Consider using 'spotInstanceRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirSpotInstanceRequestId :: Lens.Lens' SpotInstanceRequest (Lude.Maybe Lude.Text)
sirSpotInstanceRequestId = Lens.lens (spotInstanceRequestId :: SpotInstanceRequest -> Lude.Maybe Lude.Text) (\s a -> s {spotInstanceRequestId = a} :: SpotInstanceRequest)
{-# DEPRECATED sirSpotInstanceRequestId "Use generic-lens or generic-optics with 'spotInstanceRequestId' instead." #-}

-- | The Spot Instance request type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirType :: Lens.Lens' SpotInstanceRequest (Lude.Maybe SpotInstanceType)
sirType = Lens.lens (type' :: SpotInstanceRequest -> Lude.Maybe SpotInstanceType) (\s a -> s {type' = a} :: SpotInstanceRequest)
{-# DEPRECATED sirType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The start date of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The request becomes active at this date and time.
--
-- /Note:/ Consider using 'validFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirValidFrom :: Lens.Lens' SpotInstanceRequest (Lude.Maybe Lude.DateTime)
sirValidFrom = Lens.lens (validFrom :: SpotInstanceRequest -> Lude.Maybe Lude.DateTime) (\s a -> s {validFrom = a} :: SpotInstanceRequest)
{-# DEPRECATED sirValidFrom "Use generic-lens or generic-optics with 'validFrom' instead." #-}

-- | The date and time when the Spot Instance request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirCreateTime :: Lens.Lens' SpotInstanceRequest (Lude.Maybe Lude.DateTime)
sirCreateTime = Lens.lens (createTime :: SpotInstanceRequest -> Lude.Maybe Lude.DateTime) (\s a -> s {createTime = a} :: SpotInstanceRequest)
{-# DEPRECATED sirCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | Any tags assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirTags :: Lens.Lens' SpotInstanceRequest (Lude.Maybe [Tag])
sirTags = Lens.lens (tags :: SpotInstanceRequest -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: SpotInstanceRequest)
{-# DEPRECATED sirTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML SpotInstanceRequest where
  parseXML x =
    SpotInstanceRequest'
      Lude.<$> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "actualBlockHourlyPrice")
      Lude.<*> (x Lude..@? "blockDurationMinutes")
      Lude.<*> (x Lude..@? "instanceInterruptionBehavior")
      Lude.<*> (x Lude..@? "productDescription")
      Lude.<*> (x Lude..@? "spotPrice")
      Lude.<*> (x Lude..@? "launchSpecification")
      Lude.<*> (x Lude..@? "availabilityZoneGroup")
      Lude.<*> (x Lude..@? "launchedAvailabilityZone")
      Lude.<*> (x Lude..@? "validUntil")
      Lude.<*> (x Lude..@? "launchGroup")
      Lude.<*> (x Lude..@? "fault")
      Lude.<*> (x Lude..@? "spotInstanceRequestId")
      Lude.<*> (x Lude..@? "type")
      Lude.<*> (x Lude..@? "validFrom")
      Lude.<*> (x Lude..@? "createTime")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
