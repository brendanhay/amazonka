{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotInstanceRequest
  ( SpotInstanceRequest (..)
  -- * Smart constructor
  , mkSpotInstanceRequest
  -- * Lenses
  , sirActualBlockHourlyPrice
  , sirAvailabilityZoneGroup
  , sirBlockDurationMinutes
  , sirCreateTime
  , sirFault
  , sirInstanceId
  , sirInstanceInterruptionBehavior
  , sirLaunchGroup
  , sirLaunchSpecification
  , sirLaunchedAvailabilityZone
  , sirProductDescription
  , sirSpotInstanceRequestId
  , sirSpotPrice
  , sirState
  , sirStatus
  , sirTags
  , sirType
  , sirValidFrom
  , sirValidUntil
  ) where

import qualified Network.AWS.EC2.Types.InstanceId as Types
import qualified Network.AWS.EC2.Types.InstanceInterruptionBehavior as Types
import qualified Network.AWS.EC2.Types.LaunchSpecification as Types
import qualified Network.AWS.EC2.Types.RIProductDescription as Types
import qualified Network.AWS.EC2.Types.SpotInstanceState as Types
import qualified Network.AWS.EC2.Types.SpotInstanceStateFault as Types
import qualified Network.AWS.EC2.Types.SpotInstanceStatus as Types
import qualified Network.AWS.EC2.Types.SpotInstanceType as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Spot Instance request.
--
-- /See:/ 'mkSpotInstanceRequest' smart constructor.
data SpotInstanceRequest = SpotInstanceRequest'
  { actualBlockHourlyPrice :: Core.Maybe Core.Text
    -- ^ If you specified a duration and your Spot Instance request was fulfilled, this is the fixed hourly price in effect for the Spot Instance while it runs.
  , availabilityZoneGroup :: Core.Maybe Core.Text
    -- ^ The Availability Zone group. If you specify the same Availability Zone group for all Spot Instance requests, all Spot Instances are launched in the same Availability Zone.
  , blockDurationMinutes :: Core.Maybe Core.Int
    -- ^ The duration for the Spot Instance, in minutes.
  , createTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time when the Spot Instance request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
  , fault :: Core.Maybe Types.SpotInstanceStateFault
    -- ^ The fault codes for the Spot Instance request, if any.
  , instanceId :: Core.Maybe Types.InstanceId
    -- ^ The instance ID, if an instance has been launched to fulfill the Spot Instance request.
  , instanceInterruptionBehavior :: Core.Maybe Types.InstanceInterruptionBehavior
    -- ^ The behavior when a Spot Instance is interrupted.
  , launchGroup :: Core.Maybe Core.Text
    -- ^ The instance launch group. Launch groups are Spot Instances that launch together and terminate together.
  , launchSpecification :: Core.Maybe Types.LaunchSpecification
    -- ^ Additional information for launching instances.
  , launchedAvailabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone in which the request is launched.
  , productDescription :: Core.Maybe Types.RIProductDescription
    -- ^ The product description associated with the Spot Instance.
  , spotInstanceRequestId :: Core.Maybe Core.Text
    -- ^ The ID of the Spot Instance request.
  , spotPrice :: Core.Maybe Core.Text
    -- ^ The maximum price per hour that you are willing to pay for a Spot Instance.
  , state :: Core.Maybe Types.SpotInstanceState
    -- ^ The state of the Spot Instance request. Spot status information helps track your Spot Instance requests. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot status> in the /Amazon EC2 User Guide for Linux Instances/ .
  , status :: Core.Maybe Types.SpotInstanceStatus
    -- ^ The status code and status message describing the Spot Instance request.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the resource.
  , type' :: Core.Maybe Types.SpotInstanceType
    -- ^ The Spot Instance request type.
  , validFrom :: Core.Maybe Core.UTCTime
    -- ^ The start date of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The request becomes active at this date and time.
  , validUntil :: Core.Maybe Core.UTCTime
    -- ^ The end date of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
--
--     * For a persistent request, the request remains active until the @validUntil@ date and time is reached. Otherwise, the request remains active until you cancel it. 
--
--
--     * For a one-time request, the request remains active until all instances launch, the request is canceled, or the @validUntil@ date and time is reached. By default, the request is valid for 7 days from the date the request was created.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SpotInstanceRequest' value with any optional fields omitted.
mkSpotInstanceRequest
    :: SpotInstanceRequest
mkSpotInstanceRequest
  = SpotInstanceRequest'{actualBlockHourlyPrice = Core.Nothing,
                         availabilityZoneGroup = Core.Nothing,
                         blockDurationMinutes = Core.Nothing, createTime = Core.Nothing,
                         fault = Core.Nothing, instanceId = Core.Nothing,
                         instanceInterruptionBehavior = Core.Nothing,
                         launchGroup = Core.Nothing, launchSpecification = Core.Nothing,
                         launchedAvailabilityZone = Core.Nothing,
                         productDescription = Core.Nothing,
                         spotInstanceRequestId = Core.Nothing, spotPrice = Core.Nothing,
                         state = Core.Nothing, status = Core.Nothing, tags = Core.Nothing,
                         type' = Core.Nothing, validFrom = Core.Nothing,
                         validUntil = Core.Nothing}

-- | If you specified a duration and your Spot Instance request was fulfilled, this is the fixed hourly price in effect for the Spot Instance while it runs.
--
-- /Note:/ Consider using 'actualBlockHourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirActualBlockHourlyPrice :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Text)
sirActualBlockHourlyPrice = Lens.field @"actualBlockHourlyPrice"
{-# INLINEABLE sirActualBlockHourlyPrice #-}
{-# DEPRECATED actualBlockHourlyPrice "Use generic-lens or generic-optics with 'actualBlockHourlyPrice' instead"  #-}

-- | The Availability Zone group. If you specify the same Availability Zone group for all Spot Instance requests, all Spot Instances are launched in the same Availability Zone.
--
-- /Note:/ Consider using 'availabilityZoneGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirAvailabilityZoneGroup :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Text)
sirAvailabilityZoneGroup = Lens.field @"availabilityZoneGroup"
{-# INLINEABLE sirAvailabilityZoneGroup #-}
{-# DEPRECATED availabilityZoneGroup "Use generic-lens or generic-optics with 'availabilityZoneGroup' instead"  #-}

-- | The duration for the Spot Instance, in minutes.
--
-- /Note:/ Consider using 'blockDurationMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirBlockDurationMinutes :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Int)
sirBlockDurationMinutes = Lens.field @"blockDurationMinutes"
{-# INLINEABLE sirBlockDurationMinutes #-}
{-# DEPRECATED blockDurationMinutes "Use generic-lens or generic-optics with 'blockDurationMinutes' instead"  #-}

-- | The date and time when the Spot Instance request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirCreateTime :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.UTCTime)
sirCreateTime = Lens.field @"createTime"
{-# INLINEABLE sirCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | The fault codes for the Spot Instance request, if any.
--
-- /Note:/ Consider using 'fault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirFault :: Lens.Lens' SpotInstanceRequest (Core.Maybe Types.SpotInstanceStateFault)
sirFault = Lens.field @"fault"
{-# INLINEABLE sirFault #-}
{-# DEPRECATED fault "Use generic-lens or generic-optics with 'fault' instead"  #-}

-- | The instance ID, if an instance has been launched to fulfill the Spot Instance request.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirInstanceId :: Lens.Lens' SpotInstanceRequest (Core.Maybe Types.InstanceId)
sirInstanceId = Lens.field @"instanceId"
{-# INLINEABLE sirInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The behavior when a Spot Instance is interrupted.
--
-- /Note:/ Consider using 'instanceInterruptionBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirInstanceInterruptionBehavior :: Lens.Lens' SpotInstanceRequest (Core.Maybe Types.InstanceInterruptionBehavior)
sirInstanceInterruptionBehavior = Lens.field @"instanceInterruptionBehavior"
{-# INLINEABLE sirInstanceInterruptionBehavior #-}
{-# DEPRECATED instanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead"  #-}

-- | The instance launch group. Launch groups are Spot Instances that launch together and terminate together.
--
-- /Note:/ Consider using 'launchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirLaunchGroup :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Text)
sirLaunchGroup = Lens.field @"launchGroup"
{-# INLINEABLE sirLaunchGroup #-}
{-# DEPRECATED launchGroup "Use generic-lens or generic-optics with 'launchGroup' instead"  #-}

-- | Additional information for launching instances.
--
-- /Note:/ Consider using 'launchSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirLaunchSpecification :: Lens.Lens' SpotInstanceRequest (Core.Maybe Types.LaunchSpecification)
sirLaunchSpecification = Lens.field @"launchSpecification"
{-# INLINEABLE sirLaunchSpecification #-}
{-# DEPRECATED launchSpecification "Use generic-lens or generic-optics with 'launchSpecification' instead"  #-}

-- | The Availability Zone in which the request is launched.
--
-- /Note:/ Consider using 'launchedAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirLaunchedAvailabilityZone :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Text)
sirLaunchedAvailabilityZone = Lens.field @"launchedAvailabilityZone"
{-# INLINEABLE sirLaunchedAvailabilityZone #-}
{-# DEPRECATED launchedAvailabilityZone "Use generic-lens or generic-optics with 'launchedAvailabilityZone' instead"  #-}

-- | The product description associated with the Spot Instance.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirProductDescription :: Lens.Lens' SpotInstanceRequest (Core.Maybe Types.RIProductDescription)
sirProductDescription = Lens.field @"productDescription"
{-# INLINEABLE sirProductDescription #-}
{-# DEPRECATED productDescription "Use generic-lens or generic-optics with 'productDescription' instead"  #-}

-- | The ID of the Spot Instance request.
--
-- /Note:/ Consider using 'spotInstanceRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirSpotInstanceRequestId :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Text)
sirSpotInstanceRequestId = Lens.field @"spotInstanceRequestId"
{-# INLINEABLE sirSpotInstanceRequestId #-}
{-# DEPRECATED spotInstanceRequestId "Use generic-lens or generic-optics with 'spotInstanceRequestId' instead"  #-}

-- | The maximum price per hour that you are willing to pay for a Spot Instance.
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirSpotPrice :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Text)
sirSpotPrice = Lens.field @"spotPrice"
{-# INLINEABLE sirSpotPrice #-}
{-# DEPRECATED spotPrice "Use generic-lens or generic-optics with 'spotPrice' instead"  #-}

-- | The state of the Spot Instance request. Spot status information helps track your Spot Instance requests. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot status> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirState :: Lens.Lens' SpotInstanceRequest (Core.Maybe Types.SpotInstanceState)
sirState = Lens.field @"state"
{-# INLINEABLE sirState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The status code and status message describing the Spot Instance request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirStatus :: Lens.Lens' SpotInstanceRequest (Core.Maybe Types.SpotInstanceStatus)
sirStatus = Lens.field @"status"
{-# INLINEABLE sirStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Any tags assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirTags :: Lens.Lens' SpotInstanceRequest (Core.Maybe [Types.Tag])
sirTags = Lens.field @"tags"
{-# INLINEABLE sirTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The Spot Instance request type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirType :: Lens.Lens' SpotInstanceRequest (Core.Maybe Types.SpotInstanceType)
sirType = Lens.field @"type'"
{-# INLINEABLE sirType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The start date of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The request becomes active at this date and time.
--
-- /Note:/ Consider using 'validFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirValidFrom :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.UTCTime)
sirValidFrom = Lens.field @"validFrom"
{-# INLINEABLE sirValidFrom #-}
{-# DEPRECATED validFrom "Use generic-lens or generic-optics with 'validFrom' instead"  #-}

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
sirValidUntil :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.UTCTime)
sirValidUntil = Lens.field @"validUntil"
{-# INLINEABLE sirValidUntil #-}
{-# DEPRECATED validUntil "Use generic-lens or generic-optics with 'validUntil' instead"  #-}

instance Core.FromXML SpotInstanceRequest where
        parseXML x
          = SpotInstanceRequest' Core.<$>
              (x Core..@? "actualBlockHourlyPrice") Core.<*>
                x Core..@? "availabilityZoneGroup"
                Core.<*> x Core..@? "blockDurationMinutes"
                Core.<*> x Core..@? "createTime"
                Core.<*> x Core..@? "fault"
                Core.<*> x Core..@? "instanceId"
                Core.<*> x Core..@? "instanceInterruptionBehavior"
                Core.<*> x Core..@? "launchGroup"
                Core.<*> x Core..@? "launchSpecification"
                Core.<*> x Core..@? "launchedAvailabilityZone"
                Core.<*> x Core..@? "productDescription"
                Core.<*> x Core..@? "spotInstanceRequestId"
                Core.<*> x Core..@? "spotPrice"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "status"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "type"
                Core.<*> x Core..@? "validFrom"
                Core.<*> x Core..@? "validUntil"
