{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.Event
  ( Event (..)
  -- * Smart constructor
  , mkEvent
  -- * Lenses
  , eEventCode
  , eEventId
  , eEventTime
  , eMessage
  , ePreSignedLogUrl
  , eResourceId
  ) where

import qualified Network.AWS.GameLift.Types.EventCode as Types
import qualified Network.AWS.GameLift.Types.EventId as Types
import qualified Network.AWS.GameLift.Types.Message as Types
import qualified Network.AWS.GameLift.Types.PreSignedLogUrl as Types
import qualified Network.AWS.GameLift.Types.ResourceId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Log entry describing an event that involves Amazon GameLift resources (such as a fleet). In addition to tracking activity, event codes and messages can provide additional information for troubleshooting and debugging problems.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { eventCode :: Core.Maybe Types.EventCode
    -- ^ The type of event being logged. 
--
-- __Fleet creation events (ordered by fleet creation activity):__ 
--
--     * FLEET_CREATED -- A fleet resource was successfully created with a status of @NEW@ . Event messaging includes the fleet ID.
--
--
--     * FLEET_STATE_DOWNLOADING -- Fleet status changed from @NEW@ to @DOWNLOADING@ . The compressed build has started downloading to a fleet instance for installation.
--
--
--     * FLEET_BINARY_DOWNLOAD_FAILED -- The build failed to download to the fleet instance.
--
--
--     * FLEET_CREATION_EXTRACTING_BUILD – The game server build was successfully downloaded to an instance, and the build files are now being extracted from the uploaded build and saved to an instance. Failure at this stage prevents a fleet from moving to @ACTIVE@ status. Logs for this stage display a list of the files that are extracted and saved on the instance. Access the logs by using the URL in /PreSignedLogUrl/ .
--
--
--     * FLEET_CREATION_RUNNING_INSTALLER – The game server build files were successfully extracted, and the Amazon GameLift is now running the build's install script (if one is included). Failure in this stage prevents a fleet from moving to @ACTIVE@ status. Logs for this stage list the installation steps and whether or not the install completed successfully. Access the logs by using the URL in /PreSignedLogUrl/ . 
--
--
--     * FLEET_CREATION_VALIDATING_RUNTIME_CONFIG -- The build process was successful, and the Amazon GameLift is now verifying that the game server launch paths, which are specified in the fleet's runtime configuration, exist. If any listed launch path exists, Amazon GameLift tries to launch a game server process and waits for the process to report ready. Failures in this stage prevent a fleet from moving to @ACTIVE@ status. Logs for this stage list the launch paths in the runtime configuration and indicate whether each is found. Access the logs by using the URL in /PreSignedLogUrl/ . 
--
--
--     * FLEET_STATE_VALIDATING -- Fleet status changed from @DOWNLOADING@ to @VALIDATING@ .
--
--
--     * FLEET_VALIDATION_LAUNCH_PATH_NOT_FOUND -- Validation of the runtime configuration failed because the executable specified in a launch path does not exist on the instance.
--
--
--     * FLEET_STATE_BUILDING -- Fleet status changed from @VALIDATING@ to @BUILDING@ .
--
--
--     * FLEET_VALIDATION_EXECUTABLE_RUNTIME_FAILURE -- Validation of the runtime configuration failed because the executable specified in a launch path failed to run on the fleet instance.
--
--
--     * FLEET_STATE_ACTIVATING -- Fleet status changed from @BUILDING@ to @ACTIVATING@ . 
--
--
--     * FLEET_ACTIVATION_FAILED - The fleet failed to successfully complete one of the steps in the fleet activation process. This event code indicates that the game build was successfully downloaded to a fleet instance, built, and validated, but was not able to start a server process. Learn more at <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html#fleets-creating-debug-creation Debug Fleet Creation Issues> 
--
--
--     * FLEET_STATE_ACTIVE -- The fleet's status changed from @ACTIVATING@ to @ACTIVE@ . The fleet is now ready to host game sessions.
--
--
-- __VPC peering events:__ 
--
--     * FLEET_VPC_PEERING_SUCCEEDED -- A VPC peering connection has been established between the VPC for an Amazon GameLift fleet and a VPC in your AWS account.
--
--
--     * FLEET_VPC_PEERING_FAILED -- A requested VPC peering connection has failed. Event details and status information (see 'DescribeVpcPeeringConnections' ) provide additional detail. A common reason for peering failure is that the two VPCs have overlapping CIDR blocks of IPv4 addresses. To resolve this, change the CIDR block for the VPC in your AWS account. For more information on VPC peering failures, see <https://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html https://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html> 
--
--
--     * FLEET_VPC_PEERING_DELETED -- A VPC peering connection has been successfully deleted.
--
--
-- __Spot instance events:__ 
--
--     * INSTANCE_INTERRUPTED -- A spot instance was interrupted by EC2 with a two-minute notification.
--
--
-- __Other fleet events:__ 
--
--     * FLEET_SCALING_EVENT -- A change was made to the fleet's capacity settings (desired instances, minimum/maximum scaling limits). Event messaging includes the new capacity settings.
--
--
--     * FLEET_NEW_GAME_SESSION_PROTECTION_POLICY_UPDATED -- A change was made to the fleet's game session protection policy setting. Event messaging includes both the old and new policy setting. 
--
--
--     * FLEET_DELETED -- A request to delete a fleet was initiated.
--
--
--     * GENERIC_EVENT -- An unspecified event has occurred.
--
--
  , eventId :: Core.Maybe Types.EventId
    -- ^ A unique identifier for a fleet event.
  , eventTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Time stamp indicating when this event occurred. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
  , message :: Core.Maybe Types.Message
    -- ^ Additional information related to the event.
  , preSignedLogUrl :: Core.Maybe Types.PreSignedLogUrl
    -- ^ Location of stored logs with additional detail that is related to the event. This is useful for debugging issues. The URL is valid for 15 minutes. You can also access fleet creation logs through the Amazon GameLift console.
  , resourceId :: Core.Maybe Types.ResourceId
    -- ^ A unique identifier for an event resource, such as a fleet ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Event' value with any optional fields omitted.
mkEvent
    :: Event
mkEvent
  = Event'{eventCode = Core.Nothing, eventId = Core.Nothing,
           eventTime = Core.Nothing, message = Core.Nothing,
           preSignedLogUrl = Core.Nothing, resourceId = Core.Nothing}

-- | The type of event being logged. 
--
-- __Fleet creation events (ordered by fleet creation activity):__ 
--
--     * FLEET_CREATED -- A fleet resource was successfully created with a status of @NEW@ . Event messaging includes the fleet ID.
--
--
--     * FLEET_STATE_DOWNLOADING -- Fleet status changed from @NEW@ to @DOWNLOADING@ . The compressed build has started downloading to a fleet instance for installation.
--
--
--     * FLEET_BINARY_DOWNLOAD_FAILED -- The build failed to download to the fleet instance.
--
--
--     * FLEET_CREATION_EXTRACTING_BUILD – The game server build was successfully downloaded to an instance, and the build files are now being extracted from the uploaded build and saved to an instance. Failure at this stage prevents a fleet from moving to @ACTIVE@ status. Logs for this stage display a list of the files that are extracted and saved on the instance. Access the logs by using the URL in /PreSignedLogUrl/ .
--
--
--     * FLEET_CREATION_RUNNING_INSTALLER – The game server build files were successfully extracted, and the Amazon GameLift is now running the build's install script (if one is included). Failure in this stage prevents a fleet from moving to @ACTIVE@ status. Logs for this stage list the installation steps and whether or not the install completed successfully. Access the logs by using the URL in /PreSignedLogUrl/ . 
--
--
--     * FLEET_CREATION_VALIDATING_RUNTIME_CONFIG -- The build process was successful, and the Amazon GameLift is now verifying that the game server launch paths, which are specified in the fleet's runtime configuration, exist. If any listed launch path exists, Amazon GameLift tries to launch a game server process and waits for the process to report ready. Failures in this stage prevent a fleet from moving to @ACTIVE@ status. Logs for this stage list the launch paths in the runtime configuration and indicate whether each is found. Access the logs by using the URL in /PreSignedLogUrl/ . 
--
--
--     * FLEET_STATE_VALIDATING -- Fleet status changed from @DOWNLOADING@ to @VALIDATING@ .
--
--
--     * FLEET_VALIDATION_LAUNCH_PATH_NOT_FOUND -- Validation of the runtime configuration failed because the executable specified in a launch path does not exist on the instance.
--
--
--     * FLEET_STATE_BUILDING -- Fleet status changed from @VALIDATING@ to @BUILDING@ .
--
--
--     * FLEET_VALIDATION_EXECUTABLE_RUNTIME_FAILURE -- Validation of the runtime configuration failed because the executable specified in a launch path failed to run on the fleet instance.
--
--
--     * FLEET_STATE_ACTIVATING -- Fleet status changed from @BUILDING@ to @ACTIVATING@ . 
--
--
--     * FLEET_ACTIVATION_FAILED - The fleet failed to successfully complete one of the steps in the fleet activation process. This event code indicates that the game build was successfully downloaded to a fleet instance, built, and validated, but was not able to start a server process. Learn more at <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html#fleets-creating-debug-creation Debug Fleet Creation Issues> 
--
--
--     * FLEET_STATE_ACTIVE -- The fleet's status changed from @ACTIVATING@ to @ACTIVE@ . The fleet is now ready to host game sessions.
--
--
-- __VPC peering events:__ 
--
--     * FLEET_VPC_PEERING_SUCCEEDED -- A VPC peering connection has been established between the VPC for an Amazon GameLift fleet and a VPC in your AWS account.
--
--
--     * FLEET_VPC_PEERING_FAILED -- A requested VPC peering connection has failed. Event details and status information (see 'DescribeVpcPeeringConnections' ) provide additional detail. A common reason for peering failure is that the two VPCs have overlapping CIDR blocks of IPv4 addresses. To resolve this, change the CIDR block for the VPC in your AWS account. For more information on VPC peering failures, see <https://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html https://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html> 
--
--
--     * FLEET_VPC_PEERING_DELETED -- A VPC peering connection has been successfully deleted.
--
--
-- __Spot instance events:__ 
--
--     * INSTANCE_INTERRUPTED -- A spot instance was interrupted by EC2 with a two-minute notification.
--
--
-- __Other fleet events:__ 
--
--     * FLEET_SCALING_EVENT -- A change was made to the fleet's capacity settings (desired instances, minimum/maximum scaling limits). Event messaging includes the new capacity settings.
--
--
--     * FLEET_NEW_GAME_SESSION_PROTECTION_POLICY_UPDATED -- A change was made to the fleet's game session protection policy setting. Event messaging includes both the old and new policy setting. 
--
--
--     * FLEET_DELETED -- A request to delete a fleet was initiated.
--
--
--     * GENERIC_EVENT -- An unspecified event has occurred.
--
--
--
-- /Note:/ Consider using 'eventCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventCode :: Lens.Lens' Event (Core.Maybe Types.EventCode)
eEventCode = Lens.field @"eventCode"
{-# INLINEABLE eEventCode #-}
{-# DEPRECATED eventCode "Use generic-lens or generic-optics with 'eventCode' instead"  #-}

-- | A unique identifier for a fleet event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventId :: Lens.Lens' Event (Core.Maybe Types.EventId)
eEventId = Lens.field @"eventId"
{-# INLINEABLE eEventId #-}
{-# DEPRECATED eventId "Use generic-lens or generic-optics with 'eventId' instead"  #-}

-- | Time stamp indicating when this event occurred. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'eventTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventTime :: Lens.Lens' Event (Core.Maybe Core.NominalDiffTime)
eEventTime = Lens.field @"eventTime"
{-# INLINEABLE eEventTime #-}
{-# DEPRECATED eventTime "Use generic-lens or generic-optics with 'eventTime' instead"  #-}

-- | Additional information related to the event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Event (Core.Maybe Types.Message)
eMessage = Lens.field @"message"
{-# INLINEABLE eMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | Location of stored logs with additional detail that is related to the event. This is useful for debugging issues. The URL is valid for 15 minutes. You can also access fleet creation logs through the Amazon GameLift console.
--
-- /Note:/ Consider using 'preSignedLogUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePreSignedLogUrl :: Lens.Lens' Event (Core.Maybe Types.PreSignedLogUrl)
ePreSignedLogUrl = Lens.field @"preSignedLogUrl"
{-# INLINEABLE ePreSignedLogUrl #-}
{-# DEPRECATED preSignedLogUrl "Use generic-lens or generic-optics with 'preSignedLogUrl' instead"  #-}

-- | A unique identifier for an event resource, such as a fleet ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eResourceId :: Lens.Lens' Event (Core.Maybe Types.ResourceId)
eResourceId = Lens.field @"resourceId"
{-# INLINEABLE eResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

instance Core.FromJSON Event where
        parseJSON
          = Core.withObject "Event" Core.$
              \ x ->
                Event' Core.<$>
                  (x Core..:? "EventCode") Core.<*> x Core..:? "EventId" Core.<*>
                    x Core..:? "EventTime"
                    Core.<*> x Core..:? "Message"
                    Core.<*> x Core..:? "PreSignedLogUrl"
                    Core.<*> x Core..:? "ResourceId"
