{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Event where

import Network.AWS.GameLift.Types.EventCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Log entry describing an event that involves Amazon GameLift resources (such as a fleet). In addition to tracking activity, event codes and messages can provide additional information for troubleshooting and debugging problems.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
  { _eResourceId :: !(Maybe Text),
    _ePreSignedLogURL :: !(Maybe Text),
    _eEventTime :: !(Maybe POSIX),
    _eMessage :: !(Maybe Text),
    _eEventCode :: !(Maybe EventCode),
    _eEventId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eResourceId' - A unique identifier for an event resource, such as a fleet ID.
--
-- * 'ePreSignedLogURL' - Location of stored logs with additional detail that is related to the event. This is useful for debugging issues. The URL is valid for 15 minutes. You can also access fleet creation logs through the Amazon GameLift console.
--
-- * 'eEventTime' - Time stamp indicating when this event occurred. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'eMessage' - Additional information related to the event.
--
-- * 'eEventCode' - The type of event being logged.  __Fleet creation events (ordered by fleet creation activity):__      * FLEET_CREATED -- A fleet resource was successfully created with a status of @NEW@ . Event messaging includes the fleet ID.     * FLEET_STATE_DOWNLOADING -- Fleet status changed from @NEW@ to @DOWNLOADING@ . The compressed build has started downloading to a fleet instance for installation.     * FLEET_BINARY_DOWNLOAD_FAILED -- The build failed to download to the fleet instance.     * FLEET_CREATION_EXTRACTING_BUILD – The game server build was successfully downloaded to an instance, and the build files are now being extracted from the uploaded build and saved to an instance. Failure at this stage prevents a fleet from moving to @ACTIVE@ status. Logs for this stage display a list of the files that are extracted and saved on the instance. Access the logs by using the URL in /PreSignedLogUrl/ .     * FLEET_CREATION_RUNNING_INSTALLER – The game server build files were successfully extracted, and the Amazon GameLift is now running the build's install script (if one is included). Failure in this stage prevents a fleet from moving to @ACTIVE@ status. Logs for this stage list the installation steps and whether or not the install completed successfully. Access the logs by using the URL in /PreSignedLogUrl/ .      * FLEET_CREATION_VALIDATING_RUNTIME_CONFIG -- The build process was successful, and the Amazon GameLift is now verifying that the game server launch paths, which are specified in the fleet's runtime configuration, exist. If any listed launch path exists, Amazon GameLift tries to launch a game server process and waits for the process to report ready. Failures in this stage prevent a fleet from moving to @ACTIVE@ status. Logs for this stage list the launch paths in the runtime configuration and indicate whether each is found. Access the logs by using the URL in /PreSignedLogUrl/ .      * FLEET_STATE_VALIDATING -- Fleet status changed from @DOWNLOADING@ to @VALIDATING@ .     * FLEET_VALIDATION_LAUNCH_PATH_NOT_FOUND -- Validation of the runtime configuration failed because the executable specified in a launch path does not exist on the instance.     * FLEET_STATE_BUILDING -- Fleet status changed from @VALIDATING@ to @BUILDING@ .     * FLEET_VALIDATION_EXECUTABLE_RUNTIME_FAILURE -- Validation of the runtime configuration failed because the executable specified in a launch path failed to run on the fleet instance.     * FLEET_STATE_ACTIVATING -- Fleet status changed from @BUILDING@ to @ACTIVATING@ .      * FLEET_ACTIVATION_FAILED - The fleet failed to successfully complete one of the steps in the fleet activation process. This event code indicates that the game build was successfully downloaded to a fleet instance, built, and validated, but was not able to start a server process. Learn more at <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html#fleets-creating-debug-creation Debug Fleet Creation Issues>      * FLEET_STATE_ACTIVE -- The fleet's status changed from @ACTIVATING@ to @ACTIVE@ . The fleet is now ready to host game sessions. __VPC peering events:__      * FLEET_VPC_PEERING_SUCCEEDED -- A VPC peering connection has been established between the VPC for an Amazon GameLift fleet and a VPC in your AWS account.     * FLEET_VPC_PEERING_FAILED -- A requested VPC peering connection has failed. Event details and status information (see 'DescribeVpcPeeringConnections' ) provide additional detail. A common reason for peering failure is that the two VPCs have overlapping CIDR blocks of IPv4 addresses. To resolve this, change the CIDR block for the VPC in your AWS account. For more information on VPC peering failures, see <https://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html https://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html>      * FLEET_VPC_PEERING_DELETED -- A VPC peering connection has been successfully deleted. __Spot instance events:__      * INSTANCE_INTERRUPTED -- A spot instance was interrupted by EC2 with a two-minute notification. __Other fleet events:__      * FLEET_SCALING_EVENT -- A change was made to the fleet's capacity settings (desired instances, minimum/maximum scaling limits). Event messaging includes the new capacity settings.     * FLEET_NEW_GAME_SESSION_PROTECTION_POLICY_UPDATED -- A change was made to the fleet's game session protection policy setting. Event messaging includes both the old and new policy setting.      * FLEET_DELETED -- A request to delete a fleet was initiated.     * GENERIC_EVENT -- An unspecified event has occurred.
--
-- * 'eEventId' - A unique identifier for a fleet event.
event ::
  Event
event =
  Event'
    { _eResourceId = Nothing,
      _ePreSignedLogURL = Nothing,
      _eEventTime = Nothing,
      _eMessage = Nothing,
      _eEventCode = Nothing,
      _eEventId = Nothing
    }

-- | A unique identifier for an event resource, such as a fleet ID.
eResourceId :: Lens' Event (Maybe Text)
eResourceId = lens _eResourceId (\s a -> s {_eResourceId = a})

-- | Location of stored logs with additional detail that is related to the event. This is useful for debugging issues. The URL is valid for 15 minutes. You can also access fleet creation logs through the Amazon GameLift console.
ePreSignedLogURL :: Lens' Event (Maybe Text)
ePreSignedLogURL = lens _ePreSignedLogURL (\s a -> s {_ePreSignedLogURL = a})

-- | Time stamp indicating when this event occurred. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
eEventTime :: Lens' Event (Maybe UTCTime)
eEventTime = lens _eEventTime (\s a -> s {_eEventTime = a}) . mapping _Time

-- | Additional information related to the event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\s a -> s {_eMessage = a})

-- | The type of event being logged.  __Fleet creation events (ordered by fleet creation activity):__      * FLEET_CREATED -- A fleet resource was successfully created with a status of @NEW@ . Event messaging includes the fleet ID.     * FLEET_STATE_DOWNLOADING -- Fleet status changed from @NEW@ to @DOWNLOADING@ . The compressed build has started downloading to a fleet instance for installation.     * FLEET_BINARY_DOWNLOAD_FAILED -- The build failed to download to the fleet instance.     * FLEET_CREATION_EXTRACTING_BUILD – The game server build was successfully downloaded to an instance, and the build files are now being extracted from the uploaded build and saved to an instance. Failure at this stage prevents a fleet from moving to @ACTIVE@ status. Logs for this stage display a list of the files that are extracted and saved on the instance. Access the logs by using the URL in /PreSignedLogUrl/ .     * FLEET_CREATION_RUNNING_INSTALLER – The game server build files were successfully extracted, and the Amazon GameLift is now running the build's install script (if one is included). Failure in this stage prevents a fleet from moving to @ACTIVE@ status. Logs for this stage list the installation steps and whether or not the install completed successfully. Access the logs by using the URL in /PreSignedLogUrl/ .      * FLEET_CREATION_VALIDATING_RUNTIME_CONFIG -- The build process was successful, and the Amazon GameLift is now verifying that the game server launch paths, which are specified in the fleet's runtime configuration, exist. If any listed launch path exists, Amazon GameLift tries to launch a game server process and waits for the process to report ready. Failures in this stage prevent a fleet from moving to @ACTIVE@ status. Logs for this stage list the launch paths in the runtime configuration and indicate whether each is found. Access the logs by using the URL in /PreSignedLogUrl/ .      * FLEET_STATE_VALIDATING -- Fleet status changed from @DOWNLOADING@ to @VALIDATING@ .     * FLEET_VALIDATION_LAUNCH_PATH_NOT_FOUND -- Validation of the runtime configuration failed because the executable specified in a launch path does not exist on the instance.     * FLEET_STATE_BUILDING -- Fleet status changed from @VALIDATING@ to @BUILDING@ .     * FLEET_VALIDATION_EXECUTABLE_RUNTIME_FAILURE -- Validation of the runtime configuration failed because the executable specified in a launch path failed to run on the fleet instance.     * FLEET_STATE_ACTIVATING -- Fleet status changed from @BUILDING@ to @ACTIVATING@ .      * FLEET_ACTIVATION_FAILED - The fleet failed to successfully complete one of the steps in the fleet activation process. This event code indicates that the game build was successfully downloaded to a fleet instance, built, and validated, but was not able to start a server process. Learn more at <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html#fleets-creating-debug-creation Debug Fleet Creation Issues>      * FLEET_STATE_ACTIVE -- The fleet's status changed from @ACTIVATING@ to @ACTIVE@ . The fleet is now ready to host game sessions. __VPC peering events:__      * FLEET_VPC_PEERING_SUCCEEDED -- A VPC peering connection has been established between the VPC for an Amazon GameLift fleet and a VPC in your AWS account.     * FLEET_VPC_PEERING_FAILED -- A requested VPC peering connection has failed. Event details and status information (see 'DescribeVpcPeeringConnections' ) provide additional detail. A common reason for peering failure is that the two VPCs have overlapping CIDR blocks of IPv4 addresses. To resolve this, change the CIDR block for the VPC in your AWS account. For more information on VPC peering failures, see <https://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html https://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html>      * FLEET_VPC_PEERING_DELETED -- A VPC peering connection has been successfully deleted. __Spot instance events:__      * INSTANCE_INTERRUPTED -- A spot instance was interrupted by EC2 with a two-minute notification. __Other fleet events:__      * FLEET_SCALING_EVENT -- A change was made to the fleet's capacity settings (desired instances, minimum/maximum scaling limits). Event messaging includes the new capacity settings.     * FLEET_NEW_GAME_SESSION_PROTECTION_POLICY_UPDATED -- A change was made to the fleet's game session protection policy setting. Event messaging includes both the old and new policy setting.      * FLEET_DELETED -- A request to delete a fleet was initiated.     * GENERIC_EVENT -- An unspecified event has occurred.
eEventCode :: Lens' Event (Maybe EventCode)
eEventCode = lens _eEventCode (\s a -> s {_eEventCode = a})

-- | A unique identifier for a fleet event.
eEventId :: Lens' Event (Maybe Text)
eEventId = lens _eEventId (\s a -> s {_eEventId = a})

instance FromJSON Event where
  parseJSON =
    withObject
      "Event"
      ( \x ->
          Event'
            <$> (x .:? "ResourceId")
            <*> (x .:? "PreSignedLogUrl")
            <*> (x .:? "EventTime")
            <*> (x .:? "Message")
            <*> (x .:? "EventCode")
            <*> (x .:? "EventId")
      )

instance Hashable Event

instance NFData Event
