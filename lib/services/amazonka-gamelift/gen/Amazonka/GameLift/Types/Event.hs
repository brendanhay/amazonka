{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GameLift.Types.Event
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.Event where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.EventCode
import qualified Amazonka.Prelude as Prelude

-- | Log entry describing an event that involves GameLift resources (such as
-- a fleet). In addition to tracking activity, event codes and messages can
-- provide additional information for troubleshooting and debugging
-- problems.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The type of event being logged.
    --
    -- __Fleet state transition events:__
    --
    -- -   FLEET_CREATED -- A fleet resource was successfully created with a
    --     status of @NEW@. Event messaging includes the fleet ID.
    --
    -- -   FLEET_STATE_DOWNLOADING -- Fleet status changed from @NEW@ to
    --     @DOWNLOADING@. The compressed build has started downloading to a
    --     fleet instance for installation.
    --
    -- -   FLEET_STATE_VALIDATING -- Fleet status changed from @DOWNLOADING@ to
    --     @VALIDATING@. GameLift has successfully downloaded the build and is
    --     now validating the build files.
    --
    -- -   FLEET_STATE_BUILDING -- Fleet status changed from @VALIDATING@ to
    --     @BUILDING@. GameLift has successfully verified the build files and
    --     is now running the installation scripts.
    --
    -- -   FLEET_STATE_ACTIVATING -- Fleet status changed from @BUILDING@ to
    --     @ACTIVATING@. GameLift is trying to launch an instance and test the
    --     connectivity between the build and the GameLift Service via the
    --     Server SDK.
    --
    -- -   FLEET_STATE_ACTIVE -- The fleet\'s status changed from @ACTIVATING@
    --     to @ACTIVE@. The fleet is now ready to host game sessions.
    --
    -- -   FLEET_STATE_ERROR -- The Fleet\'s status changed to @ERROR@.
    --     Describe the fleet event message for more details.
    --
    -- __Fleet creation events (ordered by fleet creation activity):__
    --
    -- -   FLEET_BINARY_DOWNLOAD_FAILED -- The build failed to download to the
    --     fleet instance.
    --
    -- -   FLEET_CREATION_EXTRACTING_BUILD -- The game server build was
    --     successfully downloaded to an instance, and the build files are now
    --     being extracted from the uploaded build and saved to an instance.
    --     Failure at this stage prevents a fleet from moving to ACTIVE status.
    --     Logs for this stage display a list of the files that are extracted
    --     and saved on the instance. Access the logs by using the URL in
    --     /PreSignedLogUrl/.
    --
    -- -   FLEET_CREATION_RUNNING_INSTALLER -- The game server build files were
    --     successfully extracted, and the GameLift is now running the build\'s
    --     install script (if one is included). Failure in this stage prevents
    --     a fleet from moving to ACTIVE status. Logs for this stage list the
    --     installation steps and whether or not the install completed
    --     successfully. Access the logs by using the URL in /PreSignedLogUrl/.
    --
    -- -   FLEET_CREATION_VALIDATING_RUNTIME_CONFIG -- The build process was
    --     successful, and the GameLift is now verifying that the game server
    --     launch paths, which are specified in the fleet\'s runtime
    --     configuration, exist. If any listed launch path exists, GameLift
    --     tries to launch a game server process and waits for the process to
    --     report ready. Failures in this stage prevent a fleet from moving to
    --     @ACTIVE@ status. Logs for this stage list the launch paths in the
    --     runtime configuration and indicate whether each is found. Access the
    --     logs by using the URL in /PreSignedLogUrl/.
    --
    -- -   FLEET_VALIDATION_LAUNCH_PATH_NOT_FOUND -- Validation of the runtime
    --     configuration failed because the executable specified in a launch
    --     path does not exist on the instance.
    --
    -- -   FLEET_VALIDATION_EXECUTABLE_RUNTIME_FAILURE -- Validation of the
    --     runtime configuration failed because the executable specified in a
    --     launch path failed to run on the fleet instance.
    --
    -- -   FLEET_VALIDATION_TIMED_OUT -- Validation of the fleet at the end of
    --     creation timed out. Try fleet creation again.
    --
    -- -   FLEET_ACTIVATION_FAILED -- The fleet failed to successfully complete
    --     one of the steps in the fleet activation process. This event code
    --     indicates that the game build was successfully downloaded to a fleet
    --     instance, built, and validated, but was not able to start a server
    --     process. For more information, see
    --     <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html#fleets-creating-debug-creation Debug Fleet Creation Issues>.
    --
    -- -   FLEET_ACTIVATION_FAILED_NO_INSTANCES -- Fleet creation was not able
    --     to obtain any instances based on the input fleet attributes. Try
    --     again at a different time or choose a different combination of fleet
    --     attributes such as fleet type, instance type, etc.
    --
    -- -   FLEET_INITIALIZATION_FAILED -- A generic exception occurred during
    --     fleet creation. Describe the fleet event message for more details.
    --
    -- __VPC peering events:__
    --
    -- -   FLEET_VPC_PEERING_SUCCEEDED -- A VPC peering connection has been
    --     established between the VPC for an GameLift fleet and a VPC in your
    --     Amazon Web Services account.
    --
    -- -   FLEET_VPC_PEERING_FAILED -- A requested VPC peering connection has
    --     failed. Event details and status information provide additional
    --     detail. A common reason for peering failure is that the two VPCs
    --     have overlapping CIDR blocks of IPv4 addresses. To resolve this,
    --     change the CIDR block for the VPC in your Amazon Web Services
    --     account. For more information on VPC peering failures, see
    --     <https://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html>
    --
    -- -   FLEET_VPC_PEERING_DELETED -- A VPC peering connection has been
    --     successfully deleted.
    --
    -- __Spot instance events:__
    --
    -- -   INSTANCE_INTERRUPTED -- A spot instance was interrupted by EC2 with
    --     a two-minute notification.
    --
    -- __Server process events:__
    --
    -- -   SERVER_PROCESS_INVALID_PATH -- The game server executable or script
    --     could not be found based on the Fleet runtime configuration. Check
    --     that the launch path is correct based on the operating system of the
    --     Fleet.
    --
    -- -   SERVER_PROCESS_SDK_INITIALIZATION_TIMEOUT -- The server process did
    --     not call InitSDK() within the time expected. Check your game session
    --     log to see why InitSDK() was not called in time.
    --
    -- -   SERVER_PROCESS_PROCESS_READY_TIMEOUT -- The server process did not
    --     call ProcessReady() within the time expected after calling
    --     InitSDK(). Check your game session log to see why ProcessReady() was
    --     not called in time.
    --
    -- -   SERVER_PROCESS_CRASHED -- The server process exited without calling
    --     ProcessEnding(). Check your game session log to see why
    --     ProcessEnding() was not called.
    --
    -- -   SERVER_PROCESS_TERMINATED_UNHEALTHY -- The server process did not
    --     report a valid health check for too long and was therefore
    --     terminated by GameLift. Check your game session log to see if the
    --     thread became stuck processing a synchronous task for too long.
    --
    -- -   SERVER_PROCESS_FORCE_TERMINATED -- The server process did not exit
    --     cleanly after OnProcessTerminate() was sent within the time
    --     expected. Check your game session log to see why termination took
    --     longer than expected.
    --
    -- -   SERVER_PROCESS_PROCESS_EXIT_TIMEOUT -- The server process did not
    --     exit cleanly within the time expected after calling ProcessEnding().
    --     Check your game session log to see why termination took longer than
    --     expected.
    --
    -- __Game session events:__
    --
    -- -   GAME_SESSION_ACTIVATION_TIMEOUT -- GameSession failed to activate
    --     within the expected time. Check your game session log to see why
    --     ActivateGameSession() took longer to complete than expected.
    --
    -- __Other fleet events:__
    --
    -- -   FLEET_SCALING_EVENT -- A change was made to the fleet\'s capacity
    --     settings (desired instances, minimum\/maximum scaling limits). Event
    --     messaging includes the new capacity settings.
    --
    -- -   FLEET_NEW_GAME_SESSION_PROTECTION_POLICY_UPDATED -- A change was
    --     made to the fleet\'s game session protection policy setting. Event
    --     messaging includes both the old and new policy setting.
    --
    -- -   FLEET_DELETED -- A request to delete a fleet was initiated.
    --
    -- -   GENERIC_EVENT -- An unspecified event has occurred.
    eventCode :: Prelude.Maybe EventCode,
    -- | A unique identifier for a fleet event.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | Time stamp indicating when this event occurred. Format is a number
    -- expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    eventTime :: Prelude.Maybe Data.POSIX,
    -- | Additional information related to the event.
    message :: Prelude.Maybe Prelude.Text,
    -- | Location of stored logs with additional detail that is related to the
    -- event. This is useful for debugging issues. The URL is valid for 15
    -- minutes. You can also access fleet creation logs through the GameLift
    -- console.
    preSignedLogUrl :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for an event resource, such as a fleet ID.
    resourceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventCode', 'event_eventCode' - The type of event being logged.
--
-- __Fleet state transition events:__
--
-- -   FLEET_CREATED -- A fleet resource was successfully created with a
--     status of @NEW@. Event messaging includes the fleet ID.
--
-- -   FLEET_STATE_DOWNLOADING -- Fleet status changed from @NEW@ to
--     @DOWNLOADING@. The compressed build has started downloading to a
--     fleet instance for installation.
--
-- -   FLEET_STATE_VALIDATING -- Fleet status changed from @DOWNLOADING@ to
--     @VALIDATING@. GameLift has successfully downloaded the build and is
--     now validating the build files.
--
-- -   FLEET_STATE_BUILDING -- Fleet status changed from @VALIDATING@ to
--     @BUILDING@. GameLift has successfully verified the build files and
--     is now running the installation scripts.
--
-- -   FLEET_STATE_ACTIVATING -- Fleet status changed from @BUILDING@ to
--     @ACTIVATING@. GameLift is trying to launch an instance and test the
--     connectivity between the build and the GameLift Service via the
--     Server SDK.
--
-- -   FLEET_STATE_ACTIVE -- The fleet\'s status changed from @ACTIVATING@
--     to @ACTIVE@. The fleet is now ready to host game sessions.
--
-- -   FLEET_STATE_ERROR -- The Fleet\'s status changed to @ERROR@.
--     Describe the fleet event message for more details.
--
-- __Fleet creation events (ordered by fleet creation activity):__
--
-- -   FLEET_BINARY_DOWNLOAD_FAILED -- The build failed to download to the
--     fleet instance.
--
-- -   FLEET_CREATION_EXTRACTING_BUILD -- The game server build was
--     successfully downloaded to an instance, and the build files are now
--     being extracted from the uploaded build and saved to an instance.
--     Failure at this stage prevents a fleet from moving to ACTIVE status.
--     Logs for this stage display a list of the files that are extracted
--     and saved on the instance. Access the logs by using the URL in
--     /PreSignedLogUrl/.
--
-- -   FLEET_CREATION_RUNNING_INSTALLER -- The game server build files were
--     successfully extracted, and the GameLift is now running the build\'s
--     install script (if one is included). Failure in this stage prevents
--     a fleet from moving to ACTIVE status. Logs for this stage list the
--     installation steps and whether or not the install completed
--     successfully. Access the logs by using the URL in /PreSignedLogUrl/.
--
-- -   FLEET_CREATION_VALIDATING_RUNTIME_CONFIG -- The build process was
--     successful, and the GameLift is now verifying that the game server
--     launch paths, which are specified in the fleet\'s runtime
--     configuration, exist. If any listed launch path exists, GameLift
--     tries to launch a game server process and waits for the process to
--     report ready. Failures in this stage prevent a fleet from moving to
--     @ACTIVE@ status. Logs for this stage list the launch paths in the
--     runtime configuration and indicate whether each is found. Access the
--     logs by using the URL in /PreSignedLogUrl/.
--
-- -   FLEET_VALIDATION_LAUNCH_PATH_NOT_FOUND -- Validation of the runtime
--     configuration failed because the executable specified in a launch
--     path does not exist on the instance.
--
-- -   FLEET_VALIDATION_EXECUTABLE_RUNTIME_FAILURE -- Validation of the
--     runtime configuration failed because the executable specified in a
--     launch path failed to run on the fleet instance.
--
-- -   FLEET_VALIDATION_TIMED_OUT -- Validation of the fleet at the end of
--     creation timed out. Try fleet creation again.
--
-- -   FLEET_ACTIVATION_FAILED -- The fleet failed to successfully complete
--     one of the steps in the fleet activation process. This event code
--     indicates that the game build was successfully downloaded to a fleet
--     instance, built, and validated, but was not able to start a server
--     process. For more information, see
--     <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html#fleets-creating-debug-creation Debug Fleet Creation Issues>.
--
-- -   FLEET_ACTIVATION_FAILED_NO_INSTANCES -- Fleet creation was not able
--     to obtain any instances based on the input fleet attributes. Try
--     again at a different time or choose a different combination of fleet
--     attributes such as fleet type, instance type, etc.
--
-- -   FLEET_INITIALIZATION_FAILED -- A generic exception occurred during
--     fleet creation. Describe the fleet event message for more details.
--
-- __VPC peering events:__
--
-- -   FLEET_VPC_PEERING_SUCCEEDED -- A VPC peering connection has been
--     established between the VPC for an GameLift fleet and a VPC in your
--     Amazon Web Services account.
--
-- -   FLEET_VPC_PEERING_FAILED -- A requested VPC peering connection has
--     failed. Event details and status information provide additional
--     detail. A common reason for peering failure is that the two VPCs
--     have overlapping CIDR blocks of IPv4 addresses. To resolve this,
--     change the CIDR block for the VPC in your Amazon Web Services
--     account. For more information on VPC peering failures, see
--     <https://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html>
--
-- -   FLEET_VPC_PEERING_DELETED -- A VPC peering connection has been
--     successfully deleted.
--
-- __Spot instance events:__
--
-- -   INSTANCE_INTERRUPTED -- A spot instance was interrupted by EC2 with
--     a two-minute notification.
--
-- __Server process events:__
--
-- -   SERVER_PROCESS_INVALID_PATH -- The game server executable or script
--     could not be found based on the Fleet runtime configuration. Check
--     that the launch path is correct based on the operating system of the
--     Fleet.
--
-- -   SERVER_PROCESS_SDK_INITIALIZATION_TIMEOUT -- The server process did
--     not call InitSDK() within the time expected. Check your game session
--     log to see why InitSDK() was not called in time.
--
-- -   SERVER_PROCESS_PROCESS_READY_TIMEOUT -- The server process did not
--     call ProcessReady() within the time expected after calling
--     InitSDK(). Check your game session log to see why ProcessReady() was
--     not called in time.
--
-- -   SERVER_PROCESS_CRASHED -- The server process exited without calling
--     ProcessEnding(). Check your game session log to see why
--     ProcessEnding() was not called.
--
-- -   SERVER_PROCESS_TERMINATED_UNHEALTHY -- The server process did not
--     report a valid health check for too long and was therefore
--     terminated by GameLift. Check your game session log to see if the
--     thread became stuck processing a synchronous task for too long.
--
-- -   SERVER_PROCESS_FORCE_TERMINATED -- The server process did not exit
--     cleanly after OnProcessTerminate() was sent within the time
--     expected. Check your game session log to see why termination took
--     longer than expected.
--
-- -   SERVER_PROCESS_PROCESS_EXIT_TIMEOUT -- The server process did not
--     exit cleanly within the time expected after calling ProcessEnding().
--     Check your game session log to see why termination took longer than
--     expected.
--
-- __Game session events:__
--
-- -   GAME_SESSION_ACTIVATION_TIMEOUT -- GameSession failed to activate
--     within the expected time. Check your game session log to see why
--     ActivateGameSession() took longer to complete than expected.
--
-- __Other fleet events:__
--
-- -   FLEET_SCALING_EVENT -- A change was made to the fleet\'s capacity
--     settings (desired instances, minimum\/maximum scaling limits). Event
--     messaging includes the new capacity settings.
--
-- -   FLEET_NEW_GAME_SESSION_PROTECTION_POLICY_UPDATED -- A change was
--     made to the fleet\'s game session protection policy setting. Event
--     messaging includes both the old and new policy setting.
--
-- -   FLEET_DELETED -- A request to delete a fleet was initiated.
--
-- -   GENERIC_EVENT -- An unspecified event has occurred.
--
-- 'eventId', 'event_eventId' - A unique identifier for a fleet event.
--
-- 'eventTime', 'event_eventTime' - Time stamp indicating when this event occurred. Format is a number
-- expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'message', 'event_message' - Additional information related to the event.
--
-- 'preSignedLogUrl', 'event_preSignedLogUrl' - Location of stored logs with additional detail that is related to the
-- event. This is useful for debugging issues. The URL is valid for 15
-- minutes. You can also access fleet creation logs through the GameLift
-- console.
--
-- 'resourceId', 'event_resourceId' - A unique identifier for an event resource, such as a fleet ID.
newEvent ::
  Event
newEvent =
  Event'
    { eventCode = Prelude.Nothing,
      eventId = Prelude.Nothing,
      eventTime = Prelude.Nothing,
      message = Prelude.Nothing,
      preSignedLogUrl = Prelude.Nothing,
      resourceId = Prelude.Nothing
    }

-- | The type of event being logged.
--
-- __Fleet state transition events:__
--
-- -   FLEET_CREATED -- A fleet resource was successfully created with a
--     status of @NEW@. Event messaging includes the fleet ID.
--
-- -   FLEET_STATE_DOWNLOADING -- Fleet status changed from @NEW@ to
--     @DOWNLOADING@. The compressed build has started downloading to a
--     fleet instance for installation.
--
-- -   FLEET_STATE_VALIDATING -- Fleet status changed from @DOWNLOADING@ to
--     @VALIDATING@. GameLift has successfully downloaded the build and is
--     now validating the build files.
--
-- -   FLEET_STATE_BUILDING -- Fleet status changed from @VALIDATING@ to
--     @BUILDING@. GameLift has successfully verified the build files and
--     is now running the installation scripts.
--
-- -   FLEET_STATE_ACTIVATING -- Fleet status changed from @BUILDING@ to
--     @ACTIVATING@. GameLift is trying to launch an instance and test the
--     connectivity between the build and the GameLift Service via the
--     Server SDK.
--
-- -   FLEET_STATE_ACTIVE -- The fleet\'s status changed from @ACTIVATING@
--     to @ACTIVE@. The fleet is now ready to host game sessions.
--
-- -   FLEET_STATE_ERROR -- The Fleet\'s status changed to @ERROR@.
--     Describe the fleet event message for more details.
--
-- __Fleet creation events (ordered by fleet creation activity):__
--
-- -   FLEET_BINARY_DOWNLOAD_FAILED -- The build failed to download to the
--     fleet instance.
--
-- -   FLEET_CREATION_EXTRACTING_BUILD -- The game server build was
--     successfully downloaded to an instance, and the build files are now
--     being extracted from the uploaded build and saved to an instance.
--     Failure at this stage prevents a fleet from moving to ACTIVE status.
--     Logs for this stage display a list of the files that are extracted
--     and saved on the instance. Access the logs by using the URL in
--     /PreSignedLogUrl/.
--
-- -   FLEET_CREATION_RUNNING_INSTALLER -- The game server build files were
--     successfully extracted, and the GameLift is now running the build\'s
--     install script (if one is included). Failure in this stage prevents
--     a fleet from moving to ACTIVE status. Logs for this stage list the
--     installation steps and whether or not the install completed
--     successfully. Access the logs by using the URL in /PreSignedLogUrl/.
--
-- -   FLEET_CREATION_VALIDATING_RUNTIME_CONFIG -- The build process was
--     successful, and the GameLift is now verifying that the game server
--     launch paths, which are specified in the fleet\'s runtime
--     configuration, exist. If any listed launch path exists, GameLift
--     tries to launch a game server process and waits for the process to
--     report ready. Failures in this stage prevent a fleet from moving to
--     @ACTIVE@ status. Logs for this stage list the launch paths in the
--     runtime configuration and indicate whether each is found. Access the
--     logs by using the URL in /PreSignedLogUrl/.
--
-- -   FLEET_VALIDATION_LAUNCH_PATH_NOT_FOUND -- Validation of the runtime
--     configuration failed because the executable specified in a launch
--     path does not exist on the instance.
--
-- -   FLEET_VALIDATION_EXECUTABLE_RUNTIME_FAILURE -- Validation of the
--     runtime configuration failed because the executable specified in a
--     launch path failed to run on the fleet instance.
--
-- -   FLEET_VALIDATION_TIMED_OUT -- Validation of the fleet at the end of
--     creation timed out. Try fleet creation again.
--
-- -   FLEET_ACTIVATION_FAILED -- The fleet failed to successfully complete
--     one of the steps in the fleet activation process. This event code
--     indicates that the game build was successfully downloaded to a fleet
--     instance, built, and validated, but was not able to start a server
--     process. For more information, see
--     <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html#fleets-creating-debug-creation Debug Fleet Creation Issues>.
--
-- -   FLEET_ACTIVATION_FAILED_NO_INSTANCES -- Fleet creation was not able
--     to obtain any instances based on the input fleet attributes. Try
--     again at a different time or choose a different combination of fleet
--     attributes such as fleet type, instance type, etc.
--
-- -   FLEET_INITIALIZATION_FAILED -- A generic exception occurred during
--     fleet creation. Describe the fleet event message for more details.
--
-- __VPC peering events:__
--
-- -   FLEET_VPC_PEERING_SUCCEEDED -- A VPC peering connection has been
--     established between the VPC for an GameLift fleet and a VPC in your
--     Amazon Web Services account.
--
-- -   FLEET_VPC_PEERING_FAILED -- A requested VPC peering connection has
--     failed. Event details and status information provide additional
--     detail. A common reason for peering failure is that the two VPCs
--     have overlapping CIDR blocks of IPv4 addresses. To resolve this,
--     change the CIDR block for the VPC in your Amazon Web Services
--     account. For more information on VPC peering failures, see
--     <https://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html>
--
-- -   FLEET_VPC_PEERING_DELETED -- A VPC peering connection has been
--     successfully deleted.
--
-- __Spot instance events:__
--
-- -   INSTANCE_INTERRUPTED -- A spot instance was interrupted by EC2 with
--     a two-minute notification.
--
-- __Server process events:__
--
-- -   SERVER_PROCESS_INVALID_PATH -- The game server executable or script
--     could not be found based on the Fleet runtime configuration. Check
--     that the launch path is correct based on the operating system of the
--     Fleet.
--
-- -   SERVER_PROCESS_SDK_INITIALIZATION_TIMEOUT -- The server process did
--     not call InitSDK() within the time expected. Check your game session
--     log to see why InitSDK() was not called in time.
--
-- -   SERVER_PROCESS_PROCESS_READY_TIMEOUT -- The server process did not
--     call ProcessReady() within the time expected after calling
--     InitSDK(). Check your game session log to see why ProcessReady() was
--     not called in time.
--
-- -   SERVER_PROCESS_CRASHED -- The server process exited without calling
--     ProcessEnding(). Check your game session log to see why
--     ProcessEnding() was not called.
--
-- -   SERVER_PROCESS_TERMINATED_UNHEALTHY -- The server process did not
--     report a valid health check for too long and was therefore
--     terminated by GameLift. Check your game session log to see if the
--     thread became stuck processing a synchronous task for too long.
--
-- -   SERVER_PROCESS_FORCE_TERMINATED -- The server process did not exit
--     cleanly after OnProcessTerminate() was sent within the time
--     expected. Check your game session log to see why termination took
--     longer than expected.
--
-- -   SERVER_PROCESS_PROCESS_EXIT_TIMEOUT -- The server process did not
--     exit cleanly within the time expected after calling ProcessEnding().
--     Check your game session log to see why termination took longer than
--     expected.
--
-- __Game session events:__
--
-- -   GAME_SESSION_ACTIVATION_TIMEOUT -- GameSession failed to activate
--     within the expected time. Check your game session log to see why
--     ActivateGameSession() took longer to complete than expected.
--
-- __Other fleet events:__
--
-- -   FLEET_SCALING_EVENT -- A change was made to the fleet\'s capacity
--     settings (desired instances, minimum\/maximum scaling limits). Event
--     messaging includes the new capacity settings.
--
-- -   FLEET_NEW_GAME_SESSION_PROTECTION_POLICY_UPDATED -- A change was
--     made to the fleet\'s game session protection policy setting. Event
--     messaging includes both the old and new policy setting.
--
-- -   FLEET_DELETED -- A request to delete a fleet was initiated.
--
-- -   GENERIC_EVENT -- An unspecified event has occurred.
event_eventCode :: Lens.Lens' Event (Prelude.Maybe EventCode)
event_eventCode = Lens.lens (\Event' {eventCode} -> eventCode) (\s@Event' {} a -> s {eventCode = a} :: Event)

-- | A unique identifier for a fleet event.
event_eventId :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventId = Lens.lens (\Event' {eventId} -> eventId) (\s@Event' {} a -> s {eventId = a} :: Event)

-- | Time stamp indicating when this event occurred. Format is a number
-- expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
event_eventTime :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_eventTime = Lens.lens (\Event' {eventTime} -> eventTime) (\s@Event' {} a -> s {eventTime = a} :: Event) Prelude.. Lens.mapping Data._Time

-- | Additional information related to the event.
event_message :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_message = Lens.lens (\Event' {message} -> message) (\s@Event' {} a -> s {message = a} :: Event)

-- | Location of stored logs with additional detail that is related to the
-- event. This is useful for debugging issues. The URL is valid for 15
-- minutes. You can also access fleet creation logs through the GameLift
-- console.
event_preSignedLogUrl :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_preSignedLogUrl = Lens.lens (\Event' {preSignedLogUrl} -> preSignedLogUrl) (\s@Event' {} a -> s {preSignedLogUrl = a} :: Event)

-- | A unique identifier for an event resource, such as a fleet ID.
event_resourceId :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_resourceId = Lens.lens (\Event' {resourceId} -> resourceId) (\s@Event' {} a -> s {resourceId = a} :: Event)

instance Data.FromJSON Event where
  parseJSON =
    Data.withObject
      "Event"
      ( \x ->
          Event'
            Prelude.<$> (x Data..:? "EventCode")
            Prelude.<*> (x Data..:? "EventId")
            Prelude.<*> (x Data..:? "EventTime")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "PreSignedLogUrl")
            Prelude.<*> (x Data..:? "ResourceId")
      )

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt
      `Prelude.hashWithSalt` eventCode
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` preSignedLogUrl
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf eventCode
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf eventTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf preSignedLogUrl
      `Prelude.seq` Prelude.rnf resourceId
