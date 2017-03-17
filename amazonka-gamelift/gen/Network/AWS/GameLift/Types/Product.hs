{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.Product where

import           Network.AWS.GameLift.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | AWS access credentials sometimes used for uploading game build files to Amazon GameLift. They are valid for a limited time. If they expire before you upload your game build, get a new set by calling 'RequestUploadCredentials' .
--
--
--
-- /See:/ 'awsCredentials' smart constructor.
data AWSCredentials = AWSCredentials'
    { _acSecretAccessKey :: !(Maybe Text)
    , _acSessionToken    :: !(Maybe Text)
    , _acAccessKeyId     :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AWSCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acSecretAccessKey' - Secret key for an AWS account.
--
-- * 'acSessionToken' - Token specific to a build ID.
--
-- * 'acAccessKeyId' - Access key for an AWS account.
awsCredentials
    :: AWSCredentials
awsCredentials =
    AWSCredentials'
    { _acSecretAccessKey = Nothing
    , _acSessionToken = Nothing
    , _acAccessKeyId = Nothing
    }

-- | Secret key for an AWS account.
acSecretAccessKey :: Lens' AWSCredentials (Maybe Text)
acSecretAccessKey = lens _acSecretAccessKey (\ s a -> s{_acSecretAccessKey = a});

-- | Token specific to a build ID.
acSessionToken :: Lens' AWSCredentials (Maybe Text)
acSessionToken = lens _acSessionToken (\ s a -> s{_acSessionToken = a});

-- | Access key for an AWS account.
acAccessKeyId :: Lens' AWSCredentials (Maybe Text)
acAccessKeyId = lens _acAccessKeyId (\ s a -> s{_acAccessKeyId = a});

instance FromJSON AWSCredentials where
        parseJSON
          = withObject "AWSCredentials"
              (\ x ->
                 AWSCredentials' <$>
                   (x .:? "SecretAccessKey") <*> (x .:? "SessionToken")
                     <*> (x .:? "AccessKeyId"))

instance Hashable AWSCredentials

instance NFData AWSCredentials

-- | Properties describing a fleet alias.
--
--
-- Alias-related operations include:
--
--     * 'CreateAlias'
--
--     * 'ListAliases'
--
--     * 'DescribeAlias'
--
--     * 'UpdateAlias'
--
--     * 'DeleteAlias'
--
--
--
--
-- /See:/ 'alias' smart constructor.
data Alias = Alias'
    { _aCreationTime    :: !(Maybe POSIX)
    , _aLastUpdatedTime :: !(Maybe POSIX)
    , _aAliasId         :: !(Maybe Text)
    , _aRoutingStrategy :: !(Maybe RoutingStrategy)
    , _aName            :: !(Maybe Text)
    , _aAliasARN        :: !(Maybe Text)
    , _aDescription     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Alias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'aLastUpdatedTime' - Time stamp indicating when this data object was last modified. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'aAliasId' - Unique identifier for an alias; alias IDs are unique within a region.
--
-- * 'aRoutingStrategy' - Alias configuration for the alias, including routing type and settings.
--
-- * 'aName' - Descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- * 'aAliasARN' - Unique identifier for an alias; alias ARNs are unique across all regions.
--
-- * 'aDescription' - Human-readable description of an alias.
alias
    :: Alias
alias =
    Alias'
    { _aCreationTime = Nothing
    , _aLastUpdatedTime = Nothing
    , _aAliasId = Nothing
    , _aRoutingStrategy = Nothing
    , _aName = Nothing
    , _aAliasARN = Nothing
    , _aDescription = Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
aCreationTime :: Lens' Alias (Maybe UTCTime)
aCreationTime = lens _aCreationTime (\ s a -> s{_aCreationTime = a}) . mapping _Time;

-- | Time stamp indicating when this data object was last modified. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
aLastUpdatedTime :: Lens' Alias (Maybe UTCTime)
aLastUpdatedTime = lens _aLastUpdatedTime (\ s a -> s{_aLastUpdatedTime = a}) . mapping _Time;

-- | Unique identifier for an alias; alias IDs are unique within a region.
aAliasId :: Lens' Alias (Maybe Text)
aAliasId = lens _aAliasId (\ s a -> s{_aAliasId = a});

-- | Alias configuration for the alias, including routing type and settings.
aRoutingStrategy :: Lens' Alias (Maybe RoutingStrategy)
aRoutingStrategy = lens _aRoutingStrategy (\ s a -> s{_aRoutingStrategy = a});

-- | Descriptive label that is associated with an alias. Alias names do not need to be unique.
aName :: Lens' Alias (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

-- | Unique identifier for an alias; alias ARNs are unique across all regions.
aAliasARN :: Lens' Alias (Maybe Text)
aAliasARN = lens _aAliasARN (\ s a -> s{_aAliasARN = a});

-- | Human-readable description of an alias.
aDescription :: Lens' Alias (Maybe Text)
aDescription = lens _aDescription (\ s a -> s{_aDescription = a});

instance FromJSON Alias where
        parseJSON
          = withObject "Alias"
              (\ x ->
                 Alias' <$>
                   (x .:? "CreationTime") <*> (x .:? "LastUpdatedTime")
                     <*> (x .:? "AliasId")
                     <*> (x .:? "RoutingStrategy")
                     <*> (x .:? "Name")
                     <*> (x .:? "AliasArn")
                     <*> (x .:? "Description"))

instance Hashable Alias

instance NFData Alias

-- | Properties describing a game build.
--
--
-- Build-related operations include:
--
--     * 'CreateBuild'
--
--     * 'ListBuilds'
--
--     * 'DescribeBuild'
--
--     * 'UpdateBuild'
--
--     * 'DeleteBuild'
--
--
--
--
-- /See:/ 'build' smart constructor.
data Build = Build'
    { _bCreationTime    :: !(Maybe POSIX)
    , _bStatus          :: !(Maybe BuildStatus)
    , _bOperatingSystem :: !(Maybe OperatingSystem)
    , _bBuildId         :: !(Maybe Text)
    , _bName            :: !(Maybe Text)
    , _bVersion         :: !(Maybe Text)
    , _bSizeOnDisk      :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Build' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'bStatus' - Current status of the build. Possible build statuses include the following:     * __INITIALIZED__ – A new build has been defined, but no files have been uploaded. You cannot create fleets for builds that are in this status. When a build is successfully created, the build status is set to this value.      * __READY__ – The game build has been successfully uploaded. You can now create new fleets for this build.     * __FAILED__ – The game build upload failed. You cannot create new fleets for this build.
--
-- * 'bOperatingSystem' - Operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build.
--
-- * 'bBuildId' - Unique identifier for a build.
--
-- * 'bName' - Descriptive label that is associated with a build. Build names do not need to be unique. It can be set using 'CreateBuild' or 'UpdateBuild' .
--
-- * 'bVersion' - Version that is associated with this build. Version strings do not need to be unique. This value can be set using 'CreateBuild' or 'UpdateBuild' .
--
-- * 'bSizeOnDisk' - File size of the uploaded game build, expressed in bytes. When the build status is @INITIALIZED@ , this value is 0.
build
    :: Build
build =
    Build'
    { _bCreationTime = Nothing
    , _bStatus = Nothing
    , _bOperatingSystem = Nothing
    , _bBuildId = Nothing
    , _bName = Nothing
    , _bVersion = Nothing
    , _bSizeOnDisk = Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
bCreationTime :: Lens' Build (Maybe UTCTime)
bCreationTime = lens _bCreationTime (\ s a -> s{_bCreationTime = a}) . mapping _Time;

-- | Current status of the build. Possible build statuses include the following:     * __INITIALIZED__ – A new build has been defined, but no files have been uploaded. You cannot create fleets for builds that are in this status. When a build is successfully created, the build status is set to this value.      * __READY__ – The game build has been successfully uploaded. You can now create new fleets for this build.     * __FAILED__ – The game build upload failed. You cannot create new fleets for this build.
bStatus :: Lens' Build (Maybe BuildStatus)
bStatus = lens _bStatus (\ s a -> s{_bStatus = a});

-- | Operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build.
bOperatingSystem :: Lens' Build (Maybe OperatingSystem)
bOperatingSystem = lens _bOperatingSystem (\ s a -> s{_bOperatingSystem = a});

-- | Unique identifier for a build.
bBuildId :: Lens' Build (Maybe Text)
bBuildId = lens _bBuildId (\ s a -> s{_bBuildId = a});

-- | Descriptive label that is associated with a build. Build names do not need to be unique. It can be set using 'CreateBuild' or 'UpdateBuild' .
bName :: Lens' Build (Maybe Text)
bName = lens _bName (\ s a -> s{_bName = a});

-- | Version that is associated with this build. Version strings do not need to be unique. This value can be set using 'CreateBuild' or 'UpdateBuild' .
bVersion :: Lens' Build (Maybe Text)
bVersion = lens _bVersion (\ s a -> s{_bVersion = a});

-- | File size of the uploaded game build, expressed in bytes. When the build status is @INITIALIZED@ , this value is 0.
bSizeOnDisk :: Lens' Build (Maybe Natural)
bSizeOnDisk = lens _bSizeOnDisk (\ s a -> s{_bSizeOnDisk = a}) . mapping _Nat;

instance FromJSON Build where
        parseJSON
          = withObject "Build"
              (\ x ->
                 Build' <$>
                   (x .:? "CreationTime") <*> (x .:? "Status") <*>
                     (x .:? "OperatingSystem")
                     <*> (x .:? "BuildId")
                     <*> (x .:? "Name")
                     <*> (x .:? "Version")
                     <*> (x .:? "SizeOnDisk"))

instance Hashable Build

instance NFData Build

-- | Player information for use when creating player sessions using a game session placement request with 'StartGameSessionPlacement' .
--
--
--
-- /See:/ 'desiredPlayerSession' smart constructor.
data DesiredPlayerSession = DesiredPlayerSession'
    { _dpsPlayerData :: !(Maybe Text)
    , _dpsPlayerId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DesiredPlayerSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpsPlayerData' - Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
--
-- * 'dpsPlayerId' - Unique identifier for a player to associate with the player session.
desiredPlayerSession
    :: DesiredPlayerSession
desiredPlayerSession =
    DesiredPlayerSession'
    { _dpsPlayerData = Nothing
    , _dpsPlayerId = Nothing
    }

-- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
dpsPlayerData :: Lens' DesiredPlayerSession (Maybe Text)
dpsPlayerData = lens _dpsPlayerData (\ s a -> s{_dpsPlayerData = a});

-- | Unique identifier for a player to associate with the player session.
dpsPlayerId :: Lens' DesiredPlayerSession (Maybe Text)
dpsPlayerId = lens _dpsPlayerId (\ s a -> s{_dpsPlayerId = a});

instance Hashable DesiredPlayerSession

instance NFData DesiredPlayerSession

instance ToJSON DesiredPlayerSession where
        toJSON DesiredPlayerSession'{..}
          = object
              (catMaybes
                 [("PlayerData" .=) <$> _dpsPlayerData,
                  ("PlayerId" .=) <$> _dpsPlayerId])

-- | Current status of fleet capacity. The number of active instances should match or be in the process of matching the number of desired instances. Pending and terminating counts are non-zero only if fleet capacity is adjusting to an 'UpdateFleetCapacity' request, or if access to resources is temporarily affected.
--
--
--
-- /See:/ 'ec2InstanceCounts' smart constructor.
data EC2InstanceCounts = EC2InstanceCounts'
    { _eicIdLE        :: !(Maybe Nat)
    , _eicTERMINATING :: !(Maybe Nat)
    , _eicPENDING     :: !(Maybe Nat)
    , _eicMAXIMUM     :: !(Maybe Nat)
    , _eicDESIRED     :: !(Maybe Nat)
    , _eicMINIMUM     :: !(Maybe Nat)
    , _eicACTIVE      :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EC2InstanceCounts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eicIdLE' - Number of active instances in the fleet that are not currently hosting a game session.
--
-- * 'eicTERMINATING' - Number of instances in the fleet that are no longer active but haven't yet been terminated.
--
-- * 'eicPENDING' - Number of instances in the fleet that are starting but not yet active.
--
-- * 'eicMAXIMUM' - Maximum value allowed for the fleet's instance count.
--
-- * 'eicDESIRED' - Ideal number of active instances in the fleet.
--
-- * 'eicMINIMUM' - Minimum value allowed for the fleet's instance count.
--
-- * 'eicACTIVE' - Actual number of active instances in the fleet.
ec2InstanceCounts
    :: EC2InstanceCounts
ec2InstanceCounts =
    EC2InstanceCounts'
    { _eicIdLE = Nothing
    , _eicTERMINATING = Nothing
    , _eicPENDING = Nothing
    , _eicMAXIMUM = Nothing
    , _eicDESIRED = Nothing
    , _eicMINIMUM = Nothing
    , _eicACTIVE = Nothing
    }

-- | Number of active instances in the fleet that are not currently hosting a game session.
eicIdLE :: Lens' EC2InstanceCounts (Maybe Natural)
eicIdLE = lens _eicIdLE (\ s a -> s{_eicIdLE = a}) . mapping _Nat;

-- | Number of instances in the fleet that are no longer active but haven't yet been terminated.
eicTERMINATING :: Lens' EC2InstanceCounts (Maybe Natural)
eicTERMINATING = lens _eicTERMINATING (\ s a -> s{_eicTERMINATING = a}) . mapping _Nat;

-- | Number of instances in the fleet that are starting but not yet active.
eicPENDING :: Lens' EC2InstanceCounts (Maybe Natural)
eicPENDING = lens _eicPENDING (\ s a -> s{_eicPENDING = a}) . mapping _Nat;

-- | Maximum value allowed for the fleet's instance count.
eicMAXIMUM :: Lens' EC2InstanceCounts (Maybe Natural)
eicMAXIMUM = lens _eicMAXIMUM (\ s a -> s{_eicMAXIMUM = a}) . mapping _Nat;

-- | Ideal number of active instances in the fleet.
eicDESIRED :: Lens' EC2InstanceCounts (Maybe Natural)
eicDESIRED = lens _eicDESIRED (\ s a -> s{_eicDESIRED = a}) . mapping _Nat;

-- | Minimum value allowed for the fleet's instance count.
eicMINIMUM :: Lens' EC2InstanceCounts (Maybe Natural)
eicMINIMUM = lens _eicMINIMUM (\ s a -> s{_eicMINIMUM = a}) . mapping _Nat;

-- | Actual number of active instances in the fleet.
eicACTIVE :: Lens' EC2InstanceCounts (Maybe Natural)
eicACTIVE = lens _eicACTIVE (\ s a -> s{_eicACTIVE = a}) . mapping _Nat;

instance FromJSON EC2InstanceCounts where
        parseJSON
          = withObject "EC2InstanceCounts"
              (\ x ->
                 EC2InstanceCounts' <$>
                   (x .:? "IDLE") <*> (x .:? "TERMINATING") <*>
                     (x .:? "PENDING")
                     <*> (x .:? "MAXIMUM")
                     <*> (x .:? "DESIRED")
                     <*> (x .:? "MINIMUM")
                     <*> (x .:? "ACTIVE"))

instance Hashable EC2InstanceCounts

instance NFData EC2InstanceCounts

-- | Maximum number of instances allowed based on the Amazon Elastic Compute Cloud (Amazon EC2) instance type. Instance limits can be retrieved by calling 'DescribeEC2InstanceLimits' .
--
--
--
-- /See:/ 'ec2InstanceLimit' smart constructor.
data EC2InstanceLimit = EC2InstanceLimit'
    { _eilEC2InstanceType  :: !(Maybe EC2InstanceType)
    , _eilCurrentInstances :: !(Maybe Nat)
    , _eilInstanceLimit    :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EC2InstanceLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eilEC2InstanceType' - Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- * 'eilCurrentInstances' - Number of instances of the specified type that are currently in use by this AWS account.
--
-- * 'eilInstanceLimit' - Number of instances allowed.
ec2InstanceLimit
    :: EC2InstanceLimit
ec2InstanceLimit =
    EC2InstanceLimit'
    { _eilEC2InstanceType = Nothing
    , _eilCurrentInstances = Nothing
    , _eilInstanceLimit = Nothing
    }

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
eilEC2InstanceType :: Lens' EC2InstanceLimit (Maybe EC2InstanceType)
eilEC2InstanceType = lens _eilEC2InstanceType (\ s a -> s{_eilEC2InstanceType = a});

-- | Number of instances of the specified type that are currently in use by this AWS account.
eilCurrentInstances :: Lens' EC2InstanceLimit (Maybe Natural)
eilCurrentInstances = lens _eilCurrentInstances (\ s a -> s{_eilCurrentInstances = a}) . mapping _Nat;

-- | Number of instances allowed.
eilInstanceLimit :: Lens' EC2InstanceLimit (Maybe Natural)
eilInstanceLimit = lens _eilInstanceLimit (\ s a -> s{_eilInstanceLimit = a}) . mapping _Nat;

instance FromJSON EC2InstanceLimit where
        parseJSON
          = withObject "EC2InstanceLimit"
              (\ x ->
                 EC2InstanceLimit' <$>
                   (x .:? "EC2InstanceType") <*>
                     (x .:? "CurrentInstances")
                     <*> (x .:? "InstanceLimit"))

instance Hashable EC2InstanceLimit

instance NFData EC2InstanceLimit

-- | Log entry describing an event involving Amazon GameLift resources (such as a fleet). In addition to tracking activity, event codes and messages can provide additional information for troubleshooting and debugging problems.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
    { _eResourceId :: !(Maybe Text)
    , _eEventTime  :: !(Maybe POSIX)
    , _eMessage    :: !(Maybe Text)
    , _eEventCode  :: !(Maybe EventCode)
    , _eEventId    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eResourceId' - Unique identifier for an event resource, such as a fleet ID.
--
-- * 'eEventTime' - Time stamp indicating when this event occurred. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'eMessage' - Additional information related to the event.
--
-- * 'eEventCode' - Type of event being logged.
--
-- * 'eEventId' - Unique identifier for a fleet event.
event
    :: Event
event =
    Event'
    { _eResourceId = Nothing
    , _eEventTime = Nothing
    , _eMessage = Nothing
    , _eEventCode = Nothing
    , _eEventId = Nothing
    }

-- | Unique identifier for an event resource, such as a fleet ID.
eResourceId :: Lens' Event (Maybe Text)
eResourceId = lens _eResourceId (\ s a -> s{_eResourceId = a});

-- | Time stamp indicating when this event occurred. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
eEventTime :: Lens' Event (Maybe UTCTime)
eEventTime = lens _eEventTime (\ s a -> s{_eEventTime = a}) . mapping _Time;

-- | Additional information related to the event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\ s a -> s{_eMessage = a});

-- | Type of event being logged.
eEventCode :: Lens' Event (Maybe EventCode)
eEventCode = lens _eEventCode (\ s a -> s{_eEventCode = a});

-- | Unique identifier for a fleet event.
eEventId :: Lens' Event (Maybe Text)
eEventId = lens _eEventId (\ s a -> s{_eEventId = a});

instance FromJSON Event where
        parseJSON
          = withObject "Event"
              (\ x ->
                 Event' <$>
                   (x .:? "ResourceId") <*> (x .:? "EventTime") <*>
                     (x .:? "Message")
                     <*> (x .:? "EventCode")
                     <*> (x .:? "EventId"))

instance Hashable Event

instance NFData Event

-- | General properties describing a fleet.
--
--
--
-- /See:/ 'fleetAttributes' smart constructor.
data FleetAttributes = FleetAttributes'
    { _faCreationTime                   :: !(Maybe POSIX)
    , _faStatus                         :: !(Maybe FleetStatus)
    , _faServerLaunchParameters         :: !(Maybe Text)
    , _faLogPaths                       :: !(Maybe [Text])
    , _faOperatingSystem                :: !(Maybe OperatingSystem)
    , _faBuildId                        :: !(Maybe Text)
    , _faFleetARN                       :: !(Maybe Text)
    , _faTerminationTime                :: !(Maybe POSIX)
    , _faNewGameSessionProtectionPolicy :: !(Maybe ProtectionPolicy)
    , _faName                           :: !(Maybe Text)
    , _faServerLaunchPath               :: !(Maybe Text)
    , _faFleetId                        :: !(Maybe Text)
    , _faDescription                    :: !(Maybe Text)
    , _faResourceCreationLimitPolicy    :: !(Maybe ResourceCreationLimitPolicy)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FleetAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'faStatus' - Current status of the fleet. Possible fleet statuses include the following:     * __NEW__ – A new fleet has been defined and desired instances is set to 1.      * __DOWNLOADING/VALIDATING/BUILDING/ACTIVATING__ – Amazon GameLift is setting up the new fleet, creating new instances with the game build and starting server processes.     * __ACTIVE__ – Hosts can now accept game sessions.     * __ERROR__ – An error occurred when downloading, validating, building, or activating the fleet.     * __DELETING__ – Hosts are responding to a delete fleet request.     * __TERMINATED__ – The fleet no longer exists.
--
-- * 'faServerLaunchParameters' - Game server launch parameters specified for fleets created prior to 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
--
-- * 'faLogPaths' - Location of default log files. When a server process is shut down, Amazon GameLift captures and stores any log files in this location. These logs are in addition to game session logs; see more on game session logs in the <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide> . If no default log path for a fleet is specified, Amazon GameLift will automatically upload logs that are stored on each instance at @C:\game\logs@ (for Windows) or @/local/game/logs@ (for Linux). Use the Amazon GameLift console to access stored logs.
--
-- * 'faOperatingSystem' - Operating system of the fleet's computing resources. A fleet's operating system depends on the OS specified for the build that is deployed on this fleet.
--
-- * 'faBuildId' - Unique identifier for a build.
--
-- * 'faFleetARN' - Identifier for a fleet that is unique across all regions.
--
-- * 'faTerminationTime' - Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'faNewGameSessionProtectionPolicy' - Type of game session protection to set for all new instances started in the fleet.     * __NoProtection__ – The game session can be terminated during a scale-down event.     * __FullProtection__ – If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
-- * 'faName' - Descriptive label that is associated with a fleet. Fleet names do not need to be unique.
--
-- * 'faServerLaunchPath' - Path to a game server executable in the fleet's build, specified for fleets created prior to 2016-08-04 (or AWS SDK v. 0.12.16). Server launch paths for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
--
-- * 'faFleetId' - Unique identifier for a fleet.
--
-- * 'faDescription' - Human-readable description of the fleet.
--
-- * 'faResourceCreationLimitPolicy' - Fleet policy to limit the number of game sessions an individual player can create over a span of time.
fleetAttributes
    :: FleetAttributes
fleetAttributes =
    FleetAttributes'
    { _faCreationTime = Nothing
    , _faStatus = Nothing
    , _faServerLaunchParameters = Nothing
    , _faLogPaths = Nothing
    , _faOperatingSystem = Nothing
    , _faBuildId = Nothing
    , _faFleetARN = Nothing
    , _faTerminationTime = Nothing
    , _faNewGameSessionProtectionPolicy = Nothing
    , _faName = Nothing
    , _faServerLaunchPath = Nothing
    , _faFleetId = Nothing
    , _faDescription = Nothing
    , _faResourceCreationLimitPolicy = Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
faCreationTime :: Lens' FleetAttributes (Maybe UTCTime)
faCreationTime = lens _faCreationTime (\ s a -> s{_faCreationTime = a}) . mapping _Time;

-- | Current status of the fleet. Possible fleet statuses include the following:     * __NEW__ – A new fleet has been defined and desired instances is set to 1.      * __DOWNLOADING/VALIDATING/BUILDING/ACTIVATING__ – Amazon GameLift is setting up the new fleet, creating new instances with the game build and starting server processes.     * __ACTIVE__ – Hosts can now accept game sessions.     * __ERROR__ – An error occurred when downloading, validating, building, or activating the fleet.     * __DELETING__ – Hosts are responding to a delete fleet request.     * __TERMINATED__ – The fleet no longer exists.
faStatus :: Lens' FleetAttributes (Maybe FleetStatus)
faStatus = lens _faStatus (\ s a -> s{_faStatus = a});

-- | Game server launch parameters specified for fleets created prior to 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
faServerLaunchParameters :: Lens' FleetAttributes (Maybe Text)
faServerLaunchParameters = lens _faServerLaunchParameters (\ s a -> s{_faServerLaunchParameters = a});

-- | Location of default log files. When a server process is shut down, Amazon GameLift captures and stores any log files in this location. These logs are in addition to game session logs; see more on game session logs in the <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide> . If no default log path for a fleet is specified, Amazon GameLift will automatically upload logs that are stored on each instance at @C:\game\logs@ (for Windows) or @/local/game/logs@ (for Linux). Use the Amazon GameLift console to access stored logs.
faLogPaths :: Lens' FleetAttributes [Text]
faLogPaths = lens _faLogPaths (\ s a -> s{_faLogPaths = a}) . _Default . _Coerce;

-- | Operating system of the fleet's computing resources. A fleet's operating system depends on the OS specified for the build that is deployed on this fleet.
faOperatingSystem :: Lens' FleetAttributes (Maybe OperatingSystem)
faOperatingSystem = lens _faOperatingSystem (\ s a -> s{_faOperatingSystem = a});

-- | Unique identifier for a build.
faBuildId :: Lens' FleetAttributes (Maybe Text)
faBuildId = lens _faBuildId (\ s a -> s{_faBuildId = a});

-- | Identifier for a fleet that is unique across all regions.
faFleetARN :: Lens' FleetAttributes (Maybe Text)
faFleetARN = lens _faFleetARN (\ s a -> s{_faFleetARN = a});

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
faTerminationTime :: Lens' FleetAttributes (Maybe UTCTime)
faTerminationTime = lens _faTerminationTime (\ s a -> s{_faTerminationTime = a}) . mapping _Time;

-- | Type of game session protection to set for all new instances started in the fleet.     * __NoProtection__ – The game session can be terminated during a scale-down event.     * __FullProtection__ – If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
faNewGameSessionProtectionPolicy :: Lens' FleetAttributes (Maybe ProtectionPolicy)
faNewGameSessionProtectionPolicy = lens _faNewGameSessionProtectionPolicy (\ s a -> s{_faNewGameSessionProtectionPolicy = a});

-- | Descriptive label that is associated with a fleet. Fleet names do not need to be unique.
faName :: Lens' FleetAttributes (Maybe Text)
faName = lens _faName (\ s a -> s{_faName = a});

-- | Path to a game server executable in the fleet's build, specified for fleets created prior to 2016-08-04 (or AWS SDK v. 0.12.16). Server launch paths for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
faServerLaunchPath :: Lens' FleetAttributes (Maybe Text)
faServerLaunchPath = lens _faServerLaunchPath (\ s a -> s{_faServerLaunchPath = a});

-- | Unique identifier for a fleet.
faFleetId :: Lens' FleetAttributes (Maybe Text)
faFleetId = lens _faFleetId (\ s a -> s{_faFleetId = a});

-- | Human-readable description of the fleet.
faDescription :: Lens' FleetAttributes (Maybe Text)
faDescription = lens _faDescription (\ s a -> s{_faDescription = a});

-- | Fleet policy to limit the number of game sessions an individual player can create over a span of time.
faResourceCreationLimitPolicy :: Lens' FleetAttributes (Maybe ResourceCreationLimitPolicy)
faResourceCreationLimitPolicy = lens _faResourceCreationLimitPolicy (\ s a -> s{_faResourceCreationLimitPolicy = a});

instance FromJSON FleetAttributes where
        parseJSON
          = withObject "FleetAttributes"
              (\ x ->
                 FleetAttributes' <$>
                   (x .:? "CreationTime") <*> (x .:? "Status") <*>
                     (x .:? "ServerLaunchParameters")
                     <*> (x .:? "LogPaths" .!= mempty)
                     <*> (x .:? "OperatingSystem")
                     <*> (x .:? "BuildId")
                     <*> (x .:? "FleetArn")
                     <*> (x .:? "TerminationTime")
                     <*> (x .:? "NewGameSessionProtectionPolicy")
                     <*> (x .:? "Name")
                     <*> (x .:? "ServerLaunchPath")
                     <*> (x .:? "FleetId")
                     <*> (x .:? "Description")
                     <*> (x .:? "ResourceCreationLimitPolicy"))

instance Hashable FleetAttributes

instance NFData FleetAttributes

-- | Information about the fleet's capacity. Fleet capacity is measured in EC2 instances. By default, new fleets have a capacity of one instance, but can be updated as needed. The maximum number of instances for a fleet is determined by the fleet's instance type.
--
--
--
-- /See:/ 'fleetCapacity' smart constructor.
data FleetCapacity = FleetCapacity'
    { _fcInstanceType   :: !(Maybe EC2InstanceType)
    , _fcFleetId        :: !(Maybe Text)
    , _fcInstanceCounts :: !(Maybe EC2InstanceCounts)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FleetCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcInstanceType' - Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- * 'fcFleetId' - Unique identifier for a fleet.
--
-- * 'fcInstanceCounts' - Current status of fleet capacity.
fleetCapacity
    :: FleetCapacity
fleetCapacity =
    FleetCapacity'
    { _fcInstanceType = Nothing
    , _fcFleetId = Nothing
    , _fcInstanceCounts = Nothing
    }

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
fcInstanceType :: Lens' FleetCapacity (Maybe EC2InstanceType)
fcInstanceType = lens _fcInstanceType (\ s a -> s{_fcInstanceType = a});

-- | Unique identifier for a fleet.
fcFleetId :: Lens' FleetCapacity (Maybe Text)
fcFleetId = lens _fcFleetId (\ s a -> s{_fcFleetId = a});

-- | Current status of fleet capacity.
fcInstanceCounts :: Lens' FleetCapacity (Maybe EC2InstanceCounts)
fcInstanceCounts = lens _fcInstanceCounts (\ s a -> s{_fcInstanceCounts = a});

instance FromJSON FleetCapacity where
        parseJSON
          = withObject "FleetCapacity"
              (\ x ->
                 FleetCapacity' <$>
                   (x .:? "InstanceType") <*> (x .:? "FleetId") <*>
                     (x .:? "InstanceCounts"))

instance Hashable FleetCapacity

instance NFData FleetCapacity

-- | Current status of fleet utilization, including the number of game and player sessions being hosted.
--
--
--
-- /See:/ 'fleetUtilization' smart constructor.
data FleetUtilization = FleetUtilization'
    { _fuActiveGameSessionCount    :: !(Maybe Nat)
    , _fuMaximumPlayerSessionCount :: !(Maybe Nat)
    , _fuCurrentPlayerSessionCount :: !(Maybe Nat)
    , _fuFleetId                   :: !(Maybe Text)
    , _fuActiveServerProcessCount  :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FleetUtilization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fuActiveGameSessionCount' - Number of active game sessions currently being hosted on all instances in the fleet.
--
-- * 'fuMaximumPlayerSessionCount' - Maximum players allowed across all game sessions currently being hosted on all instances in the fleet.
--
-- * 'fuCurrentPlayerSessionCount' - Number of active player sessions currently being hosted on all instances in the fleet.
--
-- * 'fuFleetId' - Unique identifier for a fleet.
--
-- * 'fuActiveServerProcessCount' - Number of server processes in an @ACTIVE@ status currently running across all instances in the fleet
fleetUtilization
    :: FleetUtilization
fleetUtilization =
    FleetUtilization'
    { _fuActiveGameSessionCount = Nothing
    , _fuMaximumPlayerSessionCount = Nothing
    , _fuCurrentPlayerSessionCount = Nothing
    , _fuFleetId = Nothing
    , _fuActiveServerProcessCount = Nothing
    }

-- | Number of active game sessions currently being hosted on all instances in the fleet.
fuActiveGameSessionCount :: Lens' FleetUtilization (Maybe Natural)
fuActiveGameSessionCount = lens _fuActiveGameSessionCount (\ s a -> s{_fuActiveGameSessionCount = a}) . mapping _Nat;

-- | Maximum players allowed across all game sessions currently being hosted on all instances in the fleet.
fuMaximumPlayerSessionCount :: Lens' FleetUtilization (Maybe Natural)
fuMaximumPlayerSessionCount = lens _fuMaximumPlayerSessionCount (\ s a -> s{_fuMaximumPlayerSessionCount = a}) . mapping _Nat;

-- | Number of active player sessions currently being hosted on all instances in the fleet.
fuCurrentPlayerSessionCount :: Lens' FleetUtilization (Maybe Natural)
fuCurrentPlayerSessionCount = lens _fuCurrentPlayerSessionCount (\ s a -> s{_fuCurrentPlayerSessionCount = a}) . mapping _Nat;

-- | Unique identifier for a fleet.
fuFleetId :: Lens' FleetUtilization (Maybe Text)
fuFleetId = lens _fuFleetId (\ s a -> s{_fuFleetId = a});

-- | Number of server processes in an @ACTIVE@ status currently running across all instances in the fleet
fuActiveServerProcessCount :: Lens' FleetUtilization (Maybe Natural)
fuActiveServerProcessCount = lens _fuActiveServerProcessCount (\ s a -> s{_fuActiveServerProcessCount = a}) . mapping _Nat;

instance FromJSON FleetUtilization where
        parseJSON
          = withObject "FleetUtilization"
              (\ x ->
                 FleetUtilization' <$>
                   (x .:? "ActiveGameSessionCount") <*>
                     (x .:? "MaximumPlayerSessionCount")
                     <*> (x .:? "CurrentPlayerSessionCount")
                     <*> (x .:? "FleetId")
                     <*> (x .:? "ActiveServerProcessCount"))

instance Hashable FleetUtilization

instance NFData FleetUtilization

-- | Set of key-value pairs containing information a server process requires to set up a game session. This object allows you to pass in any set of data needed for your game. For more information, see the <http://docs.aws.amazon.com/gamelift/latest/developerguide/ Amazon GameLift Developer Guide> .
--
--
--
-- /See:/ 'gameProperty' smart constructor.
data GameProperty = GameProperty'
    { _gpKey   :: !Text
    , _gpValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GameProperty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpKey' - TBD
--
-- * 'gpValue' - TBD
gameProperty
    :: Text -- ^ 'gpKey'
    -> Text -- ^ 'gpValue'
    -> GameProperty
gameProperty pKey_ pValue_ =
    GameProperty'
    { _gpKey = pKey_
    , _gpValue = pValue_
    }

-- | TBD
gpKey :: Lens' GameProperty Text
gpKey = lens _gpKey (\ s a -> s{_gpKey = a});

-- | TBD
gpValue :: Lens' GameProperty Text
gpValue = lens _gpValue (\ s a -> s{_gpValue = a});

instance FromJSON GameProperty where
        parseJSON
          = withObject "GameProperty"
              (\ x ->
                 GameProperty' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable GameProperty

instance NFData GameProperty

instance ToJSON GameProperty where
        toJSON GameProperty'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _gpKey), Just ("Value" .= _gpValue)])

-- | Properties describing a game session.
--
--
--
-- /See:/ 'gameSession' smart constructor.
data GameSession = GameSession'
    { _gsCreationTime                :: !(Maybe POSIX)
    , _gsStatus                      :: !(Maybe GameSessionStatus)
    , _gsGameProperties              :: !(Maybe [GameProperty])
    , _gsIPAddress                   :: !(Maybe Text)
    , _gsGameSessionId               :: !(Maybe Text)
    , _gsMaximumPlayerSessionCount   :: !(Maybe Nat)
    , _gsTerminationTime             :: !(Maybe POSIX)
    , _gsPlayerSessionCreationPolicy :: !(Maybe PlayerSessionCreationPolicy)
    , _gsName                        :: !(Maybe Text)
    , _gsCurrentPlayerSessionCount   :: !(Maybe Nat)
    , _gsFleetId                     :: !(Maybe Text)
    , _gsCreatorId                   :: !(Maybe Text)
    , _gsPort                        :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GameSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'gsStatus' - Current status of the game session. A game session must have an @ACTIVE@ status to have player sessions.
--
-- * 'gsGameProperties' - Set of developer-defined properties for a game session. These properties are passed to the server process hosting the game session.
--
-- * 'gsIPAddress' - IP address of the game session. To connect to a Amazon GameLift server process, an app needs both the IP address and port number.
--
-- * 'gsGameSessionId' - Unique identifier for the game session. A game session ID has the following format: "arn:aws:gamelift:<region>::gamesession/<fleet ID>/<game session ID>".
--
-- * 'gsMaximumPlayerSessionCount' - Maximum number of players that can be connected simultaneously to the game session.
--
-- * 'gsTerminationTime' - Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'gsPlayerSessionCreationPolicy' - Indicates whether or not the game session is accepting new players.
--
-- * 'gsName' - Descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- * 'gsCurrentPlayerSessionCount' - Number of players currently in the game session.
--
-- * 'gsFleetId' - Unique identifier for a fleet the game session is running on.
--
-- * 'gsCreatorId' - Unique identifier for a player. This ID is used to enforce a resource protection policy (if one exists), that limits the number of game sessions a player can create.
--
-- * 'gsPort' - Port number for the game session. To connect to a Amazon GameLift server process, an app needs both the IP address and port number.
gameSession
    :: GameSession
gameSession =
    GameSession'
    { _gsCreationTime = Nothing
    , _gsStatus = Nothing
    , _gsGameProperties = Nothing
    , _gsIPAddress = Nothing
    , _gsGameSessionId = Nothing
    , _gsMaximumPlayerSessionCount = Nothing
    , _gsTerminationTime = Nothing
    , _gsPlayerSessionCreationPolicy = Nothing
    , _gsName = Nothing
    , _gsCurrentPlayerSessionCount = Nothing
    , _gsFleetId = Nothing
    , _gsCreatorId = Nothing
    , _gsPort = Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
gsCreationTime :: Lens' GameSession (Maybe UTCTime)
gsCreationTime = lens _gsCreationTime (\ s a -> s{_gsCreationTime = a}) . mapping _Time;

-- | Current status of the game session. A game session must have an @ACTIVE@ status to have player sessions.
gsStatus :: Lens' GameSession (Maybe GameSessionStatus)
gsStatus = lens _gsStatus (\ s a -> s{_gsStatus = a});

-- | Set of developer-defined properties for a game session. These properties are passed to the server process hosting the game session.
gsGameProperties :: Lens' GameSession [GameProperty]
gsGameProperties = lens _gsGameProperties (\ s a -> s{_gsGameProperties = a}) . _Default . _Coerce;

-- | IP address of the game session. To connect to a Amazon GameLift server process, an app needs both the IP address and port number.
gsIPAddress :: Lens' GameSession (Maybe Text)
gsIPAddress = lens _gsIPAddress (\ s a -> s{_gsIPAddress = a});

-- | Unique identifier for the game session. A game session ID has the following format: "arn:aws:gamelift:<region>::gamesession/<fleet ID>/<game session ID>".
gsGameSessionId :: Lens' GameSession (Maybe Text)
gsGameSessionId = lens _gsGameSessionId (\ s a -> s{_gsGameSessionId = a});

-- | Maximum number of players that can be connected simultaneously to the game session.
gsMaximumPlayerSessionCount :: Lens' GameSession (Maybe Natural)
gsMaximumPlayerSessionCount = lens _gsMaximumPlayerSessionCount (\ s a -> s{_gsMaximumPlayerSessionCount = a}) . mapping _Nat;

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
gsTerminationTime :: Lens' GameSession (Maybe UTCTime)
gsTerminationTime = lens _gsTerminationTime (\ s a -> s{_gsTerminationTime = a}) . mapping _Time;

-- | Indicates whether or not the game session is accepting new players.
gsPlayerSessionCreationPolicy :: Lens' GameSession (Maybe PlayerSessionCreationPolicy)
gsPlayerSessionCreationPolicy = lens _gsPlayerSessionCreationPolicy (\ s a -> s{_gsPlayerSessionCreationPolicy = a});

-- | Descriptive label that is associated with a game session. Session names do not need to be unique.
gsName :: Lens' GameSession (Maybe Text)
gsName = lens _gsName (\ s a -> s{_gsName = a});

-- | Number of players currently in the game session.
gsCurrentPlayerSessionCount :: Lens' GameSession (Maybe Natural)
gsCurrentPlayerSessionCount = lens _gsCurrentPlayerSessionCount (\ s a -> s{_gsCurrentPlayerSessionCount = a}) . mapping _Nat;

-- | Unique identifier for a fleet the game session is running on.
gsFleetId :: Lens' GameSession (Maybe Text)
gsFleetId = lens _gsFleetId (\ s a -> s{_gsFleetId = a});

-- | Unique identifier for a player. This ID is used to enforce a resource protection policy (if one exists), that limits the number of game sessions a player can create.
gsCreatorId :: Lens' GameSession (Maybe Text)
gsCreatorId = lens _gsCreatorId (\ s a -> s{_gsCreatorId = a});

-- | Port number for the game session. To connect to a Amazon GameLift server process, an app needs both the IP address and port number.
gsPort :: Lens' GameSession (Maybe Natural)
gsPort = lens _gsPort (\ s a -> s{_gsPort = a}) . mapping _Nat;

instance FromJSON GameSession where
        parseJSON
          = withObject "GameSession"
              (\ x ->
                 GameSession' <$>
                   (x .:? "CreationTime") <*> (x .:? "Status") <*>
                     (x .:? "GameProperties" .!= mempty)
                     <*> (x .:? "IpAddress")
                     <*> (x .:? "GameSessionId")
                     <*> (x .:? "MaximumPlayerSessionCount")
                     <*> (x .:? "TerminationTime")
                     <*> (x .:? "PlayerSessionCreationPolicy")
                     <*> (x .:? "Name")
                     <*> (x .:? "CurrentPlayerSessionCount")
                     <*> (x .:? "FleetId")
                     <*> (x .:? "CreatorId")
                     <*> (x .:? "Port"))

instance Hashable GameSession

instance NFData GameSession

-- | A game session's properties plus the protection policy currently in force.
--
--
--
-- /See:/ 'gameSessionDetail' smart constructor.
data GameSessionDetail = GameSessionDetail'
    { _gsdGameSession      :: !(Maybe GameSession)
    , _gsdProtectionPolicy :: !(Maybe ProtectionPolicy)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GameSessionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdGameSession' - Object that describes a game session.
--
-- * 'gsdProtectionPolicy' - Current status of protection for the game session.     * __NoProtection__ – The game session can be terminated during a scale-down event.     * __FullProtection__ – If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
gameSessionDetail
    :: GameSessionDetail
gameSessionDetail =
    GameSessionDetail'
    { _gsdGameSession = Nothing
    , _gsdProtectionPolicy = Nothing
    }

-- | Object that describes a game session.
gsdGameSession :: Lens' GameSessionDetail (Maybe GameSession)
gsdGameSession = lens _gsdGameSession (\ s a -> s{_gsdGameSession = a});

-- | Current status of protection for the game session.     * __NoProtection__ – The game session can be terminated during a scale-down event.     * __FullProtection__ – If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
gsdProtectionPolicy :: Lens' GameSessionDetail (Maybe ProtectionPolicy)
gsdProtectionPolicy = lens _gsdProtectionPolicy (\ s a -> s{_gsdProtectionPolicy = a});

instance FromJSON GameSessionDetail where
        parseJSON
          = withObject "GameSessionDetail"
              (\ x ->
                 GameSessionDetail' <$>
                   (x .:? "GameSession") <*> (x .:? "ProtectionPolicy"))

instance Hashable GameSessionDetail

instance NFData GameSessionDetail

-- | Object that describes a 'StartGameSessionPlacement' request. This object includes the full details of the original request plus the current status and start/end time stamps.
--
--
-- Game session placement-related operations include:
--
--     * 'StartGameSessionPlacement'
--
--     * 'DescribeGameSessionPlacement'
--
--     * 'StopGameSessionPlacement'
--
--
--
--
-- /See:/ 'gameSessionPlacement' smart constructor.
data GameSessionPlacement = GameSessionPlacement'
    { _gspStatus                    :: !(Maybe GameSessionPlacementState)
    , _gspPlacementId               :: !(Maybe Text)
    , _gspGameProperties            :: !(Maybe [GameProperty])
    , _gspGameSessionName           :: !(Maybe Text)
    , _gspStartTime                 :: !(Maybe POSIX)
    , _gspGameSessionRegion         :: !(Maybe Text)
    , _gspMaximumPlayerSessionCount :: !(Maybe Nat)
    , _gspEndTime                   :: !(Maybe POSIX)
    , _gspGameSessionARN            :: !(Maybe Text)
    , _gspPlayerLatencies           :: !(Maybe [PlayerLatency])
    , _gspGameSessionQueueName      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GameSessionPlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspStatus' - Current status of the game session placement request.     * __PENDING__ – The placement request is currently in the queue waiting to be processed.     * __FULFILLED__ – A new game session and player sessions (if requested) have been successfully created. Values for /GameSessionArn/ and /GameSessionRegion/ are available.      * __CANCELLED__ – The placement request was cancelled with a call to 'StopGameSessionPlacement' .     * __TIMED_OUT__ – A new game session was not successfully created before the time limit expired. You can resubmit the placement request as needed.
--
-- * 'gspPlacementId' - Unique identifier for a game session placement.
--
-- * 'gspGameProperties' - Set of developer-defined properties for a game session. These properties are passed to the server process hosting the game session.
--
-- * 'gspGameSessionName' - Descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- * 'gspStartTime' - Time stamp indicating when this request was placed in the queue. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'gspGameSessionRegion' - Name of the region where the game session created by this placement request is running. This value exists only if the game session placement status is Completed.
--
-- * 'gspMaximumPlayerSessionCount' - Maximum number of players that can be connected simultaneously to the game session.
--
-- * 'gspEndTime' - Time stamp indicating when this request was completed, cancelled, or timed out.
--
-- * 'gspGameSessionARN' - Identifier for the game session created by this placement request. This value exists only if the game session placement status is Completed. This identifier is unique across all regions.
--
-- * 'gspPlayerLatencies' - Set of values, expressed in milliseconds, indicating the amount of latency that players experience when connected to AWS regions.
--
-- * 'gspGameSessionQueueName' - Descriptive label that is associated with queue. Queue names must be unique within each region.
gameSessionPlacement
    :: GameSessionPlacement
gameSessionPlacement =
    GameSessionPlacement'
    { _gspStatus = Nothing
    , _gspPlacementId = Nothing
    , _gspGameProperties = Nothing
    , _gspGameSessionName = Nothing
    , _gspStartTime = Nothing
    , _gspGameSessionRegion = Nothing
    , _gspMaximumPlayerSessionCount = Nothing
    , _gspEndTime = Nothing
    , _gspGameSessionARN = Nothing
    , _gspPlayerLatencies = Nothing
    , _gspGameSessionQueueName = Nothing
    }

-- | Current status of the game session placement request.     * __PENDING__ – The placement request is currently in the queue waiting to be processed.     * __FULFILLED__ – A new game session and player sessions (if requested) have been successfully created. Values for /GameSessionArn/ and /GameSessionRegion/ are available.      * __CANCELLED__ – The placement request was cancelled with a call to 'StopGameSessionPlacement' .     * __TIMED_OUT__ – A new game session was not successfully created before the time limit expired. You can resubmit the placement request as needed.
gspStatus :: Lens' GameSessionPlacement (Maybe GameSessionPlacementState)
gspStatus = lens _gspStatus (\ s a -> s{_gspStatus = a});

-- | Unique identifier for a game session placement.
gspPlacementId :: Lens' GameSessionPlacement (Maybe Text)
gspPlacementId = lens _gspPlacementId (\ s a -> s{_gspPlacementId = a});

-- | Set of developer-defined properties for a game session. These properties are passed to the server process hosting the game session.
gspGameProperties :: Lens' GameSessionPlacement [GameProperty]
gspGameProperties = lens _gspGameProperties (\ s a -> s{_gspGameProperties = a}) . _Default . _Coerce;

-- | Descriptive label that is associated with a game session. Session names do not need to be unique.
gspGameSessionName :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionName = lens _gspGameSessionName (\ s a -> s{_gspGameSessionName = a});

-- | Time stamp indicating when this request was placed in the queue. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
gspStartTime :: Lens' GameSessionPlacement (Maybe UTCTime)
gspStartTime = lens _gspStartTime (\ s a -> s{_gspStartTime = a}) . mapping _Time;

-- | Name of the region where the game session created by this placement request is running. This value exists only if the game session placement status is Completed.
gspGameSessionRegion :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionRegion = lens _gspGameSessionRegion (\ s a -> s{_gspGameSessionRegion = a});

-- | Maximum number of players that can be connected simultaneously to the game session.
gspMaximumPlayerSessionCount :: Lens' GameSessionPlacement (Maybe Natural)
gspMaximumPlayerSessionCount = lens _gspMaximumPlayerSessionCount (\ s a -> s{_gspMaximumPlayerSessionCount = a}) . mapping _Nat;

-- | Time stamp indicating when this request was completed, cancelled, or timed out.
gspEndTime :: Lens' GameSessionPlacement (Maybe UTCTime)
gspEndTime = lens _gspEndTime (\ s a -> s{_gspEndTime = a}) . mapping _Time;

-- | Identifier for the game session created by this placement request. This value exists only if the game session placement status is Completed. This identifier is unique across all regions.
gspGameSessionARN :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionARN = lens _gspGameSessionARN (\ s a -> s{_gspGameSessionARN = a});

-- | Set of values, expressed in milliseconds, indicating the amount of latency that players experience when connected to AWS regions.
gspPlayerLatencies :: Lens' GameSessionPlacement [PlayerLatency]
gspPlayerLatencies = lens _gspPlayerLatencies (\ s a -> s{_gspPlayerLatencies = a}) . _Default . _Coerce;

-- | Descriptive label that is associated with queue. Queue names must be unique within each region.
gspGameSessionQueueName :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionQueueName = lens _gspGameSessionQueueName (\ s a -> s{_gspGameSessionQueueName = a});

instance FromJSON GameSessionPlacement where
        parseJSON
          = withObject "GameSessionPlacement"
              (\ x ->
                 GameSessionPlacement' <$>
                   (x .:? "Status") <*> (x .:? "PlacementId") <*>
                     (x .:? "GameProperties" .!= mempty)
                     <*> (x .:? "GameSessionName")
                     <*> (x .:? "StartTime")
                     <*> (x .:? "GameSessionRegion")
                     <*> (x .:? "MaximumPlayerSessionCount")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "GameSessionArn")
                     <*> (x .:? "PlayerLatencies" .!= mempty)
                     <*> (x .:? "GameSessionQueueName"))

instance Hashable GameSessionPlacement

instance NFData GameSessionPlacement

-- | Configuration of a queue used to process game session placement requests. The queue configuration identifies the fleets that new game session can be placed on, given available resources, and the length of time a request can remain in the queue waiting for placement.
--
--
-- Queue-related operations include:
--
--     * 'CreateGameSessionQueue'
--
--     * 'DescribeGameSessionQueues'
--
--     * 'UpdateGameSessionQueue'
--
--     * 'DeleteGameSessionQueue'
--
--
--
--
-- /See:/ 'gameSessionQueue' smart constructor.
data GameSessionQueue = GameSessionQueue'
    { _gsqTimeoutInSeconds :: !(Maybe Nat)
    , _gsqDestinations     :: !(Maybe [GameSessionQueueDestination])
    , _gsqName             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GameSessionQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsqTimeoutInSeconds' - Maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a TIMED_OUT status.
--
-- * 'gsqDestinations' - List of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
--
-- * 'gsqName' - Descriptive label that is associated with queue. Queue names must be unique within each region.
gameSessionQueue
    :: GameSessionQueue
gameSessionQueue =
    GameSessionQueue'
    { _gsqTimeoutInSeconds = Nothing
    , _gsqDestinations = Nothing
    , _gsqName = Nothing
    }

-- | Maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a TIMED_OUT status.
gsqTimeoutInSeconds :: Lens' GameSessionQueue (Maybe Natural)
gsqTimeoutInSeconds = lens _gsqTimeoutInSeconds (\ s a -> s{_gsqTimeoutInSeconds = a}) . mapping _Nat;

-- | List of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
gsqDestinations :: Lens' GameSessionQueue [GameSessionQueueDestination]
gsqDestinations = lens _gsqDestinations (\ s a -> s{_gsqDestinations = a}) . _Default . _Coerce;

-- | Descriptive label that is associated with queue. Queue names must be unique within each region.
gsqName :: Lens' GameSessionQueue (Maybe Text)
gsqName = lens _gsqName (\ s a -> s{_gsqName = a});

instance FromJSON GameSessionQueue where
        parseJSON
          = withObject "GameSessionQueue"
              (\ x ->
                 GameSessionQueue' <$>
                   (x .:? "TimeoutInSeconds") <*>
                     (x .:? "Destinations" .!= mempty)
                     <*> (x .:? "Name"))

instance Hashable GameSessionQueue

instance NFData GameSessionQueue

-- | Fleet designated in a game session queue. Requests for new game sessions in the queue are fulfilled by starting a new game session on any destination listed for a queue.
--
--
--
-- /See:/ 'gameSessionQueueDestination' smart constructor.
newtype GameSessionQueueDestination = GameSessionQueueDestination'
    { _gsqdDestinationARN :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GameSessionQueueDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsqdDestinationARN' - Amazon Resource Name (ARN) assigned to fleet or fleet alias. ARNs, which include a fleet ID or alias ID and a region name, provide a unique identifier across all regions.
gameSessionQueueDestination
    :: GameSessionQueueDestination
gameSessionQueueDestination =
    GameSessionQueueDestination'
    { _gsqdDestinationARN = Nothing
    }

-- | Amazon Resource Name (ARN) assigned to fleet or fleet alias. ARNs, which include a fleet ID or alias ID and a region name, provide a unique identifier across all regions.
gsqdDestinationARN :: Lens' GameSessionQueueDestination (Maybe Text)
gsqdDestinationARN = lens _gsqdDestinationARN (\ s a -> s{_gsqdDestinationARN = a});

instance FromJSON GameSessionQueueDestination where
        parseJSON
          = withObject "GameSessionQueueDestination"
              (\ x ->
                 GameSessionQueueDestination' <$>
                   (x .:? "DestinationArn"))

instance Hashable GameSessionQueueDestination

instance NFData GameSessionQueueDestination

instance ToJSON GameSessionQueueDestination where
        toJSON GameSessionQueueDestination'{..}
          = object
              (catMaybes
                 [("DestinationArn" .=) <$> _gsqdDestinationARN])

-- | A range of IP addresses and port settings that allow inbound traffic to connect to server processes on Amazon GameLift. Each game session hosted on a fleet is assigned a unique combination of IP address and port number, which must fall into the fleet's allowed ranges. This combination is included in the 'GameSession' object.
--
--
--
-- /See:/ 'ipPermission' smart constructor.
data IPPermission = IPPermission'
    { _ipFromPort :: !Nat
    , _ipToPort   :: !Nat
    , _ipIPRange  :: !Text
    , _ipProtocol :: !IPProtocol
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IPPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipFromPort' - Starting value for a range of allowed port numbers.
--
-- * 'ipToPort' - Ending value for a range of allowed port numbers. Port numbers are end-inclusive. This value must be higher than @FromPort@ .
--
-- * 'ipIPRange' - Range of allowed IP addresses. This value must be expressed in CIDR notation. Example: "@000.000.000.000/[subnet mask]@ " or optionally the shortened version "@0.0.0.0/[subnet mask]@ ".
--
-- * 'ipProtocol' - Network communication protocol used by the fleet.
ipPermission
    :: Natural -- ^ 'ipFromPort'
    -> Natural -- ^ 'ipToPort'
    -> Text -- ^ 'ipIPRange'
    -> IPProtocol -- ^ 'ipProtocol'
    -> IPPermission
ipPermission pFromPort_ pToPort_ pIPRange_ pProtocol_ =
    IPPermission'
    { _ipFromPort = _Nat # pFromPort_
    , _ipToPort = _Nat # pToPort_
    , _ipIPRange = pIPRange_
    , _ipProtocol = pProtocol_
    }

-- | Starting value for a range of allowed port numbers.
ipFromPort :: Lens' IPPermission Natural
ipFromPort = lens _ipFromPort (\ s a -> s{_ipFromPort = a}) . _Nat;

-- | Ending value for a range of allowed port numbers. Port numbers are end-inclusive. This value must be higher than @FromPort@ .
ipToPort :: Lens' IPPermission Natural
ipToPort = lens _ipToPort (\ s a -> s{_ipToPort = a}) . _Nat;

-- | Range of allowed IP addresses. This value must be expressed in CIDR notation. Example: "@000.000.000.000/[subnet mask]@ " or optionally the shortened version "@0.0.0.0/[subnet mask]@ ".
ipIPRange :: Lens' IPPermission Text
ipIPRange = lens _ipIPRange (\ s a -> s{_ipIPRange = a});

-- | Network communication protocol used by the fleet.
ipProtocol :: Lens' IPPermission IPProtocol
ipProtocol = lens _ipProtocol (\ s a -> s{_ipProtocol = a});

instance FromJSON IPPermission where
        parseJSON
          = withObject "IPPermission"
              (\ x ->
                 IPPermission' <$>
                   (x .: "FromPort") <*> (x .: "ToPort") <*>
                     (x .: "IpRange")
                     <*> (x .: "Protocol"))

instance Hashable IPPermission

instance NFData IPPermission

instance ToJSON IPPermission where
        toJSON IPPermission'{..}
          = object
              (catMaybes
                 [Just ("FromPort" .= _ipFromPort),
                  Just ("ToPort" .= _ipToPort),
                  Just ("IpRange" .= _ipIPRange),
                  Just ("Protocol" .= _ipProtocol)])

-- | Properties that describe an instance of a virtual computing resource that hosts one or more game servers. A fleet contains zero or more instances.
--
--
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
    { _iCreationTime    :: !(Maybe POSIX)
    , _iInstanceId      :: !(Maybe Text)
    , _iStatus          :: !(Maybe InstanceStatus)
    , _iIPAddress       :: !(Maybe Text)
    , _iOperatingSystem :: !(Maybe OperatingSystem)
    , _iType            :: !(Maybe EC2InstanceType)
    , _iFleetId         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'iInstanceId' - Unique identifier for an instance.
--
-- * 'iStatus' - Current status of the instance. Possible statuses include the following:     * __PENDING__ – The instance is in the process of being created and launching server processes as defined in the fleet's runtime configuration.      * __ACTIVE__ – The instance has been successfully created and at least one server process has successfully launched and reported back to Amazon GameLift that it is ready to host a game session. The instance is now considered ready to host game sessions.      * __TERMINATING__ – The instance is in the process of shutting down. This may happen to reduce capacity during a scaling down event or to recycle resources in the event of a problem.
--
-- * 'iIPAddress' - IP address assigned to the instance.
--
-- * 'iOperatingSystem' - Operating system that is running on this instance.
--
-- * 'iType' - EC2 instance type that defines the computing resources of this instance.
--
-- * 'iFleetId' - Unique identifier for a fleet that the instance is in.
instance'
    :: Instance
instance' =
    Instance'
    { _iCreationTime = Nothing
    , _iInstanceId = Nothing
    , _iStatus = Nothing
    , _iIPAddress = Nothing
    , _iOperatingSystem = Nothing
    , _iType = Nothing
    , _iFleetId = Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
iCreationTime :: Lens' Instance (Maybe UTCTime)
iCreationTime = lens _iCreationTime (\ s a -> s{_iCreationTime = a}) . mapping _Time;

-- | Unique identifier for an instance.
iInstanceId :: Lens' Instance (Maybe Text)
iInstanceId = lens _iInstanceId (\ s a -> s{_iInstanceId = a});

-- | Current status of the instance. Possible statuses include the following:     * __PENDING__ – The instance is in the process of being created and launching server processes as defined in the fleet's runtime configuration.      * __ACTIVE__ – The instance has been successfully created and at least one server process has successfully launched and reported back to Amazon GameLift that it is ready to host a game session. The instance is now considered ready to host game sessions.      * __TERMINATING__ – The instance is in the process of shutting down. This may happen to reduce capacity during a scaling down event or to recycle resources in the event of a problem.
iStatus :: Lens' Instance (Maybe InstanceStatus)
iStatus = lens _iStatus (\ s a -> s{_iStatus = a});

-- | IP address assigned to the instance.
iIPAddress :: Lens' Instance (Maybe Text)
iIPAddress = lens _iIPAddress (\ s a -> s{_iIPAddress = a});

-- | Operating system that is running on this instance.
iOperatingSystem :: Lens' Instance (Maybe OperatingSystem)
iOperatingSystem = lens _iOperatingSystem (\ s a -> s{_iOperatingSystem = a});

-- | EC2 instance type that defines the computing resources of this instance.
iType :: Lens' Instance (Maybe EC2InstanceType)
iType = lens _iType (\ s a -> s{_iType = a});

-- | Unique identifier for a fleet that the instance is in.
iFleetId :: Lens' Instance (Maybe Text)
iFleetId = lens _iFleetId (\ s a -> s{_iFleetId = a});

instance FromJSON Instance where
        parseJSON
          = withObject "Instance"
              (\ x ->
                 Instance' <$>
                   (x .:? "CreationTime") <*> (x .:? "InstanceId") <*>
                     (x .:? "Status")
                     <*> (x .:? "IpAddress")
                     <*> (x .:? "OperatingSystem")
                     <*> (x .:? "Type")
                     <*> (x .:? "FleetId"))

instance Hashable Instance

instance NFData Instance

-- | Information required to remotely connect to a fleet instance. Access is requested by calling 'GetInstanceAccess' .
--
--
--
-- /See:/ 'instanceAccess' smart constructor.
data InstanceAccess = InstanceAccess'
    { _iaInstanceId      :: !(Maybe Text)
    , _iaIPAddress       :: !(Maybe Text)
    , _iaOperatingSystem :: !(Maybe OperatingSystem)
    , _iaCredentials     :: !(Maybe (Sensitive InstanceCredentials))
    , _iaFleetId         :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaInstanceId' - Unique identifier for an instance being accessed.
--
-- * 'iaIPAddress' - IP address assigned to the instance.
--
-- * 'iaOperatingSystem' - Operating system that is running on the instance.
--
-- * 'iaCredentials' - Credentials required to access the instance.
--
-- * 'iaFleetId' - Unique identifier for a fleet containing the instance being accessed.
instanceAccess
    :: InstanceAccess
instanceAccess =
    InstanceAccess'
    { _iaInstanceId = Nothing
    , _iaIPAddress = Nothing
    , _iaOperatingSystem = Nothing
    , _iaCredentials = Nothing
    , _iaFleetId = Nothing
    }

-- | Unique identifier for an instance being accessed.
iaInstanceId :: Lens' InstanceAccess (Maybe Text)
iaInstanceId = lens _iaInstanceId (\ s a -> s{_iaInstanceId = a});

-- | IP address assigned to the instance.
iaIPAddress :: Lens' InstanceAccess (Maybe Text)
iaIPAddress = lens _iaIPAddress (\ s a -> s{_iaIPAddress = a});

-- | Operating system that is running on the instance.
iaOperatingSystem :: Lens' InstanceAccess (Maybe OperatingSystem)
iaOperatingSystem = lens _iaOperatingSystem (\ s a -> s{_iaOperatingSystem = a});

-- | Credentials required to access the instance.
iaCredentials :: Lens' InstanceAccess (Maybe InstanceCredentials)
iaCredentials = lens _iaCredentials (\ s a -> s{_iaCredentials = a}) . mapping _Sensitive;

-- | Unique identifier for a fleet containing the instance being accessed.
iaFleetId :: Lens' InstanceAccess (Maybe Text)
iaFleetId = lens _iaFleetId (\ s a -> s{_iaFleetId = a});

instance FromJSON InstanceAccess where
        parseJSON
          = withObject "InstanceAccess"
              (\ x ->
                 InstanceAccess' <$>
                   (x .:? "InstanceId") <*> (x .:? "IpAddress") <*>
                     (x .:? "OperatingSystem")
                     <*> (x .:? "Credentials")
                     <*> (x .:? "FleetId"))

instance Hashable InstanceAccess

instance NFData InstanceAccess

-- | Set of credentials required to remotely access a fleet instance. Access credentials are requested by calling 'GetInstanceAccess' and returned in an 'InstanceAccess' object.
--
--
--
-- /See:/ 'instanceCredentials' smart constructor.
data InstanceCredentials = InstanceCredentials'
    { _icUserName :: !(Maybe Text)
    , _icSecret   :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icUserName' - User login string.
--
-- * 'icSecret' - Secret string. For Windows instances, the secret is a password for use with Windows Remote Desktop. For Linux instances, it is a private key (which must be saved as a @.pem@ file) for use with SSH.
instanceCredentials
    :: InstanceCredentials
instanceCredentials =
    InstanceCredentials'
    { _icUserName = Nothing
    , _icSecret = Nothing
    }

-- | User login string.
icUserName :: Lens' InstanceCredentials (Maybe Text)
icUserName = lens _icUserName (\ s a -> s{_icUserName = a});

-- | Secret string. For Windows instances, the secret is a password for use with Windows Remote Desktop. For Linux instances, it is a private key (which must be saved as a @.pem@ file) for use with SSH.
icSecret :: Lens' InstanceCredentials (Maybe Text)
icSecret = lens _icSecret (\ s a -> s{_icSecret = a});

instance FromJSON InstanceCredentials where
        parseJSON
          = withObject "InstanceCredentials"
              (\ x ->
                 InstanceCredentials' <$>
                   (x .:? "UserName") <*> (x .:? "Secret"))

instance Hashable InstanceCredentials

instance NFData InstanceCredentials

-- | Regional latency information for a player, used when requesting a new game session with 'StartGameSessionPlacement' . This value indicates the amount of time lag that exists when the player is connected to a fleet in the specified region. The relative difference between a player's latency values for multiple regions are used to determine which fleets are best suited to place a new game session for the player.
--
--
--
-- /See:/ 'playerLatency' smart constructor.
data PlayerLatency = PlayerLatency'
    { _plLatencyInMilliseconds :: !(Maybe Double)
    , _plRegionIdentifier      :: !(Maybe Text)
    , _plPlayerId              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PlayerLatency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plLatencyInMilliseconds' - Amount of time that represents the time lag experienced by the player when connected to the specified region.
--
-- * 'plRegionIdentifier' - Name of the region that is associated with the latency value.
--
-- * 'plPlayerId' - Unique identifier for a player associated with the latency data.
playerLatency
    :: PlayerLatency
playerLatency =
    PlayerLatency'
    { _plLatencyInMilliseconds = Nothing
    , _plRegionIdentifier = Nothing
    , _plPlayerId = Nothing
    }

-- | Amount of time that represents the time lag experienced by the player when connected to the specified region.
plLatencyInMilliseconds :: Lens' PlayerLatency (Maybe Double)
plLatencyInMilliseconds = lens _plLatencyInMilliseconds (\ s a -> s{_plLatencyInMilliseconds = a});

-- | Name of the region that is associated with the latency value.
plRegionIdentifier :: Lens' PlayerLatency (Maybe Text)
plRegionIdentifier = lens _plRegionIdentifier (\ s a -> s{_plRegionIdentifier = a});

-- | Unique identifier for a player associated with the latency data.
plPlayerId :: Lens' PlayerLatency (Maybe Text)
plPlayerId = lens _plPlayerId (\ s a -> s{_plPlayerId = a});

instance FromJSON PlayerLatency where
        parseJSON
          = withObject "PlayerLatency"
              (\ x ->
                 PlayerLatency' <$>
                   (x .:? "LatencyInMilliseconds") <*>
                     (x .:? "RegionIdentifier")
                     <*> (x .:? "PlayerId"))

instance Hashable PlayerLatency

instance NFData PlayerLatency

instance ToJSON PlayerLatency where
        toJSON PlayerLatency'{..}
          = object
              (catMaybes
                 [("LatencyInMilliseconds" .=) <$>
                    _plLatencyInMilliseconds,
                  ("RegionIdentifier" .=) <$> _plRegionIdentifier,
                  ("PlayerId" .=) <$> _plPlayerId])

-- | Properties describing a player session. A player session represents either a player reservation for a game session or actual player activity in a game session. A player session object (including player data) is automatically passed to a game session when the player connects to the game session and is validated.
--
--
-- Player session-related operations include:
--
--     * 'CreatePlayerSession'
--
--     * 'CreatePlayerSessions'
--
--     * 'DescribePlayerSessions'
--
--
--
--
-- /See:/ 'playerSession' smart constructor.
data PlayerSession = PlayerSession'
    { _psCreationTime    :: !(Maybe POSIX)
    , _psStatus          :: !(Maybe PlayerSessionStatus)
    , _psIPAddress       :: !(Maybe Text)
    , _psGameSessionId   :: !(Maybe Text)
    , _psTerminationTime :: !(Maybe POSIX)
    , _psPlayerSessionId :: !(Maybe Text)
    , _psFleetId         :: !(Maybe Text)
    , _psPlayerData      :: !(Maybe Text)
    , _psPlayerId        :: !(Maybe Text)
    , _psPort            :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PlayerSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'psStatus' - Current status of the player session. Possible player session statuses include the following:     * __RESERVED__ – The player session request has been received, but the player has not yet connected to the server process and/or been validated.      * __ACTIVE__ – The player has been validated by the server process and is currently connected.     * __COMPLETED__ – The player connection has been dropped.     * __TIMEDOUT__ – A player session request was received, but the player did not connect and/or was not validated within the time-out limit (60 seconds).
--
-- * 'psIPAddress' - Game session IP address. All player sessions reference the game session location.
--
-- * 'psGameSessionId' - Unique identifier for the game session that the player session is connected to.
--
-- * 'psTerminationTime' - Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'psPlayerSessionId' - Unique identifier for a player session.
--
-- * 'psFleetId' - Unique identifier for a fleet that the player's game session is running on.
--
-- * 'psPlayerData' - Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
--
-- * 'psPlayerId' - Unique identifier for a player that is associated with this player session.
--
-- * 'psPort' - Port number for the game session. To connect to a Amazon GameLift server process, an app needs both the IP address and port number.
playerSession
    :: PlayerSession
playerSession =
    PlayerSession'
    { _psCreationTime = Nothing
    , _psStatus = Nothing
    , _psIPAddress = Nothing
    , _psGameSessionId = Nothing
    , _psTerminationTime = Nothing
    , _psPlayerSessionId = Nothing
    , _psFleetId = Nothing
    , _psPlayerData = Nothing
    , _psPlayerId = Nothing
    , _psPort = Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
psCreationTime :: Lens' PlayerSession (Maybe UTCTime)
psCreationTime = lens _psCreationTime (\ s a -> s{_psCreationTime = a}) . mapping _Time;

-- | Current status of the player session. Possible player session statuses include the following:     * __RESERVED__ – The player session request has been received, but the player has not yet connected to the server process and/or been validated.      * __ACTIVE__ – The player has been validated by the server process and is currently connected.     * __COMPLETED__ – The player connection has been dropped.     * __TIMEDOUT__ – A player session request was received, but the player did not connect and/or was not validated within the time-out limit (60 seconds).
psStatus :: Lens' PlayerSession (Maybe PlayerSessionStatus)
psStatus = lens _psStatus (\ s a -> s{_psStatus = a});

-- | Game session IP address. All player sessions reference the game session location.
psIPAddress :: Lens' PlayerSession (Maybe Text)
psIPAddress = lens _psIPAddress (\ s a -> s{_psIPAddress = a});

-- | Unique identifier for the game session that the player session is connected to.
psGameSessionId :: Lens' PlayerSession (Maybe Text)
psGameSessionId = lens _psGameSessionId (\ s a -> s{_psGameSessionId = a});

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
psTerminationTime :: Lens' PlayerSession (Maybe UTCTime)
psTerminationTime = lens _psTerminationTime (\ s a -> s{_psTerminationTime = a}) . mapping _Time;

-- | Unique identifier for a player session.
psPlayerSessionId :: Lens' PlayerSession (Maybe Text)
psPlayerSessionId = lens _psPlayerSessionId (\ s a -> s{_psPlayerSessionId = a});

-- | Unique identifier for a fleet that the player's game session is running on.
psFleetId :: Lens' PlayerSession (Maybe Text)
psFleetId = lens _psFleetId (\ s a -> s{_psFleetId = a});

-- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
psPlayerData :: Lens' PlayerSession (Maybe Text)
psPlayerData = lens _psPlayerData (\ s a -> s{_psPlayerData = a});

-- | Unique identifier for a player that is associated with this player session.
psPlayerId :: Lens' PlayerSession (Maybe Text)
psPlayerId = lens _psPlayerId (\ s a -> s{_psPlayerId = a});

-- | Port number for the game session. To connect to a Amazon GameLift server process, an app needs both the IP address and port number.
psPort :: Lens' PlayerSession (Maybe Natural)
psPort = lens _psPort (\ s a -> s{_psPort = a}) . mapping _Nat;

instance FromJSON PlayerSession where
        parseJSON
          = withObject "PlayerSession"
              (\ x ->
                 PlayerSession' <$>
                   (x .:? "CreationTime") <*> (x .:? "Status") <*>
                     (x .:? "IpAddress")
                     <*> (x .:? "GameSessionId")
                     <*> (x .:? "TerminationTime")
                     <*> (x .:? "PlayerSessionId")
                     <*> (x .:? "FleetId")
                     <*> (x .:? "PlayerData")
                     <*> (x .:? "PlayerId")
                     <*> (x .:? "Port"))

instance Hashable PlayerSession

instance NFData PlayerSession

-- | Policy that limits the number of game sessions a player can create on the same fleet. This optional policy gives game owners control over how players can consume available game server resources. A resource creation policy makes the following statement: "An individual player can create a maximum number of new game sessions within a specified time period".
--
--
-- The policy is evaluated when a player tries to create a new game session. For example, with a policy of 10 new game sessions and a time period of 60 minutes, on receiving a @CreateGameSession@ request, Amazon GameLift checks that the player (identified by @CreatorId@ ) has created fewer than 10 game sessions in the past 60 minutes.
--
--
-- /See:/ 'resourceCreationLimitPolicy' smart constructor.
data ResourceCreationLimitPolicy = ResourceCreationLimitPolicy'
    { _rclpNewGameSessionsPerCreator :: !(Maybe Nat)
    , _rclpPolicyPeriodInMinutes     :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourceCreationLimitPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rclpNewGameSessionsPerCreator' - Maximum number of game sessions that an individual can create during the policy period.
--
-- * 'rclpPolicyPeriodInMinutes' - Time span used in evaluating the resource creation limit policy.
resourceCreationLimitPolicy
    :: ResourceCreationLimitPolicy
resourceCreationLimitPolicy =
    ResourceCreationLimitPolicy'
    { _rclpNewGameSessionsPerCreator = Nothing
    , _rclpPolicyPeriodInMinutes = Nothing
    }

-- | Maximum number of game sessions that an individual can create during the policy period.
rclpNewGameSessionsPerCreator :: Lens' ResourceCreationLimitPolicy (Maybe Natural)
rclpNewGameSessionsPerCreator = lens _rclpNewGameSessionsPerCreator (\ s a -> s{_rclpNewGameSessionsPerCreator = a}) . mapping _Nat;

-- | Time span used in evaluating the resource creation limit policy.
rclpPolicyPeriodInMinutes :: Lens' ResourceCreationLimitPolicy (Maybe Natural)
rclpPolicyPeriodInMinutes = lens _rclpPolicyPeriodInMinutes (\ s a -> s{_rclpPolicyPeriodInMinutes = a}) . mapping _Nat;

instance FromJSON ResourceCreationLimitPolicy where
        parseJSON
          = withObject "ResourceCreationLimitPolicy"
              (\ x ->
                 ResourceCreationLimitPolicy' <$>
                   (x .:? "NewGameSessionsPerCreator") <*>
                     (x .:? "PolicyPeriodInMinutes"))

instance Hashable ResourceCreationLimitPolicy

instance NFData ResourceCreationLimitPolicy

instance ToJSON ResourceCreationLimitPolicy where
        toJSON ResourceCreationLimitPolicy'{..}
          = object
              (catMaybes
                 [("NewGameSessionsPerCreator" .=) <$>
                    _rclpNewGameSessionsPerCreator,
                  ("PolicyPeriodInMinutes" .=) <$>
                    _rclpPolicyPeriodInMinutes])

-- | Routing configuration for a fleet alias.
--
--
--
-- /See:/ 'routingStrategy' smart constructor.
data RoutingStrategy = RoutingStrategy'
    { _rsType    :: !(Maybe RoutingStrategyType)
    , _rsMessage :: !(Maybe Text)
    , _rsFleetId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RoutingStrategy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsType' - Type of routing strategy. Possible routing types include the following:     * __SIMPLE__ – The alias resolves to one specific fleet. Use this type when routing to active fleets.     * __TERMINAL__ – The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
--
-- * 'rsMessage' - Message text to be used with a terminal routing strategy.
--
-- * 'rsFleetId' - Unique identifier for a fleet that the alias points to.
routingStrategy
    :: RoutingStrategy
routingStrategy =
    RoutingStrategy'
    { _rsType = Nothing
    , _rsMessage = Nothing
    , _rsFleetId = Nothing
    }

-- | Type of routing strategy. Possible routing types include the following:     * __SIMPLE__ – The alias resolves to one specific fleet. Use this type when routing to active fleets.     * __TERMINAL__ – The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
rsType :: Lens' RoutingStrategy (Maybe RoutingStrategyType)
rsType = lens _rsType (\ s a -> s{_rsType = a});

-- | Message text to be used with a terminal routing strategy.
rsMessage :: Lens' RoutingStrategy (Maybe Text)
rsMessage = lens _rsMessage (\ s a -> s{_rsMessage = a});

-- | Unique identifier for a fleet that the alias points to.
rsFleetId :: Lens' RoutingStrategy (Maybe Text)
rsFleetId = lens _rsFleetId (\ s a -> s{_rsFleetId = a});

instance FromJSON RoutingStrategy where
        parseJSON
          = withObject "RoutingStrategy"
              (\ x ->
                 RoutingStrategy' <$>
                   (x .:? "Type") <*> (x .:? "Message") <*>
                     (x .:? "FleetId"))

instance Hashable RoutingStrategy

instance NFData RoutingStrategy

instance ToJSON RoutingStrategy where
        toJSON RoutingStrategy'{..}
          = object
              (catMaybes
                 [("Type" .=) <$> _rsType,
                  ("Message" .=) <$> _rsMessage,
                  ("FleetId" .=) <$> _rsFleetId])

-- | Collection of server process configurations that describe what processes should be run on each instance in a fleet. An instance can launch and maintain multiple server processes based on the runtime configuration; it regularly checks for an updated runtime configuration and starts new server processes to match the latest version.
--
--
-- The key purpose of a runtime configuration with multiple server process configurations is to be able to run more than one kind of game server in a single fleet. You can include configurations for more than one server executable in order to run two or more different programs to run on the same instance. This option might be useful, for example, to run more than one version of your game server on the same fleet. Another option is to specify configurations for the same server executable but with different launch parameters.
--
-- A Amazon GameLift instance is limited to 50 processes running simultaneously. To calculate the total number of processes specified in a runtime configuration, add the values of the @ConcurrentExecutions@ parameter for each @'ServerProcess' @ object in the runtime configuration.
--
--
-- /See:/ 'runtimeConfiguration' smart constructor.
newtype RuntimeConfiguration = RuntimeConfiguration'
    { _rcServerProcesses :: Maybe (List1 ServerProcess)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RuntimeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcServerProcesses' - Collection of server process configurations describing what server processes to run on each instance in a fleet
runtimeConfiguration
    :: RuntimeConfiguration
runtimeConfiguration =
    RuntimeConfiguration'
    { _rcServerProcesses = Nothing
    }

-- | Collection of server process configurations describing what server processes to run on each instance in a fleet
rcServerProcesses :: Lens' RuntimeConfiguration (Maybe (NonEmpty ServerProcess))
rcServerProcesses = lens _rcServerProcesses (\ s a -> s{_rcServerProcesses = a}) . mapping _List1;

instance FromJSON RuntimeConfiguration where
        parseJSON
          = withObject "RuntimeConfiguration"
              (\ x ->
                 RuntimeConfiguration' <$> (x .:? "ServerProcesses"))

instance Hashable RuntimeConfiguration

instance NFData RuntimeConfiguration

instance ToJSON RuntimeConfiguration where
        toJSON RuntimeConfiguration'{..}
          = object
              (catMaybes
                 [("ServerProcesses" .=) <$> _rcServerProcesses])

-- | Location in Amazon Simple Storage Service (Amazon S3) where build files can be stored for access by Amazon GameLift. This location is specified in a 'CreateBuild' request. For more details, see the <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build Create a Build with Files in Amazon S3> .
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
    { _slBucket  :: !(Maybe Text)
    , _slKey     :: !(Maybe Text)
    , _slRoleARN :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slBucket' - Amazon S3 bucket identifier. This is the name of your S3 bucket.
--
-- * 'slKey' - Name of the zip file containing your build files.
--
-- * 'slRoleARN' - Amazon Resource Name (<http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for the access role that allows Amazon GameLift to access your S3 bucket.
s3Location
    :: S3Location
s3Location =
    S3Location'
    { _slBucket = Nothing
    , _slKey = Nothing
    , _slRoleARN = Nothing
    }

-- | Amazon S3 bucket identifier. This is the name of your S3 bucket.
slBucket :: Lens' S3Location (Maybe Text)
slBucket = lens _slBucket (\ s a -> s{_slBucket = a});

-- | Name of the zip file containing your build files.
slKey :: Lens' S3Location (Maybe Text)
slKey = lens _slKey (\ s a -> s{_slKey = a});

-- | Amazon Resource Name (<http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for the access role that allows Amazon GameLift to access your S3 bucket.
slRoleARN :: Lens' S3Location (Maybe Text)
slRoleARN = lens _slRoleARN (\ s a -> s{_slRoleARN = a});

instance FromJSON S3Location where
        parseJSON
          = withObject "S3Location"
              (\ x ->
                 S3Location' <$>
                   (x .:? "Bucket") <*> (x .:? "Key") <*>
                     (x .:? "RoleArn"))

instance Hashable S3Location

instance NFData S3Location

instance ToJSON S3Location where
        toJSON S3Location'{..}
          = object
              (catMaybes
                 [("Bucket" .=) <$> _slBucket, ("Key" .=) <$> _slKey,
                  ("RoleArn" .=) <$> _slRoleARN])

-- | Rule that controls how a fleet is scaled. Scaling policies are uniquely identified by the combination of name and fleet ID.
--
--
--
-- /See:/ 'scalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
    { _spStatus                :: !(Maybe ScalingStatusType)
    , _spScalingAdjustmentType :: !(Maybe ScalingAdjustmentType)
    , _spEvaluationPeriods     :: !(Maybe Nat)
    , _spMetricName            :: !(Maybe MetricName)
    , _spComparisonOperator    :: !(Maybe ComparisonOperatorType)
    , _spName                  :: !(Maybe Text)
    , _spThreshold             :: !(Maybe Double)
    , _spScalingAdjustment     :: !(Maybe Int)
    , _spFleetId               :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spStatus' - Current status of the scaling policy. The scaling policy is only in force when in an @ACTIVE@ status.     * __ACTIVE__ – The scaling policy is currently in force.     * __UPDATE_REQUESTED__ – A request to update the scaling policy has been received.     * __UPDATING__ – A change is being made to the scaling policy.     * __DELETE_REQUESTED__ – A request to delete the scaling policy has been received.     * __DELETING__ – The scaling policy is being deleted.     * __DELETED__ – The scaling policy has been deleted.     * __ERROR__ – An error occurred in creating the policy. It should be removed and recreated.
--
-- * 'spScalingAdjustmentType' - Type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):     * __ChangeInCapacity__ – add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.     * __ExactCapacity__ – set the instance count to the scaling adjustment value.     * __PercentChangeInCapacity__ – increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down.
--
-- * 'spEvaluationPeriods' - Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
--
-- * 'spMetricName' - Name of the Amazon GameLift-defined metric that is used to trigger an adjustment.     * __ActivatingGameSessions__ – number of game sessions in the process of being created (game session status = @ACTIVATING@ ).     * __ActiveGameSessions__ – number of game sessions currently running (game session status = @ACTIVE@ ).     * __CurrentPlayerSessions__ – number of active or reserved player sessions (player session status = @ACTIVE@ or @RESERVED@ ).      * __AvailablePlayerSessions__ – number of player session slots currently available in active game sessions across the fleet, calculated by subtracting a game session's current player session count from its maximum player session count. This number does include game sessions that are not currently accepting players (game session @PlayerSessionCreationPolicy@ = @DENY_ALL@ ).     * __ActiveInstances__ – number of instances currently running a game session.     * __IdleInstances__ – number of instances not currently running a game session.
--
-- * 'spComparisonOperator' - Comparison operator to use when measuring a metric against the threshold value.
--
-- * 'spName' - Descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
--
-- * 'spThreshold' - Metric value used to trigger a scaling event.
--
-- * 'spScalingAdjustment' - Amount of adjustment to make, based on the scaling adjustment type.
--
-- * 'spFleetId' - Unique identifier for a fleet that is associated with this scaling policy.
scalingPolicy
    :: ScalingPolicy
scalingPolicy =
    ScalingPolicy'
    { _spStatus = Nothing
    , _spScalingAdjustmentType = Nothing
    , _spEvaluationPeriods = Nothing
    , _spMetricName = Nothing
    , _spComparisonOperator = Nothing
    , _spName = Nothing
    , _spThreshold = Nothing
    , _spScalingAdjustment = Nothing
    , _spFleetId = Nothing
    }

-- | Current status of the scaling policy. The scaling policy is only in force when in an @ACTIVE@ status.     * __ACTIVE__ – The scaling policy is currently in force.     * __UPDATE_REQUESTED__ – A request to update the scaling policy has been received.     * __UPDATING__ – A change is being made to the scaling policy.     * __DELETE_REQUESTED__ – A request to delete the scaling policy has been received.     * __DELETING__ – The scaling policy is being deleted.     * __DELETED__ – The scaling policy has been deleted.     * __ERROR__ – An error occurred in creating the policy. It should be removed and recreated.
spStatus :: Lens' ScalingPolicy (Maybe ScalingStatusType)
spStatus = lens _spStatus (\ s a -> s{_spStatus = a});

-- | Type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):     * __ChangeInCapacity__ – add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.     * __ExactCapacity__ – set the instance count to the scaling adjustment value.     * __PercentChangeInCapacity__ – increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down.
spScalingAdjustmentType :: Lens' ScalingPolicy (Maybe ScalingAdjustmentType)
spScalingAdjustmentType = lens _spScalingAdjustmentType (\ s a -> s{_spScalingAdjustmentType = a});

-- | Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
spEvaluationPeriods :: Lens' ScalingPolicy (Maybe Natural)
spEvaluationPeriods = lens _spEvaluationPeriods (\ s a -> s{_spEvaluationPeriods = a}) . mapping _Nat;

-- | Name of the Amazon GameLift-defined metric that is used to trigger an adjustment.     * __ActivatingGameSessions__ – number of game sessions in the process of being created (game session status = @ACTIVATING@ ).     * __ActiveGameSessions__ – number of game sessions currently running (game session status = @ACTIVE@ ).     * __CurrentPlayerSessions__ – number of active or reserved player sessions (player session status = @ACTIVE@ or @RESERVED@ ).      * __AvailablePlayerSessions__ – number of player session slots currently available in active game sessions across the fleet, calculated by subtracting a game session's current player session count from its maximum player session count. This number does include game sessions that are not currently accepting players (game session @PlayerSessionCreationPolicy@ = @DENY_ALL@ ).     * __ActiveInstances__ – number of instances currently running a game session.     * __IdleInstances__ – number of instances not currently running a game session.
spMetricName :: Lens' ScalingPolicy (Maybe MetricName)
spMetricName = lens _spMetricName (\ s a -> s{_spMetricName = a});

-- | Comparison operator to use when measuring a metric against the threshold value.
spComparisonOperator :: Lens' ScalingPolicy (Maybe ComparisonOperatorType)
spComparisonOperator = lens _spComparisonOperator (\ s a -> s{_spComparisonOperator = a});

-- | Descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
spName :: Lens' ScalingPolicy (Maybe Text)
spName = lens _spName (\ s a -> s{_spName = a});

-- | Metric value used to trigger a scaling event.
spThreshold :: Lens' ScalingPolicy (Maybe Double)
spThreshold = lens _spThreshold (\ s a -> s{_spThreshold = a});

-- | Amount of adjustment to make, based on the scaling adjustment type.
spScalingAdjustment :: Lens' ScalingPolicy (Maybe Int)
spScalingAdjustment = lens _spScalingAdjustment (\ s a -> s{_spScalingAdjustment = a});

-- | Unique identifier for a fleet that is associated with this scaling policy.
spFleetId :: Lens' ScalingPolicy (Maybe Text)
spFleetId = lens _spFleetId (\ s a -> s{_spFleetId = a});

instance FromJSON ScalingPolicy where
        parseJSON
          = withObject "ScalingPolicy"
              (\ x ->
                 ScalingPolicy' <$>
                   (x .:? "Status") <*> (x .:? "ScalingAdjustmentType")
                     <*> (x .:? "EvaluationPeriods")
                     <*> (x .:? "MetricName")
                     <*> (x .:? "ComparisonOperator")
                     <*> (x .:? "Name")
                     <*> (x .:? "Threshold")
                     <*> (x .:? "ScalingAdjustment")
                     <*> (x .:? "FleetId"))

instance Hashable ScalingPolicy

instance NFData ScalingPolicy

-- | A set of instructions for launching server processes on each instance in a fleet. Each instruction set identifies the location of the server executable, optional launch parameters, and the number of server processes with this configuration to maintain concurrently on the instance. Server process configurations make up a fleet's @'RuntimeConfiguration' @ .
--
--
--
-- /See:/ 'serverProcess' smart constructor.
data ServerProcess = ServerProcess'
    { _spParameters           :: !(Maybe Text)
    , _spLaunchPath           :: !Text
    , _spConcurrentExecutions :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ServerProcess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spParameters' - Optional list of parameters to pass to the server executable on launch.
--
-- * 'spLaunchPath' - Location of the server executable in a game build. All game builds are installed on instances at the root : for Windows instances @C:\game@ , and for Linux instances @/local/game@ . A Windows game build with an executable file located at @MyGame\latest\server.exe@ must have a launch path of "@C:\game\MyGame\latest\server.exe@ ". A Linux game build with an executable file located at @MyGame/latest/server.exe@ must have a launch path of "@/local/game/MyGame/latest/server.exe@ ".
--
-- * 'spConcurrentExecutions' - Number of server processes using this configuration to run concurrently on an instance.
serverProcess
    :: Text -- ^ 'spLaunchPath'
    -> Natural -- ^ 'spConcurrentExecutions'
    -> ServerProcess
serverProcess pLaunchPath_ pConcurrentExecutions_ =
    ServerProcess'
    { _spParameters = Nothing
    , _spLaunchPath = pLaunchPath_
    , _spConcurrentExecutions = _Nat # pConcurrentExecutions_
    }

-- | Optional list of parameters to pass to the server executable on launch.
spParameters :: Lens' ServerProcess (Maybe Text)
spParameters = lens _spParameters (\ s a -> s{_spParameters = a});

-- | Location of the server executable in a game build. All game builds are installed on instances at the root : for Windows instances @C:\game@ , and for Linux instances @/local/game@ . A Windows game build with an executable file located at @MyGame\latest\server.exe@ must have a launch path of "@C:\game\MyGame\latest\server.exe@ ". A Linux game build with an executable file located at @MyGame/latest/server.exe@ must have a launch path of "@/local/game/MyGame/latest/server.exe@ ".
spLaunchPath :: Lens' ServerProcess Text
spLaunchPath = lens _spLaunchPath (\ s a -> s{_spLaunchPath = a});

-- | Number of server processes using this configuration to run concurrently on an instance.
spConcurrentExecutions :: Lens' ServerProcess Natural
spConcurrentExecutions = lens _spConcurrentExecutions (\ s a -> s{_spConcurrentExecutions = a}) . _Nat;

instance FromJSON ServerProcess where
        parseJSON
          = withObject "ServerProcess"
              (\ x ->
                 ServerProcess' <$>
                   (x .:? "Parameters") <*> (x .: "LaunchPath") <*>
                     (x .: "ConcurrentExecutions"))

instance Hashable ServerProcess

instance NFData ServerProcess

instance ToJSON ServerProcess where
        toJSON ServerProcess'{..}
          = object
              (catMaybes
                 [("Parameters" .=) <$> _spParameters,
                  Just ("LaunchPath" .= _spLaunchPath),
                  Just
                    ("ConcurrentExecutions" .= _spConcurrentExecutions)])
