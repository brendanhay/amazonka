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

-- | AWS access credentials required to upload game build files to Amazon
-- GameLift. These credentials are generated with < CreateBuild>, and are
-- valid for a limited time. If they expire before you upload your game
-- build, get a new set by calling < RequestUploadCredentials>.
--
-- /See:/ 'awsCredentials' smart constructor.
data AWSCredentials = AWSCredentials'
    { _acSecretAccessKey :: !(Maybe Text)
    , _acSessionToken    :: !(Maybe Text)
    , _acAccessKeyId     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AWSCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acSecretAccessKey'
--
-- * 'acSessionToken'
--
-- * 'acAccessKeyId'
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
-- /See:/ 'alias' smart constructor.
data Alias = Alias'
    { _aCreationTime    :: !(Maybe POSIX)
    , _aLastUpdatedTime :: !(Maybe POSIX)
    , _aAliasId         :: !(Maybe Text)
    , _aRoutingStrategy :: !(Maybe RoutingStrategy)
    , _aName            :: !(Maybe Text)
    , _aDescription     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Alias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aCreationTime'
--
-- * 'aLastUpdatedTime'
--
-- * 'aAliasId'
--
-- * 'aRoutingStrategy'
--
-- * 'aName'
--
-- * 'aDescription'
alias
    :: Alias
alias =
    Alias'
    { _aCreationTime = Nothing
    , _aLastUpdatedTime = Nothing
    , _aAliasId = Nothing
    , _aRoutingStrategy = Nothing
    , _aName = Nothing
    , _aDescription = Nothing
    }

-- | Time stamp indicating when this object was created. Format is an integer
-- representing the number of seconds since the Unix epoch (Unix time).
aCreationTime :: Lens' Alias (Maybe UTCTime)
aCreationTime = lens _aCreationTime (\ s a -> s{_aCreationTime = a}) . mapping _Time;

-- | Time stamp indicating when this object was last modified. Format is an
-- integer representing the number of seconds since the Unix epoch (Unix
-- time).
aLastUpdatedTime :: Lens' Alias (Maybe UTCTime)
aLastUpdatedTime = lens _aLastUpdatedTime (\ s a -> s{_aLastUpdatedTime = a}) . mapping _Time;

-- | Unique identifier for a fleet alias.
aAliasId :: Lens' Alias (Maybe Text)
aAliasId = lens _aAliasId (\ s a -> s{_aAliasId = a});

-- | Undocumented member.
aRoutingStrategy :: Lens' Alias (Maybe RoutingStrategy)
aRoutingStrategy = lens _aRoutingStrategy (\ s a -> s{_aRoutingStrategy = a});

-- | Descriptive label associated with this alias. Alias names do not need to
-- be unique.
aName :: Lens' Alias (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

-- | Human-readable description of the alias.
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
                     <*> (x .:? "Description"))

instance Hashable Alias

instance NFData Alias

-- | Properties describing a game build.
--
-- /See:/ 'build' smart constructor.
data Build = Build'
    { _bCreationTime :: !(Maybe POSIX)
    , _bStatus       :: !(Maybe BuildStatus)
    , _bBuildId      :: !(Maybe Text)
    , _bName         :: !(Maybe Text)
    , _bVersion      :: !(Maybe Text)
    , _bSizeOnDisk   :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Build' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bCreationTime'
--
-- * 'bStatus'
--
-- * 'bBuildId'
--
-- * 'bName'
--
-- * 'bVersion'
--
-- * 'bSizeOnDisk'
build
    :: Build
build =
    Build'
    { _bCreationTime = Nothing
    , _bStatus = Nothing
    , _bBuildId = Nothing
    , _bName = Nothing
    , _bVersion = Nothing
    , _bSizeOnDisk = Nothing
    }

-- | Time stamp indicating when this object was created. Format is an integer
-- representing the number of seconds since the Unix epoch (Unix time).
bCreationTime :: Lens' Build (Maybe UTCTime)
bCreationTime = lens _bCreationTime (\ s a -> s{_bCreationTime = a}) . mapping _Time;

-- | Current status of the build. Possible build states include:
--
-- -   INITIALIZED: A new build has been defined, but no files have been
--     uploaded. You cannot create fleets for builds that are in this
--     state. When a build is successfully created, the build state is set
--     to this value.
-- -   READY: The game build has been successfully uploaded. You can now
--     create new fleets for this build.
-- -   FAILED: The game build upload failed. You cannot create new fleets
--     for this build.
bStatus :: Lens' Build (Maybe BuildStatus)
bStatus = lens _bStatus (\ s a -> s{_bStatus = a});

-- | Unique identifier for a build.
bBuildId :: Lens' Build (Maybe Text)
bBuildId = lens _bBuildId (\ s a -> s{_bBuildId = a});

-- | Descriptive label associated with this build. Build names do not need to
-- be unique. It can be set using < CreateBuild> or < UpdateBuild>.
bName :: Lens' Build (Maybe Text)
bName = lens _bName (\ s a -> s{_bName = a});

-- | Version associated with this build. Version strings do not need to be
-- unique to a build. This value can be set using < CreateBuild> or
-- < UpdateBuild>.
bVersion :: Lens' Build (Maybe Text)
bVersion = lens _bVersion (\ s a -> s{_bVersion = a});

-- | File size of the uploaded game build, expressed in bytes. When the build
-- state is INITIALIZED, this value is 0.
bSizeOnDisk :: Lens' Build (Maybe Natural)
bSizeOnDisk = lens _bSizeOnDisk (\ s a -> s{_bSizeOnDisk = a}) . mapping _Nat;

instance FromJSON Build where
        parseJSON
          = withObject "Build"
              (\ x ->
                 Build' <$>
                   (x .:? "CreationTime") <*> (x .:? "Status") <*>
                     (x .:? "BuildId")
                     <*> (x .:? "Name")
                     <*> (x .:? "Version")
                     <*> (x .:? "SizeOnDisk"))

instance Hashable Build

instance NFData Build

-- | Current status of fleet capacity. The number of active instances should
-- match or be in the process of matching the number of desired instances.
-- Pending and terminating counts are non-zero only if fleet capacity is
-- adjusting to an < UpdateFleetCapacity> request, or if access to
-- resources is temporarily affected.
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
-- * 'eicIdLE'
--
-- * 'eicTERMINATING'
--
-- * 'eicPENDING'
--
-- * 'eicMAXIMUM'
--
-- * 'eicDESIRED'
--
-- * 'eicMINIMUM'
--
-- * 'eicACTIVE'
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

-- | Number of active instances in the fleet that are not currently hosting a
-- game session.
eicIdLE :: Lens' EC2InstanceCounts (Maybe Natural)
eicIdLE = lens _eicIdLE (\ s a -> s{_eicIdLE = a}) . mapping _Nat;

-- | Number of instances in the fleet that are no longer active but haven\'t
-- yet been terminated.
eicTERMINATING :: Lens' EC2InstanceCounts (Maybe Natural)
eicTERMINATING = lens _eicTERMINATING (\ s a -> s{_eicTERMINATING = a}) . mapping _Nat;

-- | Number of instances in the fleet that are starting but not yet active.
eicPENDING :: Lens' EC2InstanceCounts (Maybe Natural)
eicPENDING = lens _eicPENDING (\ s a -> s{_eicPENDING = a}) . mapping _Nat;

-- | Maximum value allowed for the fleet\'s instance count.
eicMAXIMUM :: Lens' EC2InstanceCounts (Maybe Natural)
eicMAXIMUM = lens _eicMAXIMUM (\ s a -> s{_eicMAXIMUM = a}) . mapping _Nat;

-- | Ideal number of active instances in the fleet.
eicDESIRED :: Lens' EC2InstanceCounts (Maybe Natural)
eicDESIRED = lens _eicDESIRED (\ s a -> s{_eicDESIRED = a}) . mapping _Nat;

-- | Minimum value allowed for the fleet\'s instance count.
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

-- | Maximum number of instances allowed based on the Amazon Elastic Compute
-- Cloud (Amazon EC2) instance type. Instance limits can be retrieved by
-- calling < DescribeEC2InstanceLimits>.
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
-- * 'eilEC2InstanceType'
--
-- * 'eilCurrentInstances'
--
-- * 'eilInstanceLimit'
ec2InstanceLimit
    :: EC2InstanceLimit
ec2InstanceLimit =
    EC2InstanceLimit'
    { _eilEC2InstanceType = Nothing
    , _eilCurrentInstances = Nothing
    , _eilInstanceLimit = Nothing
    }

-- | Type of EC2 instances used in the fleet. EC2 instance types define the
-- CPU, memory, storage, and networking capacity of the fleetaposs hosts.
-- Amazon GameLift supports the EC2 instance types listed below. See
-- <https://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions of each.
eilEC2InstanceType :: Lens' EC2InstanceLimit (Maybe EC2InstanceType)
eilEC2InstanceType = lens _eilEC2InstanceType (\ s a -> s{_eilEC2InstanceType = a});

-- | Number of instances of the specified type that are currently in use by
-- this AWS account.
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

-- | Log entry describing an event involving an Amazon GameLift resource
-- (such as a fleet).
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
-- * 'eResourceId'
--
-- * 'eEventTime'
--
-- * 'eMessage'
--
-- * 'eEventCode'
--
-- * 'eEventId'
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

-- | Unique identifier for the resource, such as a fleet ID.
eResourceId :: Lens' Event (Maybe Text)
eResourceId = lens _eResourceId (\ s a -> s{_eResourceId = a});

-- | Time stamp indicating when this event occurred. Format is an integer
-- representing the number of seconds since the Unix epoch (Unix time).
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
-- /See:/ 'fleetAttributes' smart constructor.
data FleetAttributes = FleetAttributes'
    { _faCreationTime                   :: !(Maybe POSIX)
    , _faStatus                         :: !(Maybe FleetStatus)
    , _faServerLaunchParameters         :: !(Maybe Text)
    , _faLogPaths                       :: !(Maybe [Text])
    , _faBuildId                        :: !(Maybe Text)
    , _faTerminationTime                :: !(Maybe POSIX)
    , _faNewGameSessionProtectionPolicy :: !(Maybe ProtectionPolicy)
    , _faName                           :: !(Maybe Text)
    , _faServerLaunchPath               :: !(Maybe Text)
    , _faFleetId                        :: !(Maybe Text)
    , _faDescription                    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FleetAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faCreationTime'
--
-- * 'faStatus'
--
-- * 'faServerLaunchParameters'
--
-- * 'faLogPaths'
--
-- * 'faBuildId'
--
-- * 'faTerminationTime'
--
-- * 'faNewGameSessionProtectionPolicy'
--
-- * 'faName'
--
-- * 'faServerLaunchPath'
--
-- * 'faFleetId'
--
-- * 'faDescription'
fleetAttributes
    :: FleetAttributes
fleetAttributes =
    FleetAttributes'
    { _faCreationTime = Nothing
    , _faStatus = Nothing
    , _faServerLaunchParameters = Nothing
    , _faLogPaths = Nothing
    , _faBuildId = Nothing
    , _faTerminationTime = Nothing
    , _faNewGameSessionProtectionPolicy = Nothing
    , _faName = Nothing
    , _faServerLaunchPath = Nothing
    , _faFleetId = Nothing
    , _faDescription = Nothing
    }

-- | Time stamp indicating when this object was created. Format is an integer
-- representing the number of seconds since the Unix epoch (Unix time).
faCreationTime :: Lens' FleetAttributes (Maybe UTCTime)
faCreationTime = lens _faCreationTime (\ s a -> s{_faCreationTime = a}) . mapping _Time;

-- | Current status of the fleet. Possible fleet states include:
--
-- -   NEW: A new fleet has been defined and hosts allocated.
-- -   DOWNLOADING\/VALIDATING\/BUILDING\/ACTIVATING: The new fleet is
--     being set up with the game build, and new hosts are being started.
-- -   ACTIVE: Hosts can now accept game sessions.
-- -   ERROR: An error occurred when downloading, validating, building, or
--     activating the fleet.
-- -   DELETING: Hosts are responding to a delete fleet request.
-- -   TERMINATED: The fleet no longer exists.
faStatus :: Lens' FleetAttributes (Maybe FleetStatus)
faStatus = lens _faStatus (\ s a -> s{_faStatus = a});

-- | Parameters required to launch your game server. These parameters should
-- be expressed as a string of command-line parameters. Example: \"+sv_port
-- 33435 +start_lobby\".
faServerLaunchParameters :: Lens' FleetAttributes (Maybe Text)
faServerLaunchParameters = lens _faServerLaunchParameters (\ s a -> s{_faServerLaunchParameters = a});

-- | Path to game-session log files generated by your game server. Once a
-- game session has been terminated, Amazon GameLift captures and stores
-- the logs on Amazon S3. Use the GameLift console to access the stored
-- logs.
faLogPaths :: Lens' FleetAttributes [Text]
faLogPaths = lens _faLogPaths (\ s a -> s{_faLogPaths = a}) . _Default . _Coerce;

-- | Unique identifier for a build.
faBuildId :: Lens' FleetAttributes (Maybe Text)
faBuildId = lens _faBuildId (\ s a -> s{_faBuildId = a});

-- | Time stamp indicating when this fleet was terminated. Format is an
-- integer representing the number of seconds since the Unix epoch (Unix
-- time).
faTerminationTime :: Lens' FleetAttributes (Maybe UTCTime)
faTerminationTime = lens _faTerminationTime (\ s a -> s{_faTerminationTime = a}) . mapping _Time;

-- | Type of game session protection to set for all new instances started in
-- the fleet.
--
-- -   NoProtection: The game session can be terminated during a scale-down
--     event.
-- -   FullProtection: If the game session is in an ACTIVE status, it
--     cannot be terminated during a scale-down event.
faNewGameSessionProtectionPolicy :: Lens' FleetAttributes (Maybe ProtectionPolicy)
faNewGameSessionProtectionPolicy = lens _faNewGameSessionProtectionPolicy (\ s a -> s{_faNewGameSessionProtectionPolicy = a});

-- | Descriptive label associated with this fleet. Fleet names do not need to
-- be unique.
faName :: Lens' FleetAttributes (Maybe Text)
faName = lens _faName (\ s a -> s{_faName = a});

-- | Path to the launch executable for the game server. A game server is
-- built into a 'C:\\game' drive. This value must be expressed as
-- 'C:\\game\\[launchpath]'. Example: If, when built, your game server
-- files are in a folder called \"MyGame\", your log path should be
-- 'C:\\game\\MyGame\\server.exe'.
faServerLaunchPath :: Lens' FleetAttributes (Maybe Text)
faServerLaunchPath = lens _faServerLaunchPath (\ s a -> s{_faServerLaunchPath = a});

-- | Unique identifier for a fleet.
faFleetId :: Lens' FleetAttributes (Maybe Text)
faFleetId = lens _faFleetId (\ s a -> s{_faFleetId = a});

-- | Human-readable description of the fleet.
faDescription :: Lens' FleetAttributes (Maybe Text)
faDescription = lens _faDescription (\ s a -> s{_faDescription = a});

instance FromJSON FleetAttributes where
        parseJSON
          = withObject "FleetAttributes"
              (\ x ->
                 FleetAttributes' <$>
                   (x .:? "CreationTime") <*> (x .:? "Status") <*>
                     (x .:? "ServerLaunchParameters")
                     <*> (x .:? "LogPaths" .!= mempty)
                     <*> (x .:? "BuildId")
                     <*> (x .:? "TerminationTime")
                     <*> (x .:? "NewGameSessionProtectionPolicy")
                     <*> (x .:? "Name")
                     <*> (x .:? "ServerLaunchPath")
                     <*> (x .:? "FleetId")
                     <*> (x .:? "Description"))

instance Hashable FleetAttributes

instance NFData FleetAttributes

-- | Information about the fleet\'s capacity. Fleet capacity is measured in
-- EC2 instances. By default, new fleets have a capacity of one instance,
-- but can be updated as needed. The maximum number of instances for a
-- fleet is determined by the fleet\'s instance type.
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
-- * 'fcInstanceType'
--
-- * 'fcFleetId'
--
-- * 'fcInstanceCounts'
fleetCapacity
    :: FleetCapacity
fleetCapacity =
    FleetCapacity'
    { _fcInstanceType = Nothing
    , _fcFleetId = Nothing
    , _fcInstanceCounts = Nothing
    }

-- | Type of EC2 instances used in the fleet. EC2 instance types define the
-- CPU, memory, storage, and networking capacity of the fleetaposs hosts.
-- Amazon GameLift supports the EC2 instance types listed below. See
-- <https://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions of each.
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

-- | Current status of fleet utilization, including the number of game and
-- player sessions being hosted.
--
-- /See:/ 'fleetUtilization' smart constructor.
data FleetUtilization = FleetUtilization'
    { _fuActiveGameSessionCount    :: !(Maybe Nat)
    , _fuMaximumPlayerSessionCount :: !(Maybe Nat)
    , _fuCurrentPlayerSessionCount :: !(Maybe Nat)
    , _fuFleetId                   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FleetUtilization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fuActiveGameSessionCount'
--
-- * 'fuMaximumPlayerSessionCount'
--
-- * 'fuCurrentPlayerSessionCount'
--
-- * 'fuFleetId'
fleetUtilization
    :: FleetUtilization
fleetUtilization =
    FleetUtilization'
    { _fuActiveGameSessionCount = Nothing
    , _fuMaximumPlayerSessionCount = Nothing
    , _fuCurrentPlayerSessionCount = Nothing
    , _fuFleetId = Nothing
    }

-- | Number of active game sessions currently being hosted on fleet game
-- servers.
fuActiveGameSessionCount :: Lens' FleetUtilization (Maybe Natural)
fuActiveGameSessionCount = lens _fuActiveGameSessionCount (\ s a -> s{_fuActiveGameSessionCount = a}) . mapping _Nat;

-- | Maximum players allowed across all game sessions currently hosted in the
-- fleet.
fuMaximumPlayerSessionCount :: Lens' FleetUtilization (Maybe Natural)
fuMaximumPlayerSessionCount = lens _fuMaximumPlayerSessionCount (\ s a -> s{_fuMaximumPlayerSessionCount = a}) . mapping _Nat;

-- | Number of active player sessions currently being hosted on fleet game
-- servers.
fuCurrentPlayerSessionCount :: Lens' FleetUtilization (Maybe Natural)
fuCurrentPlayerSessionCount = lens _fuCurrentPlayerSessionCount (\ s a -> s{_fuCurrentPlayerSessionCount = a}) . mapping _Nat;

-- | Unique identifier for a fleet.
fuFleetId :: Lens' FleetUtilization (Maybe Text)
fuFleetId = lens _fuFleetId (\ s a -> s{_fuFleetId = a});

instance FromJSON FleetUtilization where
        parseJSON
          = withObject "FleetUtilization"
              (\ x ->
                 FleetUtilization' <$>
                   (x .:? "ActiveGameSessionCount") <*>
                     (x .:? "MaximumPlayerSessionCount")
                     <*> (x .:? "CurrentPlayerSessionCount")
                     <*> (x .:? "FleetId"))

instance Hashable FleetUtilization

instance NFData FleetUtilization

-- | Set of key-value pairs containing information your game server requires
-- to set up sessions. This object allows you to pass in any set of data
-- needed for your game. For more information, see the
-- <http://docs.aws.amazon.com/gamelift/latest/developerguide/ Amazon GameLift Developer Guide>.
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
-- * 'gpKey'
--
-- * 'gpValue'
gameProperty
    :: Text -- ^ 'gpKey'
    -> Text -- ^ 'gpValue'
    -> GameProperty
gameProperty pKey_ pValue_ =
    GameProperty'
    { _gpKey = pKey_
    , _gpValue = pValue_
    }

-- | Undocumented member.
gpKey :: Lens' GameProperty Text
gpKey = lens _gpKey (\ s a -> s{_gpKey = a});

-- | Undocumented member.
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GameSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsCreationTime'
--
-- * 'gsStatus'
--
-- * 'gsGameProperties'
--
-- * 'gsIPAddress'
--
-- * 'gsGameSessionId'
--
-- * 'gsMaximumPlayerSessionCount'
--
-- * 'gsTerminationTime'
--
-- * 'gsPlayerSessionCreationPolicy'
--
-- * 'gsName'
--
-- * 'gsCurrentPlayerSessionCount'
--
-- * 'gsFleetId'
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
    }

-- | Time stamp indicating when this object was created. Format is an integer
-- representing the number of seconds since the Unix epoch (Unix time).
gsCreationTime :: Lens' GameSession (Maybe UTCTime)
gsCreationTime = lens _gsCreationTime (\ s a -> s{_gsCreationTime = a}) . mapping _Time;

-- | Current status of the game session. A game session must be in an ACTIVE
-- state to have player sessions.
gsStatus :: Lens' GameSession (Maybe GameSessionStatus)
gsStatus = lens _gsStatus (\ s a -> s{_gsStatus = a});

-- | Set of custom properties for the game session.
gsGameProperties :: Lens' GameSession [GameProperty]
gsGameProperties = lens _gsGameProperties (\ s a -> s{_gsGameProperties = a}) . _Default . _Coerce;

-- | IP address of the game session.
gsIPAddress :: Lens' GameSession (Maybe Text)
gsIPAddress = lens _gsIPAddress (\ s a -> s{_gsIPAddress = a});

-- | Unique identifier for a game session.
gsGameSessionId :: Lens' GameSession (Maybe Text)
gsGameSessionId = lens _gsGameSessionId (\ s a -> s{_gsGameSessionId = a});

-- | Maximum number of players allowed in the game session.
gsMaximumPlayerSessionCount :: Lens' GameSession (Maybe Natural)
gsMaximumPlayerSessionCount = lens _gsMaximumPlayerSessionCount (\ s a -> s{_gsMaximumPlayerSessionCount = a}) . mapping _Nat;

-- | Time stamp indicating when this fleet was terminated. Format is an
-- integer representing the number of seconds since the Unix epoch (Unix
-- time).
gsTerminationTime :: Lens' GameSession (Maybe UTCTime)
gsTerminationTime = lens _gsTerminationTime (\ s a -> s{_gsTerminationTime = a}) . mapping _Time;

-- | Indicates whether or not the game session is accepting new players.
gsPlayerSessionCreationPolicy :: Lens' GameSession (Maybe PlayerSessionCreationPolicy)
gsPlayerSessionCreationPolicy = lens _gsPlayerSessionCreationPolicy (\ s a -> s{_gsPlayerSessionCreationPolicy = a});

-- | Descriptive label associated with this game session. Session names do
-- not need to be unique.
gsName :: Lens' GameSession (Maybe Text)
gsName = lens _gsName (\ s a -> s{_gsName = a});

-- | Number of players currently in the game session.
gsCurrentPlayerSessionCount :: Lens' GameSession (Maybe Natural)
gsCurrentPlayerSessionCount = lens _gsCurrentPlayerSessionCount (\ s a -> s{_gsCurrentPlayerSessionCount = a}) . mapping _Nat;

-- | Unique identifier for a fleet.
gsFleetId :: Lens' GameSession (Maybe Text)
gsFleetId = lens _gsFleetId (\ s a -> s{_gsFleetId = a});

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
                     <*> (x .:? "FleetId"))

instance Hashable GameSession

instance NFData GameSession

-- | A game session\'s properties and the protection policy currently in
-- force.
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
-- * 'gsdGameSession'
--
-- * 'gsdProtectionPolicy'
gameSessionDetail
    :: GameSessionDetail
gameSessionDetail =
    GameSessionDetail'
    { _gsdGameSession = Nothing
    , _gsdProtectionPolicy = Nothing
    }

-- | Undocumented member.
gsdGameSession :: Lens' GameSessionDetail (Maybe GameSession)
gsdGameSession = lens _gsdGameSession (\ s a -> s{_gsdGameSession = a});

-- | Current status of protection for the game session.
--
-- -   NoProtection: The game session can be terminated during a scale-down
--     event.
-- -   FullProtection: If the game session is in an ACTIVE status, it
--     cannot be terminated during a scale-down event.
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

-- | IP addresses and port settings used to limit access by incoming traffic
-- (players) to a fleet. Permissions specify a range of IP addresses and
-- port settings that must be used to gain access to a game server on a
-- fleet machine.
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
-- * 'ipFromPort'
--
-- * 'ipToPort'
--
-- * 'ipIPRange'
--
-- * 'ipProtocol'
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

-- | Ending value for a range of allowed port numbers. Port numbers are
-- end-inclusive. This value must be higher than /FromPort/.
ipToPort :: Lens' IPPermission Natural
ipToPort = lens _ipToPort (\ s a -> s{_ipToPort = a}) . _Nat;

-- | Range of allowed IP addresses. This value must be expressed in
-- <https://tools.ietf.org/id/cidr CIDR notation>. Example:
-- \"'000.000.000.000\/[subnet mask]'\" or optionally the shortened version
-- \"'0.0.0.0\/[subnet mask]'\".
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

-- | Properties describing a player session.
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
    , _psPlayerId        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PlayerSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psCreationTime'
--
-- * 'psStatus'
--
-- * 'psIPAddress'
--
-- * 'psGameSessionId'
--
-- * 'psTerminationTime'
--
-- * 'psPlayerSessionId'
--
-- * 'psFleetId'
--
-- * 'psPlayerId'
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
    , _psPlayerId = Nothing
    }

-- | Time stamp indicating when this object was created. Format is an integer
-- representing the number of seconds since the Unix epoch (Unix time).
psCreationTime :: Lens' PlayerSession (Maybe UTCTime)
psCreationTime = lens _psCreationTime (\ s a -> s{_psCreationTime = a}) . mapping _Time;

-- | Current status of the player session. Possible player session states
-- include:
--
-- -   RESERVED: The player session request has been received, but the
--     player has not yet connected to the game server and\/or been
--     validated.
-- -   ACTIVE: The player has been validated by the game server and is
--     currently connected.
-- -   COMPLETED: The player connection has been dropped.
-- -   TIMEDOUT: A player session request was received, but the player did
--     not connect and\/or was not validated within the time-out limit (60
--     seconds).
psStatus :: Lens' PlayerSession (Maybe PlayerSessionStatus)
psStatus = lens _psStatus (\ s a -> s{_psStatus = a});

-- | Game session IP address. All player sessions reference the game session
-- location.
psIPAddress :: Lens' PlayerSession (Maybe Text)
psIPAddress = lens _psIPAddress (\ s a -> s{_psIPAddress = a});

-- | Unique identifier for a game session.
psGameSessionId :: Lens' PlayerSession (Maybe Text)
psGameSessionId = lens _psGameSessionId (\ s a -> s{_psGameSessionId = a});

-- | Time stamp indicating when this fleet was terminated. Format is an
-- integer representing the number of seconds since the Unix epoch (Unix
-- time).
psTerminationTime :: Lens' PlayerSession (Maybe UTCTime)
psTerminationTime = lens _psTerminationTime (\ s a -> s{_psTerminationTime = a}) . mapping _Time;

-- | Unique identifier for a player session.
psPlayerSessionId :: Lens' PlayerSession (Maybe Text)
psPlayerSessionId = lens _psPlayerSessionId (\ s a -> s{_psPlayerSessionId = a});

-- | Unique identifier for a fleet.
psFleetId :: Lens' PlayerSession (Maybe Text)
psFleetId = lens _psFleetId (\ s a -> s{_psFleetId = a});

-- | Unique identifier for a player.
psPlayerId :: Lens' PlayerSession (Maybe Text)
psPlayerId = lens _psPlayerId (\ s a -> s{_psPlayerId = a});

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
                     <*> (x .:? "PlayerId"))

instance Hashable PlayerSession

instance NFData PlayerSession

-- | Routing configuration for a fleet alias.
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
-- * 'rsType'
--
-- * 'rsMessage'
--
-- * 'rsFleetId'
routingStrategy
    :: RoutingStrategy
routingStrategy =
    RoutingStrategy'
    { _rsType = Nothing
    , _rsMessage = Nothing
    , _rsFleetId = Nothing
    }

-- | Type of routing strategy. Possible routing types include:
--
-- -   SIMPLE: The alias resolves to one specific fleet. Use this type when
--     routing to active fleets.
-- -   TERMINAL: The alias does not resolve to a fleet but instead can be
--     used to display a message to the user. A terminal alias throws a
--     TerminalRoutingStrategyException with the < RoutingStrategy> message
--     embedded.
rsType :: Lens' RoutingStrategy (Maybe RoutingStrategyType)
rsType = lens _rsType (\ s a -> s{_rsType = a});

-- | Message text to be used with a terminal routing strategy.
rsMessage :: Lens' RoutingStrategy (Maybe Text)
rsMessage = lens _rsMessage (\ s a -> s{_rsMessage = a});

-- | Unique identifier for a fleet.
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

-- | Location in Amazon Simple Storage Service (Amazon S3) where a build\'s
-- files are stored. This location is assigned in response to a
-- < CreateBuild> call, and is always in the same region as the service
-- used to create the build. For more details see the
-- <http://aws.amazon.com/documentation/s3/ Amazon S3 documentation>.
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
-- * 'slBucket'
--
-- * 'slKey'
--
-- * 'slRoleARN'
s3Location
    :: S3Location
s3Location =
    S3Location'
    { _slBucket = Nothing
    , _slKey = Nothing
    , _slRoleARN = Nothing
    }

-- | Amazon S3 bucket identifier.
slBucket :: Lens' S3Location (Maybe Text)
slBucket = lens _slBucket (\ s a -> s{_slBucket = a});

-- | Amazon S3 bucket key.
slKey :: Lens' S3Location (Maybe Text)
slKey = lens _slKey (\ s a -> s{_slKey = a});

-- | Undocumented member.
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

-- | Rule that controls how a fleet is scaled. Scaling policies are uniquely
-- identified by the combination of name and fleet ID.
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
-- * 'spStatus'
--
-- * 'spScalingAdjustmentType'
--
-- * 'spEvaluationPeriods'
--
-- * 'spMetricName'
--
-- * 'spComparisonOperator'
--
-- * 'spName'
--
-- * 'spThreshold'
--
-- * 'spScalingAdjustment'
--
-- * 'spFleetId'
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

-- | Current status of the scaling policy. The scaling policy is only in
-- force when in an Active state.
--
-- -   ACTIVE: The scaling policy is currently in force.
-- -   UPDATEREQUESTED: A request to update the scaling policy has been
--     received.
-- -   UPDATING: A change is being made to the scaling policy.
-- -   DELETEREQUESTED: A request to delete the scaling policy has been
--     received.
-- -   DELETING: The scaling policy is being deleted.
-- -   DELETED: The scaling policy has been deleted.
-- -   ERROR: An error occurred in creating the policy. It should be
--     removed and recreated.
spStatus :: Lens' ScalingPolicy (Maybe ScalingStatusType)
spStatus = lens _spStatus (\ s a -> s{_spStatus = a});

-- | Type of adjustment to make to a fleet\'s instance count (see
-- < FleetCapacity>):
--
-- -   ChangeInCapacity: add (or subtract) the scaling adjustment value
--     from the current instance count. Positive values scale up while
--     negative values scale down.
-- -   ExactCapacity: set the instance count to the scaling adjustment
--     value.
-- -   PercentChangeInCapacity: increase or reduce the current instance
--     count by the scaling adjustment, read as a percentage. Positive
--     values scale up while negative values scale down.
spScalingAdjustmentType :: Lens' ScalingPolicy (Maybe ScalingAdjustmentType)
spScalingAdjustmentType = lens _spScalingAdjustmentType (\ s a -> s{_spScalingAdjustmentType = a});

-- | Length of time (in minutes) the metric must be at or beyond the
-- threshold before a scaling event is triggered.
spEvaluationPeriods :: Lens' ScalingPolicy (Maybe Natural)
spEvaluationPeriods = lens _spEvaluationPeriods (\ s a -> s{_spEvaluationPeriods = a}) . mapping _Nat;

-- | Name of the GameLift-defined metric that is used to trigger an
-- adjustment.
--
-- -   ActivatingGameSessions: number of game sessions in the process of
--     being created (game session status = ACTIVATING).
-- -   ActiveGameSessions: number of game sessions currently running (game
--     session status = ACTIVE).
-- -   CurrentPlayerSessions: number of active or reserved player sessions
--     (player session status = ACTIVE or RESERVED).
-- -   AvailablePlayerSessions: number of player session slots currently
--     available in active game sessions across the fleet, calculated by
--     subtracting a game session\'s current player session count from its
--     maximum player session count. This number does include game sessions
--     that are not currently accepting players (game session
--     PlayerSessionCreationPolicy = DENY_ALL).
-- -   ActiveInstances: number of instances currently running a game
--     session.
-- -   IdleInstances: number of instances not currently running a game
--     session.
spMetricName :: Lens' ScalingPolicy (Maybe MetricName)
spMetricName = lens _spMetricName (\ s a -> s{_spMetricName = a});

-- | Comparison operator to use when measuring a metric against the threshold
-- value.
spComparisonOperator :: Lens' ScalingPolicy (Maybe ComparisonOperatorType)
spComparisonOperator = lens _spComparisonOperator (\ s a -> s{_spComparisonOperator = a});

-- | Descriptive label associated with this scaling policy. Policy names do
-- not need to be unique.
spName :: Lens' ScalingPolicy (Maybe Text)
spName = lens _spName (\ s a -> s{_spName = a});

-- | Metric value used to trigger a scaling event.
spThreshold :: Lens' ScalingPolicy (Maybe Double)
spThreshold = lens _spThreshold (\ s a -> s{_spThreshold = a});

-- | Amount of adjustment to make, based on the scaling adjustment type.
spScalingAdjustment :: Lens' ScalingPolicy (Maybe Int)
spScalingAdjustment = lens _spScalingAdjustment (\ s a -> s{_spScalingAdjustment = a});

-- | Unique identity for the fleet associated with this scaling policy.
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
