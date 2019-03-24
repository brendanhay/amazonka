{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.Product where

import Network.AWS.GameLift.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Temporary access credentials used for uploading game build files to Amazon GameLift. They are valid for a limited time. If they expire before you upload your game build, get a new set by calling 'RequestUploadCredentials' .
--
--
--
-- /See:/ 'awsCredentials' smart constructor.
data AWSCredentials = AWSCredentials'
  { _acSecretAccessKey :: !(Maybe Text)
  , _acSessionToken    :: !(Maybe Text)
  , _acAccessKeyId     :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AWSCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acSecretAccessKey' - Temporary secret key allowing access to the Amazon GameLift S3 account.
--
-- * 'acSessionToken' - Token used to associate a specific build ID with the files uploaded using these credentials.
--
-- * 'acAccessKeyId' - Temporary key allowing access to the Amazon GameLift S3 account.
awsCredentials
    :: AWSCredentials
awsCredentials =
  AWSCredentials'
    { _acSecretAccessKey = Nothing
    , _acSessionToken = Nothing
    , _acAccessKeyId = Nothing
    }


-- | Temporary secret key allowing access to the Amazon GameLift S3 account.
acSecretAccessKey :: Lens' AWSCredentials (Maybe Text)
acSecretAccessKey = lens _acSecretAccessKey (\ s a -> s{_acSecretAccessKey = a})

-- | Token used to associate a specific build ID with the files uploaded using these credentials.
acSessionToken :: Lens' AWSCredentials (Maybe Text)
acSessionToken = lens _acSessionToken (\ s a -> s{_acSessionToken = a})

-- | Temporary key allowing access to the Amazon GameLift S3 account.
acAccessKeyId :: Lens' AWSCredentials (Maybe Text)
acAccessKeyId = lens _acAccessKeyId (\ s a -> s{_acAccessKeyId = a})

instance FromJSON AWSCredentials where
        parseJSON
          = withObject "AWSCredentials"
              (\ x ->
                 AWSCredentials' <$>
                   (x .:? "SecretAccessKey") <*> (x .:? "SessionToken")
                     <*> (x .:? "AccessKeyId"))

instance Hashable AWSCredentials where

instance NFData AWSCredentials where

-- | Properties describing a fleet alias.
--
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
--     * 'ResolveAlias'
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
aCreationTime = lens _aCreationTime (\ s a -> s{_aCreationTime = a}) . mapping _Time

-- | Time stamp indicating when this data object was last modified. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
aLastUpdatedTime :: Lens' Alias (Maybe UTCTime)
aLastUpdatedTime = lens _aLastUpdatedTime (\ s a -> s{_aLastUpdatedTime = a}) . mapping _Time

-- | Unique identifier for an alias; alias IDs are unique within a region.
aAliasId :: Lens' Alias (Maybe Text)
aAliasId = lens _aAliasId (\ s a -> s{_aAliasId = a})

-- | Alias configuration for the alias, including routing type and settings.
aRoutingStrategy :: Lens' Alias (Maybe RoutingStrategy)
aRoutingStrategy = lens _aRoutingStrategy (\ s a -> s{_aRoutingStrategy = a})

-- | Descriptive label that is associated with an alias. Alias names do not need to be unique.
aName :: Lens' Alias (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a})

-- | Unique identifier for an alias; alias ARNs are unique across all regions.
aAliasARN :: Lens' Alias (Maybe Text)
aAliasARN = lens _aAliasARN (\ s a -> s{_aAliasARN = a})

-- | Human-readable description of an alias.
aDescription :: Lens' Alias (Maybe Text)
aDescription = lens _aDescription (\ s a -> s{_aDescription = a})

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

instance Hashable Alias where

instance NFData Alias where

-- | Values for use in 'Player' attribute key:value pairs. This object lets you specify an attribute value using any of the valid data types: string, number, string array or data map. Each @AttributeValue@ object can use only one of the available properties.
--
--
--
-- /See:/ 'attributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { _avSL  :: !(Maybe [Text])
  , _avSDM :: !(Maybe (Map Text Double))
  , _avN   :: !(Maybe Double)
  , _avS   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avSL' - For a list of up to 10 strings. Maximum length for each string is 100 characters. Duplicate values are not recognized; all occurrences of the repeated value after the first of a repeated value are ignored.
--
-- * 'avSDM' - For a map of up to 10 data type:value pairs. Maximum length for each string value is 100 characters.
--
-- * 'avN' - For number values, expressed as double.
--
-- * 'avS' - For single string values. Maximum string length is 100 characters.
attributeValue
    :: AttributeValue
attributeValue =
  AttributeValue'
    {_avSL = Nothing, _avSDM = Nothing, _avN = Nothing, _avS = Nothing}


-- | For a list of up to 10 strings. Maximum length for each string is 100 characters. Duplicate values are not recognized; all occurrences of the repeated value after the first of a repeated value are ignored.
avSL :: Lens' AttributeValue [Text]
avSL = lens _avSL (\ s a -> s{_avSL = a}) . _Default . _Coerce

-- | For a map of up to 10 data type:value pairs. Maximum length for each string value is 100 characters.
avSDM :: Lens' AttributeValue (HashMap Text Double)
avSDM = lens _avSDM (\ s a -> s{_avSDM = a}) . _Default . _Map

-- | For number values, expressed as double.
avN :: Lens' AttributeValue (Maybe Double)
avN = lens _avN (\ s a -> s{_avN = a})

-- | For single string values. Maximum string length is 100 characters.
avS :: Lens' AttributeValue (Maybe Text)
avS = lens _avS (\ s a -> s{_avS = a})

instance FromJSON AttributeValue where
        parseJSON
          = withObject "AttributeValue"
              (\ x ->
                 AttributeValue' <$>
                   (x .:? "SL" .!= mempty) <*> (x .:? "SDM" .!= mempty)
                     <*> (x .:? "N")
                     <*> (x .:? "S"))

instance Hashable AttributeValue where

instance NFData AttributeValue where

instance ToJSON AttributeValue where
        toJSON AttributeValue'{..}
          = object
              (catMaybes
                 [("SL" .=) <$> _avSL, ("SDM" .=) <$> _avSDM,
                  ("N" .=) <$> _avN, ("S" .=) <$> _avS])

-- | Properties describing a game build.
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Build' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'bStatus' - Current status of the build. Possible build statuses include the following:     * __INITIALIZED__ -- A new build has been defined, but no files have been uploaded. You cannot create fleets for builds that are in this status. When a build is successfully created, the build status is set to this value.      * __READY__ -- The game build has been successfully uploaded. You can now create new fleets for this build.     * __FAILED__ -- The game build upload failed. You cannot create new fleets for this build.
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
bCreationTime = lens _bCreationTime (\ s a -> s{_bCreationTime = a}) . mapping _Time

-- | Current status of the build. Possible build statuses include the following:     * __INITIALIZED__ -- A new build has been defined, but no files have been uploaded. You cannot create fleets for builds that are in this status. When a build is successfully created, the build status is set to this value.      * __READY__ -- The game build has been successfully uploaded. You can now create new fleets for this build.     * __FAILED__ -- The game build upload failed. You cannot create new fleets for this build.
bStatus :: Lens' Build (Maybe BuildStatus)
bStatus = lens _bStatus (\ s a -> s{_bStatus = a})

-- | Operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build.
bOperatingSystem :: Lens' Build (Maybe OperatingSystem)
bOperatingSystem = lens _bOperatingSystem (\ s a -> s{_bOperatingSystem = a})

-- | Unique identifier for a build.
bBuildId :: Lens' Build (Maybe Text)
bBuildId = lens _bBuildId (\ s a -> s{_bBuildId = a})

-- | Descriptive label that is associated with a build. Build names do not need to be unique. It can be set using 'CreateBuild' or 'UpdateBuild' .
bName :: Lens' Build (Maybe Text)
bName = lens _bName (\ s a -> s{_bName = a})

-- | Version that is associated with this build. Version strings do not need to be unique. This value can be set using 'CreateBuild' or 'UpdateBuild' .
bVersion :: Lens' Build (Maybe Text)
bVersion = lens _bVersion (\ s a -> s{_bVersion = a})

-- | File size of the uploaded game build, expressed in bytes. When the build status is @INITIALIZED@ , this value is 0.
bSizeOnDisk :: Lens' Build (Maybe Natural)
bSizeOnDisk = lens _bSizeOnDisk (\ s a -> s{_bSizeOnDisk = a}) . mapping _Nat

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

instance Hashable Build where

instance NFData Build where

-- | Player information for use when creating player sessions using a game session placement request with 'StartGameSessionPlacement' .
--
--
--
-- /See:/ 'desiredPlayerSession' smart constructor.
data DesiredPlayerSession = DesiredPlayerSession'
  { _dpsPlayerData :: !(Maybe Text)
  , _dpsPlayerId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
  DesiredPlayerSession' {_dpsPlayerData = Nothing, _dpsPlayerId = Nothing}


-- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
dpsPlayerData :: Lens' DesiredPlayerSession (Maybe Text)
dpsPlayerData = lens _dpsPlayerData (\ s a -> s{_dpsPlayerData = a})

-- | Unique identifier for a player to associate with the player session.
dpsPlayerId :: Lens' DesiredPlayerSession (Maybe Text)
dpsPlayerId = lens _dpsPlayerId (\ s a -> s{_dpsPlayerId = a})

instance Hashable DesiredPlayerSession where

instance NFData DesiredPlayerSession where

instance ToJSON DesiredPlayerSession where
        toJSON DesiredPlayerSession'{..}
          = object
              (catMaybes
                 [("PlayerData" .=) <$> _dpsPlayerData,
                  ("PlayerId" .=) <$> _dpsPlayerId])

-- | Current status of fleet capacity. The number of active instances should match or be in the process of matching the number of desired instances. Pending and terminating counts are non-zero only if fleet capacity is adjusting to an 'UpdateFleetCapacity' request, or if access to resources is temporarily affected.
--
--
--     * 'CreateFleet'
--
--     * 'ListFleets'
--
--     * 'DeleteFleet'
--
--     * Describe fleets:
--
--     * 'DescribeFleetAttributes'
--
--     * 'DescribeFleetCapacity'
--
--     * 'DescribeFleetPortSettings'
--
--     * 'DescribeFleetUtilization'
--
--     * 'DescribeRuntimeConfiguration'
--
--     * 'DescribeEC2InstanceLimits'
--
--     * 'DescribeFleetEvents'
--
--
--
--     * Update fleets:
--
--     * 'UpdateFleetAttributes'
--
--     * 'UpdateFleetCapacity'
--
--     * 'UpdateFleetPortSettings'
--
--     * 'UpdateRuntimeConfiguration'
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions'
--
--     * 'StopFleetActions'
--
--
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
eicIdLE = lens _eicIdLE (\ s a -> s{_eicIdLE = a}) . mapping _Nat

-- | Number of instances in the fleet that are no longer active but haven't yet been terminated.
eicTERMINATING :: Lens' EC2InstanceCounts (Maybe Natural)
eicTERMINATING = lens _eicTERMINATING (\ s a -> s{_eicTERMINATING = a}) . mapping _Nat

-- | Number of instances in the fleet that are starting but not yet active.
eicPENDING :: Lens' EC2InstanceCounts (Maybe Natural)
eicPENDING = lens _eicPENDING (\ s a -> s{_eicPENDING = a}) . mapping _Nat

-- | Maximum value allowed for the fleet's instance count.
eicMAXIMUM :: Lens' EC2InstanceCounts (Maybe Natural)
eicMAXIMUM = lens _eicMAXIMUM (\ s a -> s{_eicMAXIMUM = a}) . mapping _Nat

-- | Ideal number of active instances in the fleet.
eicDESIRED :: Lens' EC2InstanceCounts (Maybe Natural)
eicDESIRED = lens _eicDESIRED (\ s a -> s{_eicDESIRED = a}) . mapping _Nat

-- | Minimum value allowed for the fleet's instance count.
eicMINIMUM :: Lens' EC2InstanceCounts (Maybe Natural)
eicMINIMUM = lens _eicMINIMUM (\ s a -> s{_eicMINIMUM = a}) . mapping _Nat

-- | Actual number of active instances in the fleet.
eicACTIVE :: Lens' EC2InstanceCounts (Maybe Natural)
eicACTIVE = lens _eicACTIVE (\ s a -> s{_eicACTIVE = a}) . mapping _Nat

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

instance Hashable EC2InstanceCounts where

instance NFData EC2InstanceCounts where

-- | Maximum number of instances allowed based on the Amazon Elastic Compute Cloud (Amazon EC2) instance type. Instance limits can be retrieved by calling 'DescribeEC2InstanceLimits' .
--
--
--
-- /See:/ 'ec2InstanceLimit' smart constructor.
data EC2InstanceLimit = EC2InstanceLimit'
  { _eilEC2InstanceType  :: !(Maybe EC2InstanceType)
  , _eilCurrentInstances :: !(Maybe Nat)
  , _eilInstanceLimit    :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
eilEC2InstanceType = lens _eilEC2InstanceType (\ s a -> s{_eilEC2InstanceType = a})

-- | Number of instances of the specified type that are currently in use by this AWS account.
eilCurrentInstances :: Lens' EC2InstanceLimit (Maybe Natural)
eilCurrentInstances = lens _eilCurrentInstances (\ s a -> s{_eilCurrentInstances = a}) . mapping _Nat

-- | Number of instances allowed.
eilInstanceLimit :: Lens' EC2InstanceLimit (Maybe Natural)
eilInstanceLimit = lens _eilInstanceLimit (\ s a -> s{_eilInstanceLimit = a}) . mapping _Nat

instance FromJSON EC2InstanceLimit where
        parseJSON
          = withObject "EC2InstanceLimit"
              (\ x ->
                 EC2InstanceLimit' <$>
                   (x .:? "EC2InstanceType") <*>
                     (x .:? "CurrentInstances")
                     <*> (x .:? "InstanceLimit"))

instance Hashable EC2InstanceLimit where

instance NFData EC2InstanceLimit where

-- | Log entry describing an event that involves Amazon GameLift resources (such as a fleet). In addition to tracking activity, event codes and messages can provide additional information for troubleshooting and debugging problems.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
  { _eResourceId      :: !(Maybe Text)
  , _ePreSignedLogURL :: !(Maybe Text)
  , _eEventTime       :: !(Maybe POSIX)
  , _eMessage         :: !(Maybe Text)
  , _eEventCode       :: !(Maybe EventCode)
  , _eEventId         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eResourceId' - Unique identifier for an event resource, such as a fleet ID.
--
-- * 'ePreSignedLogURL' - Location of stored logs with additional detail that is related to the event. This is useful for debugging issues. The URL is valid for 15 minutes. You can also access fleet creation logs through the Amazon GameLift console.
--
-- * 'eEventTime' - Time stamp indicating when this event occurred. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'eMessage' - Additional information related to the event.
--
-- * 'eEventCode' - Type of event being logged. The following events are currently in use: __Fleet creation events:__      * FLEET_CREATED -- A fleet record was successfully created with a status of @NEW@ . Event messaging includes the fleet ID.     * FLEET_STATE_DOWNLOADING -- Fleet status changed from @NEW@ to @DOWNLOADING@ . The compressed build has started downloading to a fleet instance for installation.     * FLEET_BINARY_DOWNLOAD_FAILED -- The build failed to download to the fleet instance.     * FLEET_CREATION_EXTRACTING_BUILD
