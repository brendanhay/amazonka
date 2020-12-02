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
-- Fleet-related operations include:
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
-- * 'eEventCode' - Type of event being logged. The following events are currently in use: __Fleet creation events:__      * FLEET_CREATED -- A fleet record was successfully created with a status of @NEW@ . Event messaging includes the fleet ID.     * FLEET_STATE_DOWNLOADING -- Fleet status changed from @NEW@ to @DOWNLOADING@ . The compressed build has started downloading to a fleet instance for installation.     * FLEET_BINARY_DOWNLOAD_FAILED -- The build failed to download to the fleet instance.     * FLEET_CREATION_EXTRACTING_BUILD – The game server build was successfully downloaded to an instance, and the build files are now being extracted from the uploaded build and saved to an instance. Failure at this stage prevents a fleet from moving to @ACTIVE@ status. Logs for this stage display a list of the files that are extracted and saved on the instance. Access the logs by using the URL in /PreSignedLogUrl/ .     * FLEET_CREATION_RUNNING_INSTALLER – The game server build files were successfully extracted, and the Amazon GameLift is now running the build's install script (if one is included). Failure in this stage prevents a fleet from moving to @ACTIVE@ status. Logs for this stage list the installation steps and whether or not the install completed successfully. Access the logs by using the URL in /PreSignedLogUrl/ .      * FLEET_CREATION_VALIDATING_RUNTIME_CONFIG -- The build process was successful, and the Amazon GameLift is now verifying that the game server launch paths, which are specified in the fleet's run-time configuration, exist. If any listed launch path exists, Amazon GameLift tries to launch a game server process and waits for the process to report ready. Failures in this stage prevent a fleet from moving to @ACTIVE@ status. Logs for this stage list the launch paths in the run-time configuration and indicate whether each is found. Access the logs by using the URL in /PreSignedLogUrl/ .      * FLEET_STATE_VALIDATING -- Fleet status changed from @DOWNLOADING@ to @VALIDATING@ .     * FLEET_VALIDATION_LAUNCH_PATH_NOT_FOUND -- Validation of the run-time configuration failed because the executable specified in a launch path does not exist on the instance.     * FLEET_STATE_BUILDING -- Fleet status changed from @VALIDATING@ to @BUILDING@ .     * FLEET_VALIDATION_EXECUTABLE_RUNTIME_FAILURE -- Validation of the run-time configuration failed because the executable specified in a launch path failed to run on the fleet instance.     * FLEET_STATE_ACTIVATING -- Fleet status changed from @BUILDING@ to @ACTIVATING@ .      * FLEET_ACTIVATION_FAILED - The fleet failed to successfully complete one of the steps in the fleet activation process. This event code indicates that the game build was successfully downloaded to a fleet instance, built, and validated, but was not able to start a server process. A possible reason for failure is that the game server is not reporting "process ready" to the Amazon GameLift service.     * FLEET_STATE_ACTIVE -- The fleet's status changed from @ACTIVATING@ to @ACTIVE@ . The fleet is now ready to host game sessions. __VPC peering events:__      * FLEET_VPC_PEERING_SUCCEEDED -- A VPC peering connection has been established between the VPC for an Amazon GameLift fleet and a VPC in your AWS account.     * FLEET_VPC_PEERING_FAILED -- A requested VPC peering connection has failed. Event details and status information (see 'DescribeVpcPeeringConnections' ) provide additional detail. A common reason for peering failure is that the two VPCs have overlapping CIDR blocks of IPv4 addresses. To resolve this, change the CIDR block for the VPC in your AWS account. For more information on VPC peering failures, see <http://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html http://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html>      * FLEET_VPC_PEERING_DELETED -- A VPC peering connection has been successfully deleted. __Spot instance events:__      * INSTANCE_INTERRUPTED -- A spot instance was interrupted by EC2 with a two-minute notification. __Other fleet events:__      * FLEET_SCALING_EVENT -- A change was made to the fleet's capacity settings (desired instances, minimum/maximum scaling limits). Event messaging includes the new capacity settings.     * FLEET_NEW_GAME_SESSION_PROTECTION_POLICY_UPDATED -- A change was made to the fleet's game session protection policy setting. Event messaging includes both the old and new policy setting.      * FLEET_DELETED -- A request to delete a fleet was initiated.     * GENERIC_EVENT -- An unspecified event has occurred.
--
-- * 'eEventId' - Unique identifier for a fleet event.
event
    :: Event
event =
  Event'
    { _eResourceId = Nothing
    , _ePreSignedLogURL = Nothing
    , _eEventTime = Nothing
    , _eMessage = Nothing
    , _eEventCode = Nothing
    , _eEventId = Nothing
    }


-- | Unique identifier for an event resource, such as a fleet ID.
eResourceId :: Lens' Event (Maybe Text)
eResourceId = lens _eResourceId (\ s a -> s{_eResourceId = a})

-- | Location of stored logs with additional detail that is related to the event. This is useful for debugging issues. The URL is valid for 15 minutes. You can also access fleet creation logs through the Amazon GameLift console.
ePreSignedLogURL :: Lens' Event (Maybe Text)
ePreSignedLogURL = lens _ePreSignedLogURL (\ s a -> s{_ePreSignedLogURL = a})

-- | Time stamp indicating when this event occurred. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
eEventTime :: Lens' Event (Maybe UTCTime)
eEventTime = lens _eEventTime (\ s a -> s{_eEventTime = a}) . mapping _Time

-- | Additional information related to the event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\ s a -> s{_eMessage = a})

-- | Type of event being logged. The following events are currently in use: __Fleet creation events:__      * FLEET_CREATED -- A fleet record was successfully created with a status of @NEW@ . Event messaging includes the fleet ID.     * FLEET_STATE_DOWNLOADING -- Fleet status changed from @NEW@ to @DOWNLOADING@ . The compressed build has started downloading to a fleet instance for installation.     * FLEET_BINARY_DOWNLOAD_FAILED -- The build failed to download to the fleet instance.     * FLEET_CREATION_EXTRACTING_BUILD – The game server build was successfully downloaded to an instance, and the build files are now being extracted from the uploaded build and saved to an instance. Failure at this stage prevents a fleet from moving to @ACTIVE@ status. Logs for this stage display a list of the files that are extracted and saved on the instance. Access the logs by using the URL in /PreSignedLogUrl/ .     * FLEET_CREATION_RUNNING_INSTALLER – The game server build files were successfully extracted, and the Amazon GameLift is now running the build's install script (if one is included). Failure in this stage prevents a fleet from moving to @ACTIVE@ status. Logs for this stage list the installation steps and whether or not the install completed successfully. Access the logs by using the URL in /PreSignedLogUrl/ .      * FLEET_CREATION_VALIDATING_RUNTIME_CONFIG -- The build process was successful, and the Amazon GameLift is now verifying that the game server launch paths, which are specified in the fleet's run-time configuration, exist. If any listed launch path exists, Amazon GameLift tries to launch a game server process and waits for the process to report ready. Failures in this stage prevent a fleet from moving to @ACTIVE@ status. Logs for this stage list the launch paths in the run-time configuration and indicate whether each is found. Access the logs by using the URL in /PreSignedLogUrl/ .      * FLEET_STATE_VALIDATING -- Fleet status changed from @DOWNLOADING@ to @VALIDATING@ .     * FLEET_VALIDATION_LAUNCH_PATH_NOT_FOUND -- Validation of the run-time configuration failed because the executable specified in a launch path does not exist on the instance.     * FLEET_STATE_BUILDING -- Fleet status changed from @VALIDATING@ to @BUILDING@ .     * FLEET_VALIDATION_EXECUTABLE_RUNTIME_FAILURE -- Validation of the run-time configuration failed because the executable specified in a launch path failed to run on the fleet instance.     * FLEET_STATE_ACTIVATING -- Fleet status changed from @BUILDING@ to @ACTIVATING@ .      * FLEET_ACTIVATION_FAILED - The fleet failed to successfully complete one of the steps in the fleet activation process. This event code indicates that the game build was successfully downloaded to a fleet instance, built, and validated, but was not able to start a server process. A possible reason for failure is that the game server is not reporting "process ready" to the Amazon GameLift service.     * FLEET_STATE_ACTIVE -- The fleet's status changed from @ACTIVATING@ to @ACTIVE@ . The fleet is now ready to host game sessions. __VPC peering events:__      * FLEET_VPC_PEERING_SUCCEEDED -- A VPC peering connection has been established between the VPC for an Amazon GameLift fleet and a VPC in your AWS account.     * FLEET_VPC_PEERING_FAILED -- A requested VPC peering connection has failed. Event details and status information (see 'DescribeVpcPeeringConnections' ) provide additional detail. A common reason for peering failure is that the two VPCs have overlapping CIDR blocks of IPv4 addresses. To resolve this, change the CIDR block for the VPC in your AWS account. For more information on VPC peering failures, see <http://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html http://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/invalid-peering-configurations.html>      * FLEET_VPC_PEERING_DELETED -- A VPC peering connection has been successfully deleted. __Spot instance events:__      * INSTANCE_INTERRUPTED -- A spot instance was interrupted by EC2 with a two-minute notification. __Other fleet events:__      * FLEET_SCALING_EVENT -- A change was made to the fleet's capacity settings (desired instances, minimum/maximum scaling limits). Event messaging includes the new capacity settings.     * FLEET_NEW_GAME_SESSION_PROTECTION_POLICY_UPDATED -- A change was made to the fleet's game session protection policy setting. Event messaging includes both the old and new policy setting.      * FLEET_DELETED -- A request to delete a fleet was initiated.     * GENERIC_EVENT -- An unspecified event has occurred.
eEventCode :: Lens' Event (Maybe EventCode)
eEventCode = lens _eEventCode (\ s a -> s{_eEventCode = a})

-- | Unique identifier for a fleet event.
eEventId :: Lens' Event (Maybe Text)
eEventId = lens _eEventId (\ s a -> s{_eEventId = a})

instance FromJSON Event where
        parseJSON
          = withObject "Event"
              (\ x ->
                 Event' <$>
                   (x .:? "ResourceId") <*> (x .:? "PreSignedLogUrl")
                     <*> (x .:? "EventTime")
                     <*> (x .:? "Message")
                     <*> (x .:? "EventCode")
                     <*> (x .:? "EventId"))

instance Hashable Event where

instance NFData Event where

-- | General properties describing a fleet.
--
--
-- Fleet-related operations include:
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
-- /See:/ 'fleetAttributes' smart constructor.
data FleetAttributes = FleetAttributes'
  { _faCreationTime                   :: !(Maybe POSIX)
  , _faStatus                         :: !(Maybe FleetStatus)
  , _faServerLaunchParameters         :: !(Maybe Text)
  , _faLogPaths                       :: !(Maybe [Text])
  , _faOperatingSystem                :: !(Maybe OperatingSystem)
  , _faBuildId                        :: !(Maybe Text)
  , _faFleetARN                       :: !(Maybe Text)
  , _faFleetType                      :: !(Maybe FleetType)
  , _faTerminationTime                :: !(Maybe POSIX)
  , _faInstanceType                   :: !(Maybe EC2InstanceType)
  , _faStoppedActions                 :: !(Maybe (List1 FleetAction))
  , _faNewGameSessionProtectionPolicy :: !(Maybe ProtectionPolicy)
  , _faName                           :: !(Maybe Text)
  , _faServerLaunchPath               :: !(Maybe Text)
  , _faMetricGroups                   :: !(Maybe [Text])
  , _faFleetId                        :: !(Maybe Text)
  , _faDescription                    :: !(Maybe Text)
  , _faResourceCreationLimitPolicy    :: !(Maybe ResourceCreationLimitPolicy)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FleetAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'faStatus' - Current status of the fleet. Possible fleet statuses include the following:     * __NEW__ -- A new fleet has been defined and desired instances is set to 1.      * __DOWNLOADING/VALIDATING/BUILDING/ACTIVATING__ -- Amazon GameLift is setting up the new fleet, creating new instances with the game build and starting server processes.     * __ACTIVE__ -- Hosts can now accept game sessions.     * __ERROR__ -- An error occurred when downloading, validating, building, or activating the fleet.     * __DELETING__ -- Hosts are responding to a delete fleet request.     * __TERMINATED__ -- The fleet no longer exists.
--
-- * 'faServerLaunchParameters' - Game server launch parameters specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
--
-- * 'faLogPaths' - Location of default log files. When a server process is shut down, Amazon GameLift captures and stores any log files in this location. These logs are in addition to game session logs; see more on game session logs in the <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide> . If no default log path for a fleet is specified, Amazon GameLift automatically uploads logs that are stored on each instance at @C:\game\logs@ (for Windows) or @/local/game/logs@ (for Linux). Use the Amazon GameLift console to access stored logs.
--
-- * 'faOperatingSystem' - Operating system of the fleet's computing resources. A fleet's operating system depends on the OS specified for the build that is deployed on this fleet.
--
-- * 'faBuildId' - Unique identifier for a build.
--
-- * 'faFleetARN' - Identifier for a fleet that is unique across all regions.
--
-- * 'faFleetType' - Indicates whether the fleet uses on-demand or spot instances. A spot instance in use may be interrupted with a two-minute notification.
--
-- * 'faTerminationTime' - Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'faInstanceType' - EC2 instance type indicating the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- * 'faStoppedActions' - List of fleet actions that have been suspended using 'StopFleetActions' . This includes auto-scaling.
--
-- * 'faNewGameSessionProtectionPolicy' - Type of game session protection to set for all new instances started in the fleet.     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
-- * 'faName' - Descriptive label that is associated with a fleet. Fleet names do not need to be unique.
--
-- * 'faServerLaunchPath' - Path to a game server executable in the fleet's build, specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch paths for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
--
-- * 'faMetricGroups' - Names of metric groups that this fleet is included in. In Amazon CloudWatch, you can view metrics for an individual fleet or aggregated metrics for fleets that are in a fleet metric group. A fleet can be included in only one metric group at a time.
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
    , _faFleetType = Nothing
    , _faTerminationTime = Nothing
    , _faInstanceType = Nothing
    , _faStoppedActions = Nothing
    , _faNewGameSessionProtectionPolicy = Nothing
    , _faName = Nothing
    , _faServerLaunchPath = Nothing
    , _faMetricGroups = Nothing
    , _faFleetId = Nothing
    , _faDescription = Nothing
    , _faResourceCreationLimitPolicy = Nothing
    }


-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
faCreationTime :: Lens' FleetAttributes (Maybe UTCTime)
faCreationTime = lens _faCreationTime (\ s a -> s{_faCreationTime = a}) . mapping _Time

-- | Current status of the fleet. Possible fleet statuses include the following:     * __NEW__ -- A new fleet has been defined and desired instances is set to 1.      * __DOWNLOADING/VALIDATING/BUILDING/ACTIVATING__ -- Amazon GameLift is setting up the new fleet, creating new instances with the game build and starting server processes.     * __ACTIVE__ -- Hosts can now accept game sessions.     * __ERROR__ -- An error occurred when downloading, validating, building, or activating the fleet.     * __DELETING__ -- Hosts are responding to a delete fleet request.     * __TERMINATED__ -- The fleet no longer exists.
faStatus :: Lens' FleetAttributes (Maybe FleetStatus)
faStatus = lens _faStatus (\ s a -> s{_faStatus = a})

-- | Game server launch parameters specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
faServerLaunchParameters :: Lens' FleetAttributes (Maybe Text)
faServerLaunchParameters = lens _faServerLaunchParameters (\ s a -> s{_faServerLaunchParameters = a})

-- | Location of default log files. When a server process is shut down, Amazon GameLift captures and stores any log files in this location. These logs are in addition to game session logs; see more on game session logs in the <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide> . If no default log path for a fleet is specified, Amazon GameLift automatically uploads logs that are stored on each instance at @C:\game\logs@ (for Windows) or @/local/game/logs@ (for Linux). Use the Amazon GameLift console to access stored logs.
faLogPaths :: Lens' FleetAttributes [Text]
faLogPaths = lens _faLogPaths (\ s a -> s{_faLogPaths = a}) . _Default . _Coerce

-- | Operating system of the fleet's computing resources. A fleet's operating system depends on the OS specified for the build that is deployed on this fleet.
faOperatingSystem :: Lens' FleetAttributes (Maybe OperatingSystem)
faOperatingSystem = lens _faOperatingSystem (\ s a -> s{_faOperatingSystem = a})

-- | Unique identifier for a build.
faBuildId :: Lens' FleetAttributes (Maybe Text)
faBuildId = lens _faBuildId (\ s a -> s{_faBuildId = a})

-- | Identifier for a fleet that is unique across all regions.
faFleetARN :: Lens' FleetAttributes (Maybe Text)
faFleetARN = lens _faFleetARN (\ s a -> s{_faFleetARN = a})

-- | Indicates whether the fleet uses on-demand or spot instances. A spot instance in use may be interrupted with a two-minute notification.
faFleetType :: Lens' FleetAttributes (Maybe FleetType)
faFleetType = lens _faFleetType (\ s a -> s{_faFleetType = a})

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
faTerminationTime :: Lens' FleetAttributes (Maybe UTCTime)
faTerminationTime = lens _faTerminationTime (\ s a -> s{_faTerminationTime = a}) . mapping _Time

-- | EC2 instance type indicating the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
faInstanceType :: Lens' FleetAttributes (Maybe EC2InstanceType)
faInstanceType = lens _faInstanceType (\ s a -> s{_faInstanceType = a})

-- | List of fleet actions that have been suspended using 'StopFleetActions' . This includes auto-scaling.
faStoppedActions :: Lens' FleetAttributes (Maybe (NonEmpty FleetAction))
faStoppedActions = lens _faStoppedActions (\ s a -> s{_faStoppedActions = a}) . mapping _List1

-- | Type of game session protection to set for all new instances started in the fleet.     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
faNewGameSessionProtectionPolicy :: Lens' FleetAttributes (Maybe ProtectionPolicy)
faNewGameSessionProtectionPolicy = lens _faNewGameSessionProtectionPolicy (\ s a -> s{_faNewGameSessionProtectionPolicy = a})

-- | Descriptive label that is associated with a fleet. Fleet names do not need to be unique.
faName :: Lens' FleetAttributes (Maybe Text)
faName = lens _faName (\ s a -> s{_faName = a})

-- | Path to a game server executable in the fleet's build, specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch paths for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
faServerLaunchPath :: Lens' FleetAttributes (Maybe Text)
faServerLaunchPath = lens _faServerLaunchPath (\ s a -> s{_faServerLaunchPath = a})

-- | Names of metric groups that this fleet is included in. In Amazon CloudWatch, you can view metrics for an individual fleet or aggregated metrics for fleets that are in a fleet metric group. A fleet can be included in only one metric group at a time.
faMetricGroups :: Lens' FleetAttributes [Text]
faMetricGroups = lens _faMetricGroups (\ s a -> s{_faMetricGroups = a}) . _Default . _Coerce

-- | Unique identifier for a fleet.
faFleetId :: Lens' FleetAttributes (Maybe Text)
faFleetId = lens _faFleetId (\ s a -> s{_faFleetId = a})

-- | Human-readable description of the fleet.
faDescription :: Lens' FleetAttributes (Maybe Text)
faDescription = lens _faDescription (\ s a -> s{_faDescription = a})

-- | Fleet policy to limit the number of game sessions an individual player can create over a span of time.
faResourceCreationLimitPolicy :: Lens' FleetAttributes (Maybe ResourceCreationLimitPolicy)
faResourceCreationLimitPolicy = lens _faResourceCreationLimitPolicy (\ s a -> s{_faResourceCreationLimitPolicy = a})

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
                     <*> (x .:? "FleetType")
                     <*> (x .:? "TerminationTime")
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "StoppedActions")
                     <*> (x .:? "NewGameSessionProtectionPolicy")
                     <*> (x .:? "Name")
                     <*> (x .:? "ServerLaunchPath")
                     <*> (x .:? "MetricGroups" .!= mempty)
                     <*> (x .:? "FleetId")
                     <*> (x .:? "Description")
                     <*> (x .:? "ResourceCreationLimitPolicy"))

instance Hashable FleetAttributes where

instance NFData FleetAttributes where

-- | Information about the fleet's capacity. Fleet capacity is measured in EC2 instances. By default, new fleets have a capacity of one instance, but can be updated as needed. The maximum number of instances for a fleet is determined by the fleet's instance type.
--
--
-- Fleet-related operations include:
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
-- /See:/ 'fleetCapacity' smart constructor.
data FleetCapacity = FleetCapacity'
  { _fcInstanceType   :: !(Maybe EC2InstanceType)
  , _fcFleetId        :: !(Maybe Text)
  , _fcInstanceCounts :: !(Maybe EC2InstanceCounts)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
fcInstanceType = lens _fcInstanceType (\ s a -> s{_fcInstanceType = a})

-- | Unique identifier for a fleet.
fcFleetId :: Lens' FleetCapacity (Maybe Text)
fcFleetId = lens _fcFleetId (\ s a -> s{_fcFleetId = a})

-- | Current status of fleet capacity.
fcInstanceCounts :: Lens' FleetCapacity (Maybe EC2InstanceCounts)
fcInstanceCounts = lens _fcInstanceCounts (\ s a -> s{_fcInstanceCounts = a})

instance FromJSON FleetCapacity where
        parseJSON
          = withObject "FleetCapacity"
              (\ x ->
                 FleetCapacity' <$>
                   (x .:? "InstanceType") <*> (x .:? "FleetId") <*>
                     (x .:? "InstanceCounts"))

instance Hashable FleetCapacity where

instance NFData FleetCapacity where

-- | Current status of fleet utilization, including the number of game and player sessions being hosted.
--
--
-- Fleet-related operations include:
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
-- /See:/ 'fleetUtilization' smart constructor.
data FleetUtilization = FleetUtilization'
  { _fuActiveGameSessionCount    :: !(Maybe Nat)
  , _fuMaximumPlayerSessionCount :: !(Maybe Nat)
  , _fuCurrentPlayerSessionCount :: !(Maybe Nat)
  , _fuFleetId                   :: !(Maybe Text)
  , _fuActiveServerProcessCount  :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
fuActiveGameSessionCount = lens _fuActiveGameSessionCount (\ s a -> s{_fuActiveGameSessionCount = a}) . mapping _Nat

-- | Maximum players allowed across all game sessions currently being hosted on all instances in the fleet.
fuMaximumPlayerSessionCount :: Lens' FleetUtilization (Maybe Natural)
fuMaximumPlayerSessionCount = lens _fuMaximumPlayerSessionCount (\ s a -> s{_fuMaximumPlayerSessionCount = a}) . mapping _Nat

-- | Number of active player sessions currently being hosted on all instances in the fleet.
fuCurrentPlayerSessionCount :: Lens' FleetUtilization (Maybe Natural)
fuCurrentPlayerSessionCount = lens _fuCurrentPlayerSessionCount (\ s a -> s{_fuCurrentPlayerSessionCount = a}) . mapping _Nat

-- | Unique identifier for a fleet.
fuFleetId :: Lens' FleetUtilization (Maybe Text)
fuFleetId = lens _fuFleetId (\ s a -> s{_fuFleetId = a})

-- | Number of server processes in an @ACTIVE@ status currently running across all instances in the fleet
fuActiveServerProcessCount :: Lens' FleetUtilization (Maybe Natural)
fuActiveServerProcessCount = lens _fuActiveServerProcessCount (\ s a -> s{_fuActiveServerProcessCount = a}) . mapping _Nat

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

instance Hashable FleetUtilization where

instance NFData FleetUtilization where

-- | Set of key-value pairs that contain information about a game session. When included in a game session request, these properties communicate details to be used when setting up the new game session, such as to specify a game mode, level, or map. Game properties are passed to the game server process when initiating a new game session; the server process uses the properties as appropriate. For more information, see the <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-client-api.html#gamelift-sdk-client-api-create Amazon GameLift Developer Guide> .
--
--
--
-- /See:/ 'gameProperty' smart constructor.
data GameProperty = GameProperty'
  { _gpKey   :: !Text
  , _gpValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GameProperty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpKey' - Game property identifier.
--
-- * 'gpValue' - Game property value.
gameProperty
    :: Text -- ^ 'gpKey'
    -> Text -- ^ 'gpValue'
    -> GameProperty
gameProperty pKey_ pValue_ = GameProperty' {_gpKey = pKey_, _gpValue = pValue_}


-- | Game property identifier.
gpKey :: Lens' GameProperty Text
gpKey = lens _gpKey (\ s a -> s{_gpKey = a})

-- | Game property value.
gpValue :: Lens' GameProperty Text
gpValue = lens _gpValue (\ s a -> s{_gpValue = a})

instance FromJSON GameProperty where
        parseJSON
          = withObject "GameProperty"
              (\ x ->
                 GameProperty' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable GameProperty where

instance NFData GameProperty where

instance ToJSON GameProperty where
        toJSON GameProperty'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _gpKey), Just ("Value" .= _gpValue)])

-- | Properties describing a game session.
--
--
-- A game session in ACTIVE status can host players. When a game session ends, its status is set to @TERMINATED@ .
--
-- Once the session ends, the game session object is retained for 30 days. This means you can reuse idempotency token values after this time. Game session logs are retained for 14 days.
--
-- Game-session-related operations include:
--
--     * 'CreateGameSession'
--
--     * 'DescribeGameSessions'
--
--     * 'DescribeGameSessionDetails'
--
--     * 'SearchGameSessions'
--
--     * 'UpdateGameSession'
--
--     * 'GetGameSessionLogUrl'
--
--     * Game session placements
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
--
--
-- /See:/ 'gameSession' smart constructor.
data GameSession = GameSession'
  { _gsCreationTime                :: !(Maybe POSIX)
  , _gsStatus                      :: !(Maybe GameSessionStatus)
  , _gsGameProperties              :: !(Maybe [GameProperty])
  , _gsIPAddress                   :: !(Maybe Text)
  , _gsGameSessionId               :: !(Maybe Text)
  , _gsMatchmakerData              :: !(Maybe Text)
  , _gsMaximumPlayerSessionCount   :: !(Maybe Nat)
  , _gsTerminationTime             :: !(Maybe POSIX)
  , _gsPlayerSessionCreationPolicy :: !(Maybe PlayerSessionCreationPolicy)
  , _gsName                        :: !(Maybe Text)
  , _gsCurrentPlayerSessionCount   :: !(Maybe Nat)
  , _gsStatusReason                :: !(Maybe GameSessionStatusReason)
  , _gsGameSessionData             :: !(Maybe Text)
  , _gsFleetId                     :: !(Maybe Text)
  , _gsCreatorId                   :: !(Maybe Text)
  , _gsPort                        :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GameSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'gsStatus' - Current status of the game session. A game session must have an @ACTIVE@ status to have player sessions.
--
-- * 'gsGameProperties' - Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). You can search for active game sessions based on this custom data with 'SearchGameSessions' .
--
-- * 'gsIPAddress' - IP address of the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
--
-- * 'gsGameSessionId' - Unique identifier for the game session. A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .
--
-- * 'gsMatchmakerData' - Information about the matchmaking process that was used to create the game session. It is in JSON syntax, formatted as a string. In addition the matchmaking configuration used, it contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/match-server.html#match-server-data Match Data> . Matchmaker data is useful when requesting match backfills, and is updated whenever new players are added during a successful backfill (see 'StartMatchBackfill' ).
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
-- * 'gsStatusReason' - Provides additional information about game session status. @INTERRUPTED@ indicates that the game session was hosted on a spot instance that was reclaimed, causing the active game session to be terminated.
--
-- * 'gsGameSessionData' - Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- * 'gsFleetId' - Unique identifier for a fleet that the game session is running on.
--
-- * 'gsCreatorId' - Unique identifier for a player. This ID is used to enforce a resource protection policy (if one exists), that limits the number of game sessions a player can create.
--
-- * 'gsPort' - Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
gameSession
    :: GameSession
gameSession =
  GameSession'
    { _gsCreationTime = Nothing
    , _gsStatus = Nothing
    , _gsGameProperties = Nothing
    , _gsIPAddress = Nothing
    , _gsGameSessionId = Nothing
    , _gsMatchmakerData = Nothing
    , _gsMaximumPlayerSessionCount = Nothing
    , _gsTerminationTime = Nothing
    , _gsPlayerSessionCreationPolicy = Nothing
    , _gsName = Nothing
    , _gsCurrentPlayerSessionCount = Nothing
    , _gsStatusReason = Nothing
    , _gsGameSessionData = Nothing
    , _gsFleetId = Nothing
    , _gsCreatorId = Nothing
    , _gsPort = Nothing
    }


-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
gsCreationTime :: Lens' GameSession (Maybe UTCTime)
gsCreationTime = lens _gsCreationTime (\ s a -> s{_gsCreationTime = a}) . mapping _Time

-- | Current status of the game session. A game session must have an @ACTIVE@ status to have player sessions.
gsStatus :: Lens' GameSession (Maybe GameSessionStatus)
gsStatus = lens _gsStatus (\ s a -> s{_gsStatus = a})

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). You can search for active game sessions based on this custom data with 'SearchGameSessions' .
gsGameProperties :: Lens' GameSession [GameProperty]
gsGameProperties = lens _gsGameProperties (\ s a -> s{_gsGameProperties = a}) . _Default . _Coerce

-- | IP address of the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
gsIPAddress :: Lens' GameSession (Maybe Text)
gsIPAddress = lens _gsIPAddress (\ s a -> s{_gsIPAddress = a})

-- | Unique identifier for the game session. A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .
gsGameSessionId :: Lens' GameSession (Maybe Text)
gsGameSessionId = lens _gsGameSessionId (\ s a -> s{_gsGameSessionId = a})

-- | Information about the matchmaking process that was used to create the game session. It is in JSON syntax, formatted as a string. In addition the matchmaking configuration used, it contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/match-server.html#match-server-data Match Data> . Matchmaker data is useful when requesting match backfills, and is updated whenever new players are added during a successful backfill (see 'StartMatchBackfill' ).
gsMatchmakerData :: Lens' GameSession (Maybe Text)
gsMatchmakerData = lens _gsMatchmakerData (\ s a -> s{_gsMatchmakerData = a})

-- | Maximum number of players that can be connected simultaneously to the game session.
gsMaximumPlayerSessionCount :: Lens' GameSession (Maybe Natural)
gsMaximumPlayerSessionCount = lens _gsMaximumPlayerSessionCount (\ s a -> s{_gsMaximumPlayerSessionCount = a}) . mapping _Nat

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
gsTerminationTime :: Lens' GameSession (Maybe UTCTime)
gsTerminationTime = lens _gsTerminationTime (\ s a -> s{_gsTerminationTime = a}) . mapping _Time

-- | Indicates whether or not the game session is accepting new players.
gsPlayerSessionCreationPolicy :: Lens' GameSession (Maybe PlayerSessionCreationPolicy)
gsPlayerSessionCreationPolicy = lens _gsPlayerSessionCreationPolicy (\ s a -> s{_gsPlayerSessionCreationPolicy = a})

-- | Descriptive label that is associated with a game session. Session names do not need to be unique.
gsName :: Lens' GameSession (Maybe Text)
gsName = lens _gsName (\ s a -> s{_gsName = a})

-- | Number of players currently in the game session.
gsCurrentPlayerSessionCount :: Lens' GameSession (Maybe Natural)
gsCurrentPlayerSessionCount = lens _gsCurrentPlayerSessionCount (\ s a -> s{_gsCurrentPlayerSessionCount = a}) . mapping _Nat

-- | Provides additional information about game session status. @INTERRUPTED@ indicates that the game session was hosted on a spot instance that was reclaimed, causing the active game session to be terminated.
gsStatusReason :: Lens' GameSession (Maybe GameSessionStatusReason)
gsStatusReason = lens _gsStatusReason (\ s a -> s{_gsStatusReason = a})

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
gsGameSessionData :: Lens' GameSession (Maybe Text)
gsGameSessionData = lens _gsGameSessionData (\ s a -> s{_gsGameSessionData = a})

-- | Unique identifier for a fleet that the game session is running on.
gsFleetId :: Lens' GameSession (Maybe Text)
gsFleetId = lens _gsFleetId (\ s a -> s{_gsFleetId = a})

-- | Unique identifier for a player. This ID is used to enforce a resource protection policy (if one exists), that limits the number of game sessions a player can create.
gsCreatorId :: Lens' GameSession (Maybe Text)
gsCreatorId = lens _gsCreatorId (\ s a -> s{_gsCreatorId = a})

-- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
gsPort :: Lens' GameSession (Maybe Natural)
gsPort = lens _gsPort (\ s a -> s{_gsPort = a}) . mapping _Nat

instance FromJSON GameSession where
        parseJSON
          = withObject "GameSession"
              (\ x ->
                 GameSession' <$>
                   (x .:? "CreationTime") <*> (x .:? "Status") <*>
                     (x .:? "GameProperties" .!= mempty)
                     <*> (x .:? "IpAddress")
                     <*> (x .:? "GameSessionId")
                     <*> (x .:? "MatchmakerData")
                     <*> (x .:? "MaximumPlayerSessionCount")
                     <*> (x .:? "TerminationTime")
                     <*> (x .:? "PlayerSessionCreationPolicy")
                     <*> (x .:? "Name")
                     <*> (x .:? "CurrentPlayerSessionCount")
                     <*> (x .:? "StatusReason")
                     <*> (x .:? "GameSessionData")
                     <*> (x .:? "FleetId")
                     <*> (x .:? "CreatorId")
                     <*> (x .:? "Port"))

instance Hashable GameSession where

instance NFData GameSession where

-- | Connection information for the new game session that is created with matchmaking. (with 'StartMatchmaking' ). Once a match is set, the FlexMatch engine places the match and creates a new game session for it. This information, including the game session endpoint and player sessions for each player in the original matchmaking request, is added to the 'MatchmakingTicket' , which can be retrieved by calling 'DescribeMatchmaking' .
--
--
--
-- /See:/ 'gameSessionConnectionInfo' smart constructor.
data GameSessionConnectionInfo = GameSessionConnectionInfo'
  { _gsciMatchedPlayerSessions :: !(Maybe [MatchedPlayerSession])
  , _gsciIPAddress             :: !(Maybe Text)
  , _gsciGameSessionARN        :: !(Maybe Text)
  , _gsciPort                  :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GameSessionConnectionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsciMatchedPlayerSessions' - Collection of player session IDs, one for each player ID that was included in the original matchmaking request.
--
-- * 'gsciIPAddress' - IP address of the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
--
-- * 'gsciGameSessionARN' - Amazon Resource Name (<http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) that is assigned to a game session and uniquely identifies it.
--
-- * 'gsciPort' - Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
gameSessionConnectionInfo
    :: GameSessionConnectionInfo
gameSessionConnectionInfo =
  GameSessionConnectionInfo'
    { _gsciMatchedPlayerSessions = Nothing
    , _gsciIPAddress = Nothing
    , _gsciGameSessionARN = Nothing
    , _gsciPort = Nothing
    }


-- | Collection of player session IDs, one for each player ID that was included in the original matchmaking request.
gsciMatchedPlayerSessions :: Lens' GameSessionConnectionInfo [MatchedPlayerSession]
gsciMatchedPlayerSessions = lens _gsciMatchedPlayerSessions (\ s a -> s{_gsciMatchedPlayerSessions = a}) . _Default . _Coerce

-- | IP address of the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
gsciIPAddress :: Lens' GameSessionConnectionInfo (Maybe Text)
gsciIPAddress = lens _gsciIPAddress (\ s a -> s{_gsciIPAddress = a})

-- | Amazon Resource Name (<http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) that is assigned to a game session and uniquely identifies it.
gsciGameSessionARN :: Lens' GameSessionConnectionInfo (Maybe Text)
gsciGameSessionARN = lens _gsciGameSessionARN (\ s a -> s{_gsciGameSessionARN = a})

-- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
gsciPort :: Lens' GameSessionConnectionInfo (Maybe Natural)
gsciPort = lens _gsciPort (\ s a -> s{_gsciPort = a}) . mapping _Nat

instance FromJSON GameSessionConnectionInfo where
        parseJSON
          = withObject "GameSessionConnectionInfo"
              (\ x ->
                 GameSessionConnectionInfo' <$>
                   (x .:? "MatchedPlayerSessions" .!= mempty) <*>
                     (x .:? "IpAddress")
                     <*> (x .:? "GameSessionArn")
                     <*> (x .:? "Port"))

instance Hashable GameSessionConnectionInfo where

instance NFData GameSessionConnectionInfo where

-- | A game session's properties plus the protection policy currently in force.
--
--
--
-- /See:/ 'gameSessionDetail' smart constructor.
data GameSessionDetail = GameSessionDetail'
  { _gsdGameSession      :: !(Maybe GameSession)
  , _gsdProtectionPolicy :: !(Maybe ProtectionPolicy)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GameSessionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdGameSession' - Object that describes a game session.
--
-- * 'gsdProtectionPolicy' - Current status of protection for the game session.     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
gameSessionDetail
    :: GameSessionDetail
gameSessionDetail =
  GameSessionDetail' {_gsdGameSession = Nothing, _gsdProtectionPolicy = Nothing}


-- | Object that describes a game session.
gsdGameSession :: Lens' GameSessionDetail (Maybe GameSession)
gsdGameSession = lens _gsdGameSession (\ s a -> s{_gsdGameSession = a})

-- | Current status of protection for the game session.     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
gsdProtectionPolicy :: Lens' GameSessionDetail (Maybe ProtectionPolicy)
gsdProtectionPolicy = lens _gsdProtectionPolicy (\ s a -> s{_gsdProtectionPolicy = a})

instance FromJSON GameSessionDetail where
        parseJSON
          = withObject "GameSessionDetail"
              (\ x ->
                 GameSessionDetail' <$>
                   (x .:? "GameSession") <*> (x .:? "ProtectionPolicy"))

instance Hashable GameSessionDetail where

instance NFData GameSessionDetail where

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
  , _gspIPAddress                 :: !(Maybe Text)
  , _gspGameSessionName           :: !(Maybe Text)
  , _gspStartTime                 :: !(Maybe POSIX)
  , _gspGameSessionId             :: !(Maybe Text)
  , _gspGameSessionRegion         :: !(Maybe Text)
  , _gspMatchmakerData            :: !(Maybe Text)
  , _gspMaximumPlayerSessionCount :: !(Maybe Nat)
  , _gspEndTime                   :: !(Maybe POSIX)
  , _gspGameSessionARN            :: !(Maybe Text)
  , _gspPlayerLatencies           :: !(Maybe [PlayerLatency])
  , _gspGameSessionData           :: !(Maybe Text)
  , _gspGameSessionQueueName      :: !(Maybe Text)
  , _gspPlacedPlayerSessions      :: !(Maybe [PlacedPlayerSession])
  , _gspPort                      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GameSessionPlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspStatus' - Current status of the game session placement request.     * __PENDING__ -- The placement request is currently in the queue waiting to be processed.     * __FULFILLED__ -- A new game session and player sessions (if requested) have been successfully created. Values for /GameSessionArn/ and /GameSessionRegion/ are available.      * __CANCELLED__ -- The placement request was canceled with a call to 'StopGameSessionPlacement' .     * __TIMED_OUT__ -- A new game session was not successfully created before the time limit expired. You can resubmit the placement request as needed.
--
-- * 'gspPlacementId' - Unique identifier for a game session placement.
--
-- * 'gspGameProperties' - Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- * 'gspIPAddress' - IP address of the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- * 'gspGameSessionName' - Descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- * 'gspStartTime' - Time stamp indicating when this request was placed in the queue. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'gspGameSessionId' - Unique identifier for the game session. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- * 'gspGameSessionRegion' - Name of the region where the game session created by this placement request is running. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- * 'gspMatchmakerData' - Information on the matchmaking process for this game. Data is in JSON syntax, formatted as a string. It identifies the matchmaking configuration used to create the match, and contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/match-server.html#match-server-data Match Data> .
--
-- * 'gspMaximumPlayerSessionCount' - Maximum number of players that can be connected simultaneously to the game session.
--
-- * 'gspEndTime' - Time stamp indicating when this request was completed, canceled, or timed out.
--
-- * 'gspGameSessionARN' - Identifier for the game session created by this placement request. This value is set once the new game session is placed (placement status is @FULFILLED@ ). This identifier is unique across all regions. You can use this value as a @GameSessionId@ value as needed.
--
-- * 'gspPlayerLatencies' - Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS regions.
--
-- * 'gspGameSessionData' - Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- * 'gspGameSessionQueueName' - Descriptive label that is associated with game session queue. Queue names must be unique within each region.
--
-- * 'gspPlacedPlayerSessions' - Collection of information on player sessions created in response to the game session placement request. These player sessions are created only once a new game session is successfully placed (placement status is @FULFILLED@ ). This information includes the player ID (as provided in the placement request) and the corresponding player session ID. Retrieve full player sessions by calling 'DescribePlayerSessions' with the player session ID.
--
-- * 'gspPort' - Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
gameSessionPlacement
    :: GameSessionPlacement
gameSessionPlacement =
  GameSessionPlacement'
    { _gspStatus = Nothing
    , _gspPlacementId = Nothing
    , _gspGameProperties = Nothing
    , _gspIPAddress = Nothing
    , _gspGameSessionName = Nothing
    , _gspStartTime = Nothing
    , _gspGameSessionId = Nothing
    , _gspGameSessionRegion = Nothing
    , _gspMatchmakerData = Nothing
    , _gspMaximumPlayerSessionCount = Nothing
    , _gspEndTime = Nothing
    , _gspGameSessionARN = Nothing
    , _gspPlayerLatencies = Nothing
    , _gspGameSessionData = Nothing
    , _gspGameSessionQueueName = Nothing
    , _gspPlacedPlayerSessions = Nothing
    , _gspPort = Nothing
    }


-- | Current status of the game session placement request.     * __PENDING__ -- The placement request is currently in the queue waiting to be processed.     * __FULFILLED__ -- A new game session and player sessions (if requested) have been successfully created. Values for /GameSessionArn/ and /GameSessionRegion/ are available.      * __CANCELLED__ -- The placement request was canceled with a call to 'StopGameSessionPlacement' .     * __TIMED_OUT__ -- A new game session was not successfully created before the time limit expired. You can resubmit the placement request as needed.
gspStatus :: Lens' GameSessionPlacement (Maybe GameSessionPlacementState)
gspStatus = lens _gspStatus (\ s a -> s{_gspStatus = a})

-- | Unique identifier for a game session placement.
gspPlacementId :: Lens' GameSessionPlacement (Maybe Text)
gspPlacementId = lens _gspPlacementId (\ s a -> s{_gspPlacementId = a})

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
gspGameProperties :: Lens' GameSessionPlacement [GameProperty]
gspGameProperties = lens _gspGameProperties (\ s a -> s{_gspGameProperties = a}) . _Default . _Coerce

-- | IP address of the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
gspIPAddress :: Lens' GameSessionPlacement (Maybe Text)
gspIPAddress = lens _gspIPAddress (\ s a -> s{_gspIPAddress = a})

-- | Descriptive label that is associated with a game session. Session names do not need to be unique.
gspGameSessionName :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionName = lens _gspGameSessionName (\ s a -> s{_gspGameSessionName = a})

-- | Time stamp indicating when this request was placed in the queue. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
gspStartTime :: Lens' GameSessionPlacement (Maybe UTCTime)
gspStartTime = lens _gspStartTime (\ s a -> s{_gspStartTime = a}) . mapping _Time

-- | Unique identifier for the game session. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
gspGameSessionId :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionId = lens _gspGameSessionId (\ s a -> s{_gspGameSessionId = a})

-- | Name of the region where the game session created by this placement request is running. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
gspGameSessionRegion :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionRegion = lens _gspGameSessionRegion (\ s a -> s{_gspGameSessionRegion = a})

-- | Information on the matchmaking process for this game. Data is in JSON syntax, formatted as a string. It identifies the matchmaking configuration used to create the match, and contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/match-server.html#match-server-data Match Data> .
gspMatchmakerData :: Lens' GameSessionPlacement (Maybe Text)
gspMatchmakerData = lens _gspMatchmakerData (\ s a -> s{_gspMatchmakerData = a})

-- | Maximum number of players that can be connected simultaneously to the game session.
gspMaximumPlayerSessionCount :: Lens' GameSessionPlacement (Maybe Natural)
gspMaximumPlayerSessionCount = lens _gspMaximumPlayerSessionCount (\ s a -> s{_gspMaximumPlayerSessionCount = a}) . mapping _Nat

-- | Time stamp indicating when this request was completed, canceled, or timed out.
gspEndTime :: Lens' GameSessionPlacement (Maybe UTCTime)
gspEndTime = lens _gspEndTime (\ s a -> s{_gspEndTime = a}) . mapping _Time

-- | Identifier for the game session created by this placement request. This value is set once the new game session is placed (placement status is @FULFILLED@ ). This identifier is unique across all regions. You can use this value as a @GameSessionId@ value as needed.
gspGameSessionARN :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionARN = lens _gspGameSessionARN (\ s a -> s{_gspGameSessionARN = a})

-- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS regions.
gspPlayerLatencies :: Lens' GameSessionPlacement [PlayerLatency]
gspPlayerLatencies = lens _gspPlayerLatencies (\ s a -> s{_gspPlayerLatencies = a}) . _Default . _Coerce

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
gspGameSessionData :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionData = lens _gspGameSessionData (\ s a -> s{_gspGameSessionData = a})

-- | Descriptive label that is associated with game session queue. Queue names must be unique within each region.
gspGameSessionQueueName :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionQueueName = lens _gspGameSessionQueueName (\ s a -> s{_gspGameSessionQueueName = a})

-- | Collection of information on player sessions created in response to the game session placement request. These player sessions are created only once a new game session is successfully placed (placement status is @FULFILLED@ ). This information includes the player ID (as provided in the placement request) and the corresponding player session ID. Retrieve full player sessions by calling 'DescribePlayerSessions' with the player session ID.
gspPlacedPlayerSessions :: Lens' GameSessionPlacement [PlacedPlayerSession]
gspPlacedPlayerSessions = lens _gspPlacedPlayerSessions (\ s a -> s{_gspPlacedPlayerSessions = a}) . _Default . _Coerce

-- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
gspPort :: Lens' GameSessionPlacement (Maybe Natural)
gspPort = lens _gspPort (\ s a -> s{_gspPort = a}) . mapping _Nat

instance FromJSON GameSessionPlacement where
        parseJSON
          = withObject "GameSessionPlacement"
              (\ x ->
                 GameSessionPlacement' <$>
                   (x .:? "Status") <*> (x .:? "PlacementId") <*>
                     (x .:? "GameProperties" .!= mempty)
                     <*> (x .:? "IpAddress")
                     <*> (x .:? "GameSessionName")
                     <*> (x .:? "StartTime")
                     <*> (x .:? "GameSessionId")
                     <*> (x .:? "GameSessionRegion")
                     <*> (x .:? "MatchmakerData")
                     <*> (x .:? "MaximumPlayerSessionCount")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "GameSessionArn")
                     <*> (x .:? "PlayerLatencies" .!= mempty)
                     <*> (x .:? "GameSessionData")
                     <*> (x .:? "GameSessionQueueName")
                     <*> (x .:? "PlacedPlayerSessions" .!= mempty)
                     <*> (x .:? "Port"))

instance Hashable GameSessionPlacement where

instance NFData GameSessionPlacement where

-- | Configuration of a queue that is used to process game session placement requests. The queue configuration identifies several game features:
--
--
--     * The destinations where a new game session can potentially be hosted. Amazon GameLift tries these destinations in an order based on either the queue's default order or player latency information, if provided in a placement request. With latency information, Amazon GameLift can place game sessions where the majority of players are reporting the lowest possible latency.
--
--     * The length of time that placement requests can wait in the queue before timing out.
--
--     * A set of optional latency policies that protect individual players from high latencies, preventing game sessions from being placed where any individual player is reporting latency higher than a policy's maximum.
--
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
  { _gsqGameSessionQueueARN   :: !(Maybe Text)
  , _gsqPlayerLatencyPolicies :: !(Maybe [PlayerLatencyPolicy])
  , _gsqTimeoutInSeconds      :: !(Maybe Nat)
  , _gsqDestinations          :: !(Maybe [GameSessionQueueDestination])
  , _gsqName                  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GameSessionQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsqGameSessionQueueARN' - Amazon Resource Name (<http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) that is assigned to a game session queue and uniquely identifies it. Format is @arn:aws:gamelift:<region>::fleet/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@ .
--
-- * 'gsqPlayerLatencyPolicies' - Collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, it is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement.
--
-- * 'gsqTimeoutInSeconds' - Maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
--
-- * 'gsqDestinations' - List of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
--
-- * 'gsqName' - Descriptive label that is associated with game session queue. Queue names must be unique within each region.
gameSessionQueue
    :: GameSessionQueue
gameSessionQueue =
  GameSessionQueue'
    { _gsqGameSessionQueueARN = Nothing
    , _gsqPlayerLatencyPolicies = Nothing
    , _gsqTimeoutInSeconds = Nothing
    , _gsqDestinations = Nothing
    , _gsqName = Nothing
    }


-- | Amazon Resource Name (<http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) that is assigned to a game session queue and uniquely identifies it. Format is @arn:aws:gamelift:<region>::fleet/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@ .
gsqGameSessionQueueARN :: Lens' GameSessionQueue (Maybe Text)
gsqGameSessionQueueARN = lens _gsqGameSessionQueueARN (\ s a -> s{_gsqGameSessionQueueARN = a})

-- | Collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, it is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement.
gsqPlayerLatencyPolicies :: Lens' GameSessionQueue [PlayerLatencyPolicy]
gsqPlayerLatencyPolicies = lens _gsqPlayerLatencyPolicies (\ s a -> s{_gsqPlayerLatencyPolicies = a}) . _Default . _Coerce

-- | Maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
gsqTimeoutInSeconds :: Lens' GameSessionQueue (Maybe Natural)
gsqTimeoutInSeconds = lens _gsqTimeoutInSeconds (\ s a -> s{_gsqTimeoutInSeconds = a}) . mapping _Nat

-- | List of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
gsqDestinations :: Lens' GameSessionQueue [GameSessionQueueDestination]
gsqDestinations = lens _gsqDestinations (\ s a -> s{_gsqDestinations = a}) . _Default . _Coerce

-- | Descriptive label that is associated with game session queue. Queue names must be unique within each region.
gsqName :: Lens' GameSessionQueue (Maybe Text)
gsqName = lens _gsqName (\ s a -> s{_gsqName = a})

instance FromJSON GameSessionQueue where
        parseJSON
          = withObject "GameSessionQueue"
              (\ x ->
                 GameSessionQueue' <$>
                   (x .:? "GameSessionQueueArn") <*>
                     (x .:? "PlayerLatencyPolicies" .!= mempty)
                     <*> (x .:? "TimeoutInSeconds")
                     <*> (x .:? "Destinations" .!= mempty)
                     <*> (x .:? "Name"))

instance Hashable GameSessionQueue where

instance NFData GameSessionQueue where

-- | Fleet designated in a game session queue. Requests for new game sessions in the queue are fulfilled by starting a new game session on any destination configured for a queue.
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
-- /See:/ 'gameSessionQueueDestination' smart constructor.
newtype GameSessionQueueDestination = GameSessionQueueDestination'
  { _gsqdDestinationARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GameSessionQueueDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsqdDestinationARN' - Amazon Resource Name (ARN) assigned to fleet or fleet alias. ARNs, which include a fleet ID or alias ID and a region name, provide a unique identifier across all regions.
gameSessionQueueDestination
    :: GameSessionQueueDestination
gameSessionQueueDestination =
  GameSessionQueueDestination' {_gsqdDestinationARN = Nothing}


-- | Amazon Resource Name (ARN) assigned to fleet or fleet alias. ARNs, which include a fleet ID or alias ID and a region name, provide a unique identifier across all regions.
gsqdDestinationARN :: Lens' GameSessionQueueDestination (Maybe Text)
gsqdDestinationARN = lens _gsqdDestinationARN (\ s a -> s{_gsqdDestinationARN = a})

instance FromJSON GameSessionQueueDestination where
        parseJSON
          = withObject "GameSessionQueueDestination"
              (\ x ->
                 GameSessionQueueDestination' <$>
                   (x .:? "DestinationArn"))

instance Hashable GameSessionQueueDestination where

instance NFData GameSessionQueueDestination where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
ipFromPort = lens _ipFromPort (\ s a -> s{_ipFromPort = a}) . _Nat

-- | Ending value for a range of allowed port numbers. Port numbers are end-inclusive. This value must be higher than @FromPort@ .
ipToPort :: Lens' IPPermission Natural
ipToPort = lens _ipToPort (\ s a -> s{_ipToPort = a}) . _Nat

-- | Range of allowed IP addresses. This value must be expressed in CIDR notation. Example: "@000.000.000.000/[subnet mask]@ " or optionally the shortened version "@0.0.0.0/[subnet mask]@ ".
ipIPRange :: Lens' IPPermission Text
ipIPRange = lens _ipIPRange (\ s a -> s{_ipIPRange = a})

-- | Network communication protocol used by the fleet.
ipProtocol :: Lens' IPPermission IPProtocol
ipProtocol = lens _ipProtocol (\ s a -> s{_ipProtocol = a})

instance FromJSON IPPermission where
        parseJSON
          = withObject "IPPermission"
              (\ x ->
                 IPPermission' <$>
                   (x .: "FromPort") <*> (x .: "ToPort") <*>
                     (x .: "IpRange")
                     <*> (x .: "Protocol"))

instance Hashable IPPermission where

instance NFData IPPermission where

instance ToJSON IPPermission where
        toJSON IPPermission'{..}
          = object
              (catMaybes
                 [Just ("FromPort" .= _ipFromPort),
                  Just ("ToPort" .= _ipToPort),
                  Just ("IpRange" .= _ipIPRange),
                  Just ("Protocol" .= _ipProtocol)])

-- | Properties that describe an instance of a virtual computing resource that hosts one or more game servers. A fleet may contain zero or more instances.
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'iInstanceId' - Unique identifier for an instance.
--
-- * 'iStatus' - Current status of the instance. Possible statuses include the following:     * __PENDING__ -- The instance is in the process of being created and launching server processes as defined in the fleet's run-time configuration.      * __ACTIVE__ -- The instance has been successfully created and at least one server process has successfully launched and reported back to Amazon GameLift that it is ready to host a game session. The instance is now considered ready to host game sessions.      * __TERMINATING__ -- The instance is in the process of shutting down. This may happen to reduce capacity during a scaling down event or to recycle resources in the event of a problem.
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
iCreationTime = lens _iCreationTime (\ s a -> s{_iCreationTime = a}) . mapping _Time

-- | Unique identifier for an instance.
iInstanceId :: Lens' Instance (Maybe Text)
iInstanceId = lens _iInstanceId (\ s a -> s{_iInstanceId = a})

-- | Current status of the instance. Possible statuses include the following:     * __PENDING__ -- The instance is in the process of being created and launching server processes as defined in the fleet's run-time configuration.      * __ACTIVE__ -- The instance has been successfully created and at least one server process has successfully launched and reported back to Amazon GameLift that it is ready to host a game session. The instance is now considered ready to host game sessions.      * __TERMINATING__ -- The instance is in the process of shutting down. This may happen to reduce capacity during a scaling down event or to recycle resources in the event of a problem.
iStatus :: Lens' Instance (Maybe InstanceStatus)
iStatus = lens _iStatus (\ s a -> s{_iStatus = a})

-- | IP address assigned to the instance.
iIPAddress :: Lens' Instance (Maybe Text)
iIPAddress = lens _iIPAddress (\ s a -> s{_iIPAddress = a})

-- | Operating system that is running on this instance.
iOperatingSystem :: Lens' Instance (Maybe OperatingSystem)
iOperatingSystem = lens _iOperatingSystem (\ s a -> s{_iOperatingSystem = a})

-- | EC2 instance type that defines the computing resources of this instance.
iType :: Lens' Instance (Maybe EC2InstanceType)
iType = lens _iType (\ s a -> s{_iType = a})

-- | Unique identifier for a fleet that the instance is in.
iFleetId :: Lens' Instance (Maybe Text)
iFleetId = lens _iFleetId (\ s a -> s{_iFleetId = a})

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

instance Hashable Instance where

instance NFData Instance where

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
  } deriving (Eq, Show, Data, Typeable, Generic)


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
iaInstanceId = lens _iaInstanceId (\ s a -> s{_iaInstanceId = a})

-- | IP address assigned to the instance.
iaIPAddress :: Lens' InstanceAccess (Maybe Text)
iaIPAddress = lens _iaIPAddress (\ s a -> s{_iaIPAddress = a})

-- | Operating system that is running on the instance.
iaOperatingSystem :: Lens' InstanceAccess (Maybe OperatingSystem)
iaOperatingSystem = lens _iaOperatingSystem (\ s a -> s{_iaOperatingSystem = a})

-- | Credentials required to access the instance.
iaCredentials :: Lens' InstanceAccess (Maybe InstanceCredentials)
iaCredentials = lens _iaCredentials (\ s a -> s{_iaCredentials = a}) . mapping _Sensitive

-- | Unique identifier for a fleet containing the instance being accessed.
iaFleetId :: Lens' InstanceAccess (Maybe Text)
iaFleetId = lens _iaFleetId (\ s a -> s{_iaFleetId = a})

instance FromJSON InstanceAccess where
        parseJSON
          = withObject "InstanceAccess"
              (\ x ->
                 InstanceAccess' <$>
                   (x .:? "InstanceId") <*> (x .:? "IpAddress") <*>
                     (x .:? "OperatingSystem")
                     <*> (x .:? "Credentials")
                     <*> (x .:? "FleetId"))

instance Hashable InstanceAccess where

instance NFData InstanceAccess where

-- | Set of credentials required to remotely access a fleet instance. Access credentials are requested by calling 'GetInstanceAccess' and returned in an 'InstanceAccess' object.
--
--
--
-- /See:/ 'instanceCredentials' smart constructor.
data InstanceCredentials = InstanceCredentials'
  { _icUserName :: !(Maybe Text)
  , _icSecret   :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


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
  InstanceCredentials' {_icUserName = Nothing, _icSecret = Nothing}


-- | User login string.
icUserName :: Lens' InstanceCredentials (Maybe Text)
icUserName = lens _icUserName (\ s a -> s{_icUserName = a})

-- | Secret string. For Windows instances, the secret is a password for use with Windows Remote Desktop. For Linux instances, it is a private key (which must be saved as a @.pem@ file) for use with SSH.
icSecret :: Lens' InstanceCredentials (Maybe Text)
icSecret = lens _icSecret (\ s a -> s{_icSecret = a})

instance FromJSON InstanceCredentials where
        parseJSON
          = withObject "InstanceCredentials"
              (\ x ->
                 InstanceCredentials' <$>
                   (x .:? "UserName") <*> (x .:? "Secret"))

instance Hashable InstanceCredentials where

instance NFData InstanceCredentials where

-- | Represents a new player session that is created as a result of a successful FlexMatch match. A successful match automatically creates new player sessions for every player ID in the original matchmaking request.
--
--
-- When players connect to the match's game session, they must include both player ID and player session ID in order to claim their assigned player slot.
--
--
-- /See:/ 'matchedPlayerSession' smart constructor.
data MatchedPlayerSession = MatchedPlayerSession'
  { _mpsPlayerSessionId :: !(Maybe Text)
  , _mpsPlayerId        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MatchedPlayerSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpsPlayerSessionId' - Unique identifier for a player session
--
-- * 'mpsPlayerId' - Unique identifier for a player
matchedPlayerSession
    :: MatchedPlayerSession
matchedPlayerSession =
  MatchedPlayerSession' {_mpsPlayerSessionId = Nothing, _mpsPlayerId = Nothing}


-- | Unique identifier for a player session
mpsPlayerSessionId :: Lens' MatchedPlayerSession (Maybe Text)
mpsPlayerSessionId = lens _mpsPlayerSessionId (\ s a -> s{_mpsPlayerSessionId = a})

-- | Unique identifier for a player
mpsPlayerId :: Lens' MatchedPlayerSession (Maybe Text)
mpsPlayerId = lens _mpsPlayerId (\ s a -> s{_mpsPlayerId = a})

instance FromJSON MatchedPlayerSession where
        parseJSON
          = withObject "MatchedPlayerSession"
              (\ x ->
                 MatchedPlayerSession' <$>
                   (x .:? "PlayerSessionId") <*> (x .:? "PlayerId"))

instance Hashable MatchedPlayerSession where

instance NFData MatchedPlayerSession where

-- | Guidelines for use with FlexMatch to match players into games. All matchmaking requests must specify a matchmaking configuration.
--
--
--
-- /See:/ 'matchmakingConfiguration' smart constructor.
data MatchmakingConfiguration = MatchmakingConfiguration'
  { _mcCreationTime             :: !(Maybe POSIX)
  , _mcGameProperties           :: !(Maybe [GameProperty])
  , _mcRuleSetName              :: !(Maybe Text)
  , _mcAcceptanceTimeoutSeconds :: !(Maybe Nat)
  , _mcRequestTimeoutSeconds    :: !(Maybe Nat)
  , _mcNotificationTarget       :: !(Maybe Text)
  , _mcGameSessionQueueARNs     :: !(Maybe [Text])
  , _mcName                     :: !(Maybe Text)
  , _mcCustomEventData          :: !(Maybe Text)
  , _mcAcceptanceRequired       :: !(Maybe Bool)
  , _mcGameSessionData          :: !(Maybe Text)
  , _mcDescription              :: !(Maybe Text)
  , _mcAdditionalPlayerCount    :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MatchmakingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'mcGameProperties' - Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match.
--
-- * 'mcRuleSetName' - Unique identifier for a matchmaking rule set to use with this configuration. A matchmaking configuration can only use rule sets that are defined in the same region.
--
-- * 'mcAcceptanceTimeoutSeconds' - Length of time (in seconds) to wait for players to accept a proposed match. If any player rejects the match or fails to accept before the timeout, the ticket continues to look for an acceptable match.
--
-- * 'mcRequestTimeoutSeconds' - Maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that time out can be resubmitted as needed.
--
-- * 'mcNotificationTarget' - SNS topic ARN that is set up to receive matchmaking notifications.
--
-- * 'mcGameSessionQueueARNs' - Amazon Resource Name (<http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) that is assigned to a game session queue and uniquely identifies it. Format is @arn:aws:gamelift:<region>::fleet/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@ . These queues are used when placing game sessions for matches that are created with this matchmaking configuration. Queues can be located in any region.
--
-- * 'mcName' - Unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
--
-- * 'mcCustomEventData' - Information to attached to all events related to the matchmaking configuration.
--
-- * 'mcAcceptanceRequired' - Flag that determines whether or not a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE.
--
-- * 'mcGameSessionData' - Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match.
--
-- * 'mcDescription' - Descriptive label that is associated with matchmaking configuration.
--
-- * 'mcAdditionalPlayerCount' - Number of player slots in a match to keep open for future players. For example, if the configuration's rule set specifies a match for a single 12-person team, and the additional player count is set to 2, only 10 players are selected for the match.
matchmakingConfiguration
    :: MatchmakingConfiguration
matchmakingConfiguration =
  MatchmakingConfiguration'
    { _mcCreationTime = Nothing
    , _mcGameProperties = Nothing
    , _mcRuleSetName = Nothing
    , _mcAcceptanceTimeoutSeconds = Nothing
    , _mcRequestTimeoutSeconds = Nothing
    , _mcNotificationTarget = Nothing
    , _mcGameSessionQueueARNs = Nothing
    , _mcName = Nothing
    , _mcCustomEventData = Nothing
    , _mcAcceptanceRequired = Nothing
    , _mcGameSessionData = Nothing
    , _mcDescription = Nothing
    , _mcAdditionalPlayerCount = Nothing
    }


-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
mcCreationTime :: Lens' MatchmakingConfiguration (Maybe UTCTime)
mcCreationTime = lens _mcCreationTime (\ s a -> s{_mcCreationTime = a}) . mapping _Time

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match.
mcGameProperties :: Lens' MatchmakingConfiguration [GameProperty]
mcGameProperties = lens _mcGameProperties (\ s a -> s{_mcGameProperties = a}) . _Default . _Coerce

-- | Unique identifier for a matchmaking rule set to use with this configuration. A matchmaking configuration can only use rule sets that are defined in the same region.
mcRuleSetName :: Lens' MatchmakingConfiguration (Maybe Text)
mcRuleSetName = lens _mcRuleSetName (\ s a -> s{_mcRuleSetName = a})

-- | Length of time (in seconds) to wait for players to accept a proposed match. If any player rejects the match or fails to accept before the timeout, the ticket continues to look for an acceptable match.
mcAcceptanceTimeoutSeconds :: Lens' MatchmakingConfiguration (Maybe Natural)
mcAcceptanceTimeoutSeconds = lens _mcAcceptanceTimeoutSeconds (\ s a -> s{_mcAcceptanceTimeoutSeconds = a}) . mapping _Nat

-- | Maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that time out can be resubmitted as needed.
mcRequestTimeoutSeconds :: Lens' MatchmakingConfiguration (Maybe Natural)
mcRequestTimeoutSeconds = lens _mcRequestTimeoutSeconds (\ s a -> s{_mcRequestTimeoutSeconds = a}) . mapping _Nat

-- | SNS topic ARN that is set up to receive matchmaking notifications.
mcNotificationTarget :: Lens' MatchmakingConfiguration (Maybe Text)
mcNotificationTarget = lens _mcNotificationTarget (\ s a -> s{_mcNotificationTarget = a})

-- | Amazon Resource Name (<http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) that is assigned to a game session queue and uniquely identifies it. Format is @arn:aws:gamelift:<region>::fleet/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@ . These queues are used when placing game sessions for matches that are created with this matchmaking configuration. Queues can be located in any region.
mcGameSessionQueueARNs :: Lens' MatchmakingConfiguration [Text]
mcGameSessionQueueARNs = lens _mcGameSessionQueueARNs (\ s a -> s{_mcGameSessionQueueARNs = a}) . _Default . _Coerce

-- | Unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
mcName :: Lens' MatchmakingConfiguration (Maybe Text)
mcName = lens _mcName (\ s a -> s{_mcName = a})

-- | Information to attached to all events related to the matchmaking configuration.
mcCustomEventData :: Lens' MatchmakingConfiguration (Maybe Text)
mcCustomEventData = lens _mcCustomEventData (\ s a -> s{_mcCustomEventData = a})

-- | Flag that determines whether or not a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE.
mcAcceptanceRequired :: Lens' MatchmakingConfiguration (Maybe Bool)
mcAcceptanceRequired = lens _mcAcceptanceRequired (\ s a -> s{_mcAcceptanceRequired = a})

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match.
mcGameSessionData :: Lens' MatchmakingConfiguration (Maybe Text)
mcGameSessionData = lens _mcGameSessionData (\ s a -> s{_mcGameSessionData = a})

-- | Descriptive label that is associated with matchmaking configuration.
mcDescription :: Lens' MatchmakingConfiguration (Maybe Text)
mcDescription = lens _mcDescription (\ s a -> s{_mcDescription = a})

-- | Number of player slots in a match to keep open for future players. For example, if the configuration's rule set specifies a match for a single 12-person team, and the additional player count is set to 2, only 10 players are selected for the match.
mcAdditionalPlayerCount :: Lens' MatchmakingConfiguration (Maybe Natural)
mcAdditionalPlayerCount = lens _mcAdditionalPlayerCount (\ s a -> s{_mcAdditionalPlayerCount = a}) . mapping _Nat

instance FromJSON MatchmakingConfiguration where
        parseJSON
          = withObject "MatchmakingConfiguration"
              (\ x ->
                 MatchmakingConfiguration' <$>
                   (x .:? "CreationTime") <*>
                     (x .:? "GameProperties" .!= mempty)
                     <*> (x .:? "RuleSetName")
                     <*> (x .:? "AcceptanceTimeoutSeconds")
                     <*> (x .:? "RequestTimeoutSeconds")
                     <*> (x .:? "NotificationTarget")
                     <*> (x .:? "GameSessionQueueArns" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "CustomEventData")
                     <*> (x .:? "AcceptanceRequired")
                     <*> (x .:? "GameSessionData")
                     <*> (x .:? "Description")
                     <*> (x .:? "AdditionalPlayerCount"))

instance Hashable MatchmakingConfiguration where

instance NFData MatchmakingConfiguration where

-- | Set of rule statements, used with FlexMatch, that determine how to build a certain kind of player match. Each rule set describes a type of group to be created and defines the parameters for acceptable player matches. Rule sets are used in 'MatchmakingConfiguration' objects.
--
--
-- A rule set may define the following elements for a match. For detailed information and examples showing how to construct a rule set, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/match-rulesets.html Build a FlexMatch Rule Set> .
--
--     * Teams -- Required. A rule set must define one or multiple teams for the match and set minimum and maximum team sizes. For example, a rule set might describe a 4x4 match that requires all eight slots to be filled.
--
--     * Player attributes -- Optional. These attributes specify a set of player characteristics to evaluate when looking for a match. Matchmaking requests that use a rule set with player attributes must provide the corresponding attribute values. For example, an attribute might specify a player's skill or level.
--
--     * Rules -- Optional. Rules define how to evaluate potential players for a match based on player attributes. A rule might specify minimum requirements for individual players, teams, or entire matches. For example, a rule might require each player to meet a certain skill level, each team to have at least one player in a certain role, or the match to have a minimum average skill level. or may describe an entire group--such as all teams must be evenly matched or have at least one player in a certain role.
--
--     * Expansions -- Optional. Expansions allow you to relax the rules after a period of time when no acceptable matches are found. This feature lets you balance getting players into games in a reasonable amount of time instead of making them wait indefinitely for the best possible match. For example, you might use an expansion to increase the maximum skill variance between players after 30 seconds.
--
--
--
--
-- /See:/ 'matchmakingRuleSet' smart constructor.
data MatchmakingRuleSet = MatchmakingRuleSet'
  { _mrsCreationTime :: !(Maybe POSIX)
  , _mrsRuleSetName  :: !(Maybe Text)
  , _mrsRuleSetBody  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MatchmakingRuleSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrsCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'mrsRuleSetName' - Unique identifier for a matchmaking rule set
--
-- * 'mrsRuleSetBody' - Collection of matchmaking rules, formatted as a JSON string. (Note that comments14 are not allowed in JSON, but most elements support a description field.)
matchmakingRuleSet
    :: Text -- ^ 'mrsRuleSetBody'
    -> MatchmakingRuleSet
matchmakingRuleSet pRuleSetBody_ =
  MatchmakingRuleSet'
    { _mrsCreationTime = Nothing
    , _mrsRuleSetName = Nothing
    , _mrsRuleSetBody = pRuleSetBody_
    }


-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
mrsCreationTime :: Lens' MatchmakingRuleSet (Maybe UTCTime)
mrsCreationTime = lens _mrsCreationTime (\ s a -> s{_mrsCreationTime = a}) . mapping _Time

-- | Unique identifier for a matchmaking rule set
mrsRuleSetName :: Lens' MatchmakingRuleSet (Maybe Text)
mrsRuleSetName = lens _mrsRuleSetName (\ s a -> s{_mrsRuleSetName = a})

-- | Collection of matchmaking rules, formatted as a JSON string. (Note that comments14 are not allowed in JSON, but most elements support a description field.)
mrsRuleSetBody :: Lens' MatchmakingRuleSet Text
mrsRuleSetBody = lens _mrsRuleSetBody (\ s a -> s{_mrsRuleSetBody = a})

instance FromJSON MatchmakingRuleSet where
        parseJSON
          = withObject "MatchmakingRuleSet"
              (\ x ->
                 MatchmakingRuleSet' <$>
                   (x .:? "CreationTime") <*> (x .:? "RuleSetName") <*>
                     (x .: "RuleSetBody"))

instance Hashable MatchmakingRuleSet where

instance NFData MatchmakingRuleSet where

-- | Ticket generated to track the progress of a matchmaking request. Each ticket is uniquely identified by a ticket ID, supplied by the requester, when creating a matchmaking request with 'StartMatchmaking' . Tickets can be retrieved by calling 'DescribeMatchmaking' with the ticket ID.
--
--
--
-- /See:/ 'matchmakingTicket' smart constructor.
data MatchmakingTicket = MatchmakingTicket'
  { _mtStatus                    :: !(Maybe MatchmakingConfigurationStatus)
  , _mtConfigurationName         :: !(Maybe Text)
  , _mtStartTime                 :: !(Maybe POSIX)
  , _mtGameSessionConnectionInfo :: !(Maybe GameSessionConnectionInfo)
  , _mtTicketId                  :: !(Maybe Text)
  , _mtEstimatedWaitTime         :: !(Maybe Nat)
  , _mtStatusMessage             :: !(Maybe Text)
  , _mtEndTime                   :: !(Maybe POSIX)
  , _mtStatusReason              :: !(Maybe Text)
  , _mtPlayers                   :: !(Maybe [Player])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MatchmakingTicket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtStatus' - Current status of the matchmaking request.     * __QUEUED__ -- The matchmaking request has been received and is currently waiting to be processed.     * __SEARCHING__ -- The matchmaking request is currently being processed.      * __REQUIRES_ACCEPTANCE__ -- A match has been proposed and the players must accept the match (see 'AcceptMatch' ). This status is used only with requests that use a matchmaking configuration with a player acceptance requirement.     * __PLACING__ -- The FlexMatch engine has matched players and is in the process of placing a new game session for the match.     * __COMPLETED__ -- Players have been matched and a game session is ready to host the players. A ticket in this state contains the necessary connection information for players.     * __FAILED__ -- The matchmaking request was not completed. Tickets with players who fail to accept a proposed match are placed in @FAILED@ status.     * __CANCELLED__ -- The matchmaking request was canceled with a call to 'StopMatchmaking' .     * __TIMED_OUT__ -- The matchmaking request was not successful within the duration specified in the matchmaking configuration.
--
-- * 'mtConfigurationName' - Name of the 'MatchmakingConfiguration' that is used with this ticket. Matchmaking configurations determine how players are grouped into a match and how a new game session is created for the match.
--
-- * 'mtStartTime' - Time stamp indicating when this matchmaking request was received. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'mtGameSessionConnectionInfo' - Identifier and connection information of the game session created for the match. This information is added to the ticket only after the matchmaking request has been successfully completed.
--
-- * 'mtTicketId' - Unique identifier for a matchmaking ticket.
--
-- * 'mtEstimatedWaitTime' - Average amount of time (in seconds) that players are currently waiting for a match. If there is not enough recent data, this property may be empty.
--
-- * 'mtStatusMessage' - Additional information about the current status.
--
-- * 'mtEndTime' - Time stamp indicating when this matchmaking request stopped being processed due to success, failure, or cancellation. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'mtStatusReason' - Code to explain the current status. For example, a status reason may indicate when a ticket has returned to @SEARCHING@ status after a proposed match fails to receive player acceptances.
--
-- * 'mtPlayers' - A set of @Player@ objects, each representing a player to find matches for. Players are identified by a unique player ID and may include latency data for use during matchmaking. If the ticket is in status @COMPLETED@ , the @Player@ objects include the team the players were assigned to in the resulting match.
matchmakingTicket
    :: MatchmakingTicket
matchmakingTicket =
  MatchmakingTicket'
    { _mtStatus = Nothing
    , _mtConfigurationName = Nothing
    , _mtStartTime = Nothing
    , _mtGameSessionConnectionInfo = Nothing
    , _mtTicketId = Nothing
    , _mtEstimatedWaitTime = Nothing
    , _mtStatusMessage = Nothing
    , _mtEndTime = Nothing
    , _mtStatusReason = Nothing
    , _mtPlayers = Nothing
    }


-- | Current status of the matchmaking request.     * __QUEUED__ -- The matchmaking request has been received and is currently waiting to be processed.     * __SEARCHING__ -- The matchmaking request is currently being processed.      * __REQUIRES_ACCEPTANCE__ -- A match has been proposed and the players must accept the match (see 'AcceptMatch' ). This status is used only with requests that use a matchmaking configuration with a player acceptance requirement.     * __PLACING__ -- The FlexMatch engine has matched players and is in the process of placing a new game session for the match.     * __COMPLETED__ -- Players have been matched and a game session is ready to host the players. A ticket in this state contains the necessary connection information for players.     * __FAILED__ -- The matchmaking request was not completed. Tickets with players who fail to accept a proposed match are placed in @FAILED@ status.     * __CANCELLED__ -- The matchmaking request was canceled with a call to 'StopMatchmaking' .     * __TIMED_OUT__ -- The matchmaking request was not successful within the duration specified in the matchmaking configuration.
mtStatus :: Lens' MatchmakingTicket (Maybe MatchmakingConfigurationStatus)
mtStatus = lens _mtStatus (\ s a -> s{_mtStatus = a})

-- | Name of the 'MatchmakingConfiguration' that is used with this ticket. Matchmaking configurations determine how players are grouped into a match and how a new game session is created for the match.
mtConfigurationName :: Lens' MatchmakingTicket (Maybe Text)
mtConfigurationName = lens _mtConfigurationName (\ s a -> s{_mtConfigurationName = a})

-- | Time stamp indicating when this matchmaking request was received. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
mtStartTime :: Lens' MatchmakingTicket (Maybe UTCTime)
mtStartTime = lens _mtStartTime (\ s a -> s{_mtStartTime = a}) . mapping _Time

-- | Identifier and connection information of the game session created for the match. This information is added to the ticket only after the matchmaking request has been successfully completed.
mtGameSessionConnectionInfo :: Lens' MatchmakingTicket (Maybe GameSessionConnectionInfo)
mtGameSessionConnectionInfo = lens _mtGameSessionConnectionInfo (\ s a -> s{_mtGameSessionConnectionInfo = a})

-- | Unique identifier for a matchmaking ticket.
mtTicketId :: Lens' MatchmakingTicket (Maybe Text)
mtTicketId = lens _mtTicketId (\ s a -> s{_mtTicketId = a})

-- | Average amount of time (in seconds) that players are currently waiting for a match. If there is not enough recent data, this property may be empty.
mtEstimatedWaitTime :: Lens' MatchmakingTicket (Maybe Natural)
mtEstimatedWaitTime = lens _mtEstimatedWaitTime (\ s a -> s{_mtEstimatedWaitTime = a}) . mapping _Nat

-- | Additional information about the current status.
mtStatusMessage :: Lens' MatchmakingTicket (Maybe Text)
mtStatusMessage = lens _mtStatusMessage (\ s a -> s{_mtStatusMessage = a})

-- | Time stamp indicating when this matchmaking request stopped being processed due to success, failure, or cancellation. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
mtEndTime :: Lens' MatchmakingTicket (Maybe UTCTime)
mtEndTime = lens _mtEndTime (\ s a -> s{_mtEndTime = a}) . mapping _Time

-- | Code to explain the current status. For example, a status reason may indicate when a ticket has returned to @SEARCHING@ status after a proposed match fails to receive player acceptances.
mtStatusReason :: Lens' MatchmakingTicket (Maybe Text)
mtStatusReason = lens _mtStatusReason (\ s a -> s{_mtStatusReason = a})

-- | A set of @Player@ objects, each representing a player to find matches for. Players are identified by a unique player ID and may include latency data for use during matchmaking. If the ticket is in status @COMPLETED@ , the @Player@ objects include the team the players were assigned to in the resulting match.
mtPlayers :: Lens' MatchmakingTicket [Player]
mtPlayers = lens _mtPlayers (\ s a -> s{_mtPlayers = a}) . _Default . _Coerce

instance FromJSON MatchmakingTicket where
        parseJSON
          = withObject "MatchmakingTicket"
              (\ x ->
                 MatchmakingTicket' <$>
                   (x .:? "Status") <*> (x .:? "ConfigurationName") <*>
                     (x .:? "StartTime")
                     <*> (x .:? "GameSessionConnectionInfo")
                     <*> (x .:? "TicketId")
                     <*> (x .:? "EstimatedWaitTime")
                     <*> (x .:? "StatusMessage")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "StatusReason")
                     <*> (x .:? "Players" .!= mempty))

instance Hashable MatchmakingTicket where

instance NFData MatchmakingTicket where

-- | Information about a player session that was created as part of a 'StartGameSessionPlacement' request. This object contains only the player ID and player session ID. To retrieve full details on a player session, call 'DescribePlayerSessions' with the player session ID.
--
--
-- Player-session-related operations include:
--
--     * 'CreatePlayerSession'
--
--     * 'CreatePlayerSessions'
--
--     * 'DescribePlayerSessions'
--
--     * Game session placements
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
--
--
-- /See:/ 'placedPlayerSession' smart constructor.
data PlacedPlayerSession = PlacedPlayerSession'
  { _ppsPlayerSessionId :: !(Maybe Text)
  , _ppsPlayerId        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlacedPlayerSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppsPlayerSessionId' - Unique identifier for a player session.
--
-- * 'ppsPlayerId' - Unique identifier for a player that is associated with this player session.
placedPlayerSession
    :: PlacedPlayerSession
placedPlayerSession =
  PlacedPlayerSession' {_ppsPlayerSessionId = Nothing, _ppsPlayerId = Nothing}


-- | Unique identifier for a player session.
ppsPlayerSessionId :: Lens' PlacedPlayerSession (Maybe Text)
ppsPlayerSessionId = lens _ppsPlayerSessionId (\ s a -> s{_ppsPlayerSessionId = a})

-- | Unique identifier for a player that is associated with this player session.
ppsPlayerId :: Lens' PlacedPlayerSession (Maybe Text)
ppsPlayerId = lens _ppsPlayerId (\ s a -> s{_ppsPlayerId = a})

instance FromJSON PlacedPlayerSession where
        parseJSON
          = withObject "PlacedPlayerSession"
              (\ x ->
                 PlacedPlayerSession' <$>
                   (x .:? "PlayerSessionId") <*> (x .:? "PlayerId"))

instance Hashable PlacedPlayerSession where

instance NFData PlacedPlayerSession where

-- | Represents a player in matchmaking. When starting a matchmaking request, a player has a player ID, attributes, and may have latency data. Team information is added after a match has been successfully completed.
--
--
--
-- /See:/ 'player' smart constructor.
data Player = Player'
  { _pPlayerAttributes :: !(Maybe (Map Text AttributeValue))
  , _pTeam             :: !(Maybe Text)
  , _pPlayerId         :: !(Maybe Text)
  , _pLatencyInMs      :: !(Maybe (Map Text Nat))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Player' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPlayerAttributes' - Collection of key:value pairs containing player information for use in matchmaking. Player attribute keys must match the /playerAttributes/ used in a matchmaking rule set. Example: @"PlayerAttributes": {"skill": {"N": "23"}, "gameMode": {"S": "deathmatch"}}@ .
--
-- * 'pTeam' - Name of the team that the player is assigned to in a match. Team names are defined in a matchmaking rule set.
--
-- * 'pPlayerId' - Unique identifier for a player
--
-- * 'pLatencyInMs' - Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS regions. If this property is present, FlexMatch considers placing the match only in regions for which latency is reported.  If a matchmaker has a rule that evaluates player latency, players must report latency in order to be matched. If no latency is reported in this scenario, FlexMatch assumes that no regions are available to the player and the ticket is not matchable.
player
    :: Player
player =
  Player'
    { _pPlayerAttributes = Nothing
    , _pTeam = Nothing
    , _pPlayerId = Nothing
    , _pLatencyInMs = Nothing
    }


-- | Collection of key:value pairs containing player information for use in matchmaking. Player attribute keys must match the /playerAttributes/ used in a matchmaking rule set. Example: @"PlayerAttributes": {"skill": {"N": "23"}, "gameMode": {"S": "deathmatch"}}@ .
pPlayerAttributes :: Lens' Player (HashMap Text AttributeValue)
pPlayerAttributes = lens _pPlayerAttributes (\ s a -> s{_pPlayerAttributes = a}) . _Default . _Map

-- | Name of the team that the player is assigned to in a match. Team names are defined in a matchmaking rule set.
pTeam :: Lens' Player (Maybe Text)
pTeam = lens _pTeam (\ s a -> s{_pTeam = a})

-- | Unique identifier for a player
pPlayerId :: Lens' Player (Maybe Text)
pPlayerId = lens _pPlayerId (\ s a -> s{_pPlayerId = a})

-- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS regions. If this property is present, FlexMatch considers placing the match only in regions for which latency is reported.  If a matchmaker has a rule that evaluates player latency, players must report latency in order to be matched. If no latency is reported in this scenario, FlexMatch assumes that no regions are available to the player and the ticket is not matchable.
pLatencyInMs :: Lens' Player (HashMap Text Natural)
pLatencyInMs = lens _pLatencyInMs (\ s a -> s{_pLatencyInMs = a}) . _Default . _Map

instance FromJSON Player where
        parseJSON
          = withObject "Player"
              (\ x ->
                 Player' <$>
                   (x .:? "PlayerAttributes" .!= mempty) <*>
                     (x .:? "Team")
                     <*> (x .:? "PlayerId")
                     <*> (x .:? "LatencyInMs" .!= mempty))

instance Hashable Player where

instance NFData Player where

instance ToJSON Player where
        toJSON Player'{..}
          = object
              (catMaybes
                 [("PlayerAttributes" .=) <$> _pPlayerAttributes,
                  ("Team" .=) <$> _pTeam,
                  ("PlayerId" .=) <$> _pPlayerId,
                  ("LatencyInMs" .=) <$> _pLatencyInMs])

-- | Regional latency information for a player, used when requesting a new game session with 'StartGameSessionPlacement' . This value indicates the amount of time lag that exists when the player is connected to a fleet in the specified region. The relative difference between a player's latency values for multiple regions are used to determine which fleets are best suited to place a new game session for the player.
--
--
--
-- /See:/ 'playerLatency' smart constructor.
data PlayerLatency = PlayerLatency'
  { _plLatencyInMilliseconds :: !(Maybe Double)
  , _plRegionIdentifier      :: !(Maybe Text)
  , _plPlayerId              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
plLatencyInMilliseconds = lens _plLatencyInMilliseconds (\ s a -> s{_plLatencyInMilliseconds = a})

-- | Name of the region that is associated with the latency value.
plRegionIdentifier :: Lens' PlayerLatency (Maybe Text)
plRegionIdentifier = lens _plRegionIdentifier (\ s a -> s{_plRegionIdentifier = a})

-- | Unique identifier for a player associated with the latency data.
plPlayerId :: Lens' PlayerLatency (Maybe Text)
plPlayerId = lens _plPlayerId (\ s a -> s{_plPlayerId = a})

instance FromJSON PlayerLatency where
        parseJSON
          = withObject "PlayerLatency"
              (\ x ->
                 PlayerLatency' <$>
                   (x .:? "LatencyInMilliseconds") <*>
                     (x .:? "RegionIdentifier")
                     <*> (x .:? "PlayerId"))

instance Hashable PlayerLatency where

instance NFData PlayerLatency where

instance ToJSON PlayerLatency where
        toJSON PlayerLatency'{..}
          = object
              (catMaybes
                 [("LatencyInMilliseconds" .=) <$>
                    _plLatencyInMilliseconds,
                  ("RegionIdentifier" .=) <$> _plRegionIdentifier,
                  ("PlayerId" .=) <$> _plPlayerId])

-- | Queue setting that determines the highest latency allowed for individual players when placing a game session. When a latency policy is in force, a game session cannot be placed at any destination in a region where a player is reporting latency higher than the cap. Latency policies are only enforced when the placement request contains player latency information.
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
-- /See:/ 'playerLatencyPolicy' smart constructor.
data PlayerLatencyPolicy = PlayerLatencyPolicy'
  { _plpPolicyDurationSeconds                      :: !(Maybe Nat)
  , _plpMaximumIndividualPlayerLatencyMilliseconds :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlayerLatencyPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plpPolicyDurationSeconds' - The length of time, in seconds, that the policy is enforced while placing a new game session. A null value for this property means that the policy is enforced until the queue times out.
--
-- * 'plpMaximumIndividualPlayerLatencyMilliseconds' - The maximum latency value that is allowed for any player, in milliseconds. All policies must have a value set for this property.
playerLatencyPolicy
    :: PlayerLatencyPolicy
playerLatencyPolicy =
  PlayerLatencyPolicy'
    { _plpPolicyDurationSeconds = Nothing
    , _plpMaximumIndividualPlayerLatencyMilliseconds = Nothing
    }


-- | The length of time, in seconds, that the policy is enforced while placing a new game session. A null value for this property means that the policy is enforced until the queue times out.
plpPolicyDurationSeconds :: Lens' PlayerLatencyPolicy (Maybe Natural)
plpPolicyDurationSeconds = lens _plpPolicyDurationSeconds (\ s a -> s{_plpPolicyDurationSeconds = a}) . mapping _Nat

-- | The maximum latency value that is allowed for any player, in milliseconds. All policies must have a value set for this property.
plpMaximumIndividualPlayerLatencyMilliseconds :: Lens' PlayerLatencyPolicy (Maybe Natural)
plpMaximumIndividualPlayerLatencyMilliseconds = lens _plpMaximumIndividualPlayerLatencyMilliseconds (\ s a -> s{_plpMaximumIndividualPlayerLatencyMilliseconds = a}) . mapping _Nat

instance FromJSON PlayerLatencyPolicy where
        parseJSON
          = withObject "PlayerLatencyPolicy"
              (\ x ->
                 PlayerLatencyPolicy' <$>
                   (x .:? "PolicyDurationSeconds") <*>
                     (x .:? "MaximumIndividualPlayerLatencyMilliseconds"))

instance Hashable PlayerLatencyPolicy where

instance NFData PlayerLatencyPolicy where

instance ToJSON PlayerLatencyPolicy where
        toJSON PlayerLatencyPolicy'{..}
          = object
              (catMaybes
                 [("PolicyDurationSeconds" .=) <$>
                    _plpPolicyDurationSeconds,
                  ("MaximumIndividualPlayerLatencyMilliseconds" .=) <$>
                    _plpMaximumIndividualPlayerLatencyMilliseconds])

-- | Properties describing a player session. Player session objects are created either by creating a player session for a specific game session, or as part of a game session placement. A player session represents either a player reservation for a game session (status @RESERVED@ ) or actual player activity in a game session (status @ACTIVE@ ). A player session object (including player data) is automatically passed to a game session when the player connects to the game session and is validated.
--
--
-- When a player disconnects, the player session status changes to @COMPLETED@ . Once the session ends, the player session object is retained for 30 days and then removed.
--
-- Player-session-related operations include:
--
--     * 'CreatePlayerSession'
--
--     * 'CreatePlayerSessions'
--
--     * 'DescribePlayerSessions'
--
--     * Game session placements
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlayerSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'psStatus' - Current status of the player session. Possible player session statuses include the following:     * __RESERVED__ -- The player session request has been received, but the player has not yet connected to the server process and/or been validated.      * __ACTIVE__ -- The player has been validated by the server process and is currently connected.     * __COMPLETED__ -- The player connection has been dropped.     * __TIMEDOUT__ -- A player session request was received, but the player did not connect and/or was not validated within the timeout limit (60 seconds).
--
-- * 'psIPAddress' - IP address of the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
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
psCreationTime = lens _psCreationTime (\ s a -> s{_psCreationTime = a}) . mapping _Time

-- | Current status of the player session. Possible player session statuses include the following:     * __RESERVED__ -- The player session request has been received, but the player has not yet connected to the server process and/or been validated.      * __ACTIVE__ -- The player has been validated by the server process and is currently connected.     * __COMPLETED__ -- The player connection has been dropped.     * __TIMEDOUT__ -- A player session request was received, but the player did not connect and/or was not validated within the timeout limit (60 seconds).
psStatus :: Lens' PlayerSession (Maybe PlayerSessionStatus)
psStatus = lens _psStatus (\ s a -> s{_psStatus = a})

-- | IP address of the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
psIPAddress :: Lens' PlayerSession (Maybe Text)
psIPAddress = lens _psIPAddress (\ s a -> s{_psIPAddress = a})

-- | Unique identifier for the game session that the player session is connected to.
psGameSessionId :: Lens' PlayerSession (Maybe Text)
psGameSessionId = lens _psGameSessionId (\ s a -> s{_psGameSessionId = a})

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
psTerminationTime :: Lens' PlayerSession (Maybe UTCTime)
psTerminationTime = lens _psTerminationTime (\ s a -> s{_psTerminationTime = a}) . mapping _Time

-- | Unique identifier for a player session.
psPlayerSessionId :: Lens' PlayerSession (Maybe Text)
psPlayerSessionId = lens _psPlayerSessionId (\ s a -> s{_psPlayerSessionId = a})

-- | Unique identifier for a fleet that the player's game session is running on.
psFleetId :: Lens' PlayerSession (Maybe Text)
psFleetId = lens _psFleetId (\ s a -> s{_psFleetId = a})

-- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
psPlayerData :: Lens' PlayerSession (Maybe Text)
psPlayerData = lens _psPlayerData (\ s a -> s{_psPlayerData = a})

-- | Unique identifier for a player that is associated with this player session.
psPlayerId :: Lens' PlayerSession (Maybe Text)
psPlayerId = lens _psPlayerId (\ s a -> s{_psPlayerId = a})

-- | Port number for the game session. To connect to a Amazon GameLift server process, an app needs both the IP address and port number.
psPort :: Lens' PlayerSession (Maybe Natural)
psPort = lens _psPort (\ s a -> s{_psPort = a}) . mapping _Nat

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

instance Hashable PlayerSession where

instance NFData PlayerSession where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
rclpNewGameSessionsPerCreator = lens _rclpNewGameSessionsPerCreator (\ s a -> s{_rclpNewGameSessionsPerCreator = a}) . mapping _Nat

-- | Time span used in evaluating the resource creation limit policy.
rclpPolicyPeriodInMinutes :: Lens' ResourceCreationLimitPolicy (Maybe Natural)
rclpPolicyPeriodInMinutes = lens _rclpPolicyPeriodInMinutes (\ s a -> s{_rclpPolicyPeriodInMinutes = a}) . mapping _Nat

instance FromJSON ResourceCreationLimitPolicy where
        parseJSON
          = withObject "ResourceCreationLimitPolicy"
              (\ x ->
                 ResourceCreationLimitPolicy' <$>
                   (x .:? "NewGameSessionsPerCreator") <*>
                     (x .:? "PolicyPeriodInMinutes"))

instance Hashable ResourceCreationLimitPolicy where

instance NFData ResourceCreationLimitPolicy where

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
-- Fleet-related operations include:
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
-- /See:/ 'routingStrategy' smart constructor.
data RoutingStrategy = RoutingStrategy'
  { _rsType    :: !(Maybe RoutingStrategyType)
  , _rsMessage :: !(Maybe Text)
  , _rsFleetId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RoutingStrategy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsType' - Type of routing strategy. Possible routing types include the following:     * __SIMPLE__ -- The alias resolves to one specific fleet. Use this type when routing to active fleets.     * __TERMINAL__ -- The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
--
-- * 'rsMessage' - Message text to be used with a terminal routing strategy.
--
-- * 'rsFleetId' - Unique identifier for a fleet that the alias points to.
routingStrategy
    :: RoutingStrategy
routingStrategy =
  RoutingStrategy'
    {_rsType = Nothing, _rsMessage = Nothing, _rsFleetId = Nothing}


-- | Type of routing strategy. Possible routing types include the following:     * __SIMPLE__ -- The alias resolves to one specific fleet. Use this type when routing to active fleets.     * __TERMINAL__ -- The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
rsType :: Lens' RoutingStrategy (Maybe RoutingStrategyType)
rsType = lens _rsType (\ s a -> s{_rsType = a})

-- | Message text to be used with a terminal routing strategy.
rsMessage :: Lens' RoutingStrategy (Maybe Text)
rsMessage = lens _rsMessage (\ s a -> s{_rsMessage = a})

-- | Unique identifier for a fleet that the alias points to.
rsFleetId :: Lens' RoutingStrategy (Maybe Text)
rsFleetId = lens _rsFleetId (\ s a -> s{_rsFleetId = a})

instance FromJSON RoutingStrategy where
        parseJSON
          = withObject "RoutingStrategy"
              (\ x ->
                 RoutingStrategy' <$>
                   (x .:? "Type") <*> (x .:? "Message") <*>
                     (x .:? "FleetId"))

instance Hashable RoutingStrategy where

instance NFData RoutingStrategy where

instance ToJSON RoutingStrategy where
        toJSON RoutingStrategy'{..}
          = object
              (catMaybes
                 [("Type" .=) <$> _rsType,
                  ("Message" .=) <$> _rsMessage,
                  ("FleetId" .=) <$> _rsFleetId])

-- | A collection of server process configurations that describe what processes to run on each instance in a fleet. All fleets must have a run-time configuration. Each instance in the fleet launches the server processes specified in the run-time configuration and launches new ones as existing processes end. Each instance regularly checks for an updated run-time configuration and follows the new instructions.
--
--
-- The run-time configuration enables the instances in a fleet to run multiple processes simultaneously. Potential scenarios are as follows: (1) Run multiple processes of a single game server executable to maximize usage of your hosting resources. (2) Run one or more processes of different build executables, such as your game server executable and a related program, or two or more different versions of a game server. (3) Run multiple processes of a single game server but with different launch parameters, for example to run one process on each instance in debug mode.
--
-- A Amazon GameLift instance is limited to 50 processes running simultaneously. A run-time configuration must specify fewer than this limit. To calculate the total number of processes specified in a run-time configuration, add the values of the @ConcurrentExecutions@ parameter for each @'ServerProcess' @ object in the run-time configuration.
--
-- Fleet-related operations include:
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
-- /See:/ 'runtimeConfiguration' smart constructor.
data RuntimeConfiguration = RuntimeConfiguration'
  { _rcGameSessionActivationTimeoutSeconds :: !(Maybe Nat)
  , _rcServerProcesses                     :: !(Maybe (List1 ServerProcess))
  , _rcMaxConcurrentGameSessionActivations :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RuntimeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcGameSessionActivationTimeoutSeconds' - Maximum amount of time (in seconds) that a game session can remain in status @ACTIVATING@ . If the game session is not active before the timeout, activation is terminated and the game session status is changed to @TERMINATED@ .
--
-- * 'rcServerProcesses' - Collection of server process configurations that describe which server processes to run on each instance in a fleet.
--
-- * 'rcMaxConcurrentGameSessionActivations' - Maximum number of game sessions with status @ACTIVATING@ to allow on an instance simultaneously. This setting limits the amount of instance resources that can be used for new game activations at any one time.
runtimeConfiguration
    :: RuntimeConfiguration
runtimeConfiguration =
  RuntimeConfiguration'
    { _rcGameSessionActivationTimeoutSeconds = Nothing
    , _rcServerProcesses = Nothing
    , _rcMaxConcurrentGameSessionActivations = Nothing
    }


-- | Maximum amount of time (in seconds) that a game session can remain in status @ACTIVATING@ . If the game session is not active before the timeout, activation is terminated and the game session status is changed to @TERMINATED@ .
rcGameSessionActivationTimeoutSeconds :: Lens' RuntimeConfiguration (Maybe Natural)
rcGameSessionActivationTimeoutSeconds = lens _rcGameSessionActivationTimeoutSeconds (\ s a -> s{_rcGameSessionActivationTimeoutSeconds = a}) . mapping _Nat

-- | Collection of server process configurations that describe which server processes to run on each instance in a fleet.
rcServerProcesses :: Lens' RuntimeConfiguration (Maybe (NonEmpty ServerProcess))
rcServerProcesses = lens _rcServerProcesses (\ s a -> s{_rcServerProcesses = a}) . mapping _List1

-- | Maximum number of game sessions with status @ACTIVATING@ to allow on an instance simultaneously. This setting limits the amount of instance resources that can be used for new game activations at any one time.
rcMaxConcurrentGameSessionActivations :: Lens' RuntimeConfiguration (Maybe Natural)
rcMaxConcurrentGameSessionActivations = lens _rcMaxConcurrentGameSessionActivations (\ s a -> s{_rcMaxConcurrentGameSessionActivations = a}) . mapping _Nat

instance FromJSON RuntimeConfiguration where
        parseJSON
          = withObject "RuntimeConfiguration"
              (\ x ->
                 RuntimeConfiguration' <$>
                   (x .:? "GameSessionActivationTimeoutSeconds") <*>
                     (x .:? "ServerProcesses")
                     <*> (x .:? "MaxConcurrentGameSessionActivations"))

instance Hashable RuntimeConfiguration where

instance NFData RuntimeConfiguration where

instance ToJSON RuntimeConfiguration where
        toJSON RuntimeConfiguration'{..}
          = object
              (catMaybes
                 [("GameSessionActivationTimeoutSeconds" .=) <$>
                    _rcGameSessionActivationTimeoutSeconds,
                  ("ServerProcesses" .=) <$> _rcServerProcesses,
                  ("MaxConcurrentGameSessionActivations" .=) <$>
                    _rcMaxConcurrentGameSessionActivations])

-- | Location in Amazon Simple Storage Service (Amazon S3) where build files can be stored for access by Amazon GameLift. This location is specified in a 'CreateBuild' request. For more details, see the <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build Create a Build with Files in Amazon S3> .
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slBucket  :: !(Maybe Text)
  , _slKey     :: !(Maybe Text)
  , _slRoleARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
  S3Location' {_slBucket = Nothing, _slKey = Nothing, _slRoleARN = Nothing}


-- | Amazon S3 bucket identifier. This is the name of your S3 bucket.
slBucket :: Lens' S3Location (Maybe Text)
slBucket = lens _slBucket (\ s a -> s{_slBucket = a})

-- | Name of the zip file containing your build files.
slKey :: Lens' S3Location (Maybe Text)
slKey = lens _slKey (\ s a -> s{_slKey = a})

-- | Amazon Resource Name (<http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for the access role that allows Amazon GameLift to access your S3 bucket.
slRoleARN :: Lens' S3Location (Maybe Text)
slRoleARN = lens _slRoleARN (\ s a -> s{_slRoleARN = a})

instance FromJSON S3Location where
        parseJSON
          = withObject "S3Location"
              (\ x ->
                 S3Location' <$>
                   (x .:? "Bucket") <*> (x .:? "Key") <*>
                     (x .:? "RoleArn"))

instance Hashable S3Location where

instance NFData S3Location where

instance ToJSON S3Location where
        toJSON S3Location'{..}
          = object
              (catMaybes
                 [("Bucket" .=) <$> _slBucket, ("Key" .=) <$> _slKey,
                  ("RoleArn" .=) <$> _slRoleARN])

-- | Rule that controls how a fleet is scaled. Scaling policies are uniquely identified by the combination of name and fleet ID.
--
--
-- Operations related to fleet capacity scaling include:
--
--     * 'DescribeFleetCapacity'
--
--     * 'UpdateFleetCapacity'
--
--     * 'DescribeEC2InstanceLimits'
--
--     * Manage scaling policies:
--
--     * 'PutScalingPolicy' (auto-scaling)
--
--     * 'DescribeScalingPolicies' (auto-scaling)
--
--     * 'DeleteScalingPolicy' (auto-scaling)
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
-- /See:/ 'scalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { _spStatus                :: !(Maybe ScalingStatusType)
  , _spScalingAdjustmentType :: !(Maybe ScalingAdjustmentType)
  , _spEvaluationPeriods     :: !(Maybe Nat)
  , _spPolicyType            :: !(Maybe PolicyType)
  , _spMetricName            :: !(Maybe MetricName)
  , _spComparisonOperator    :: !(Maybe ComparisonOperatorType)
  , _spName                  :: !(Maybe Text)
  , _spThreshold             :: !(Maybe Double)
  , _spScalingAdjustment     :: !(Maybe Int)
  , _spFleetId               :: !(Maybe Text)
  , _spTargetConfiguration   :: !(Maybe TargetConfiguration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spStatus' - Current status of the scaling policy. The scaling policy can be in force only when in an @ACTIVE@ status. Scaling policies can be suspended for individual fleets (see 'StopFleetActions' ; if suspended for a fleet, the policy status does not change. View a fleet's stopped actions by calling 'DescribeFleetCapacity' .     * __ACTIVE__ -- The scaling policy can be used for auto-scaling a fleet.     * __UPDATE_REQUESTED__ -- A request to update the scaling policy has been received.     * __UPDATING__ -- A change is being made to the scaling policy.     * __DELETE_REQUESTED__ -- A request to delete the scaling policy has been received.     * __DELETING__ -- The scaling policy is being deleted.     * __DELETED__ -- The scaling policy has been deleted.     * __ERROR__ -- An error occurred in creating the policy. It should be removed and recreated.
--
-- * 'spScalingAdjustmentType' - Type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):     * __ChangeInCapacity__ -- add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.     * __ExactCapacity__ -- set the instance count to the scaling adjustment value.     * __PercentChangeInCapacity__ -- increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down.
--
-- * 'spEvaluationPeriods' - Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
--
-- * 'spPolicyType' - Type of scaling policy to create. For a target-based policy, set the parameter /MetricName/ to 'PercentAvailableGameSessions' and specify a /TargetConfiguration/ . For a rule-based policy set the following parameters: /MetricName/ , /ComparisonOperator/ , /Threshold/ , /EvaluationPeriods/ , /ScalingAdjustmentType/ , and /ScalingAdjustment/ .
--
-- * 'spMetricName' - Name of the Amazon GameLift-defined metric that is used to trigger a scaling adjustment. For detailed descriptions of fleet metrics, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch> .      * __ActivatingGameSessions__ -- Game sessions in the process of being created.     * __ActiveGameSessions__ -- Game sessions that are currently running.     * __ActiveInstances__ -- Fleet instances that are currently running at least one game session.     * __AvailableGameSessions__ -- Additional game sessions that fleet could host simultaneously, given current capacity.     * __AvailablePlayerSessions__ -- Empty player slots in currently active game sessions. This includes game sessions that are not currently accepting players. Reserved player slots are not included.     * __CurrentPlayerSessions__ -- Player slots in active game sessions that are being used by a player or are reserved for a player.      * __IdleInstances__ -- Active instances that are currently hosting zero game sessions.      * __PercentAvailableGameSessions__ -- Unused percentage of the total number of game sessions that a fleet could host simultaneously, given current capacity. Use this metric for a target-based scaling policy.     * __PercentIdleInstances__ -- Percentage of the total number of active instances that are hosting zero game sessions.     * __QueueDepth__ -- Pending game session placement requests, in any queue, where the current fleet is the top-priority destination.     * __WaitTime__ -- Current wait time for pending game session placement requests, in any queue, where the current fleet is the top-priority destination.
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
--
-- * 'spTargetConfiguration' - Object that contains settings for a target-based scaling policy.
scalingPolicy
    :: ScalingPolicy
scalingPolicy =
  ScalingPolicy'
    { _spStatus = Nothing
    , _spScalingAdjustmentType = Nothing
    , _spEvaluationPeriods = Nothing
    , _spPolicyType = Nothing
    , _spMetricName = Nothing
    , _spComparisonOperator = Nothing
    , _spName = Nothing
    , _spThreshold = Nothing
    , _spScalingAdjustment = Nothing
    , _spFleetId = Nothing
    , _spTargetConfiguration = Nothing
    }


-- | Current status of the scaling policy. The scaling policy can be in force only when in an @ACTIVE@ status. Scaling policies can be suspended for individual fleets (see 'StopFleetActions' ; if suspended for a fleet, the policy status does not change. View a fleet's stopped actions by calling 'DescribeFleetCapacity' .     * __ACTIVE__ -- The scaling policy can be used for auto-scaling a fleet.     * __UPDATE_REQUESTED__ -- A request to update the scaling policy has been received.     * __UPDATING__ -- A change is being made to the scaling policy.     * __DELETE_REQUESTED__ -- A request to delete the scaling policy has been received.     * __DELETING__ -- The scaling policy is being deleted.     * __DELETED__ -- The scaling policy has been deleted.     * __ERROR__ -- An error occurred in creating the policy. It should be removed and recreated.
spStatus :: Lens' ScalingPolicy (Maybe ScalingStatusType)
spStatus = lens _spStatus (\ s a -> s{_spStatus = a})

-- | Type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):     * __ChangeInCapacity__ -- add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.     * __ExactCapacity__ -- set the instance count to the scaling adjustment value.     * __PercentChangeInCapacity__ -- increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down.
spScalingAdjustmentType :: Lens' ScalingPolicy (Maybe ScalingAdjustmentType)
spScalingAdjustmentType = lens _spScalingAdjustmentType (\ s a -> s{_spScalingAdjustmentType = a})

-- | Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
spEvaluationPeriods :: Lens' ScalingPolicy (Maybe Natural)
spEvaluationPeriods = lens _spEvaluationPeriods (\ s a -> s{_spEvaluationPeriods = a}) . mapping _Nat

-- | Type of scaling policy to create. For a target-based policy, set the parameter /MetricName/ to 'PercentAvailableGameSessions' and specify a /TargetConfiguration/ . For a rule-based policy set the following parameters: /MetricName/ , /ComparisonOperator/ , /Threshold/ , /EvaluationPeriods/ , /ScalingAdjustmentType/ , and /ScalingAdjustment/ .
spPolicyType :: Lens' ScalingPolicy (Maybe PolicyType)
spPolicyType = lens _spPolicyType (\ s a -> s{_spPolicyType = a})

-- | Name of the Amazon GameLift-defined metric that is used to trigger a scaling adjustment. For detailed descriptions of fleet metrics, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch> .      * __ActivatingGameSessions__ -- Game sessions in the process of being created.     * __ActiveGameSessions__ -- Game sessions that are currently running.     * __ActiveInstances__ -- Fleet instances that are currently running at least one game session.     * __AvailableGameSessions__ -- Additional game sessions that fleet could host simultaneously, given current capacity.     * __AvailablePlayerSessions__ -- Empty player slots in currently active game sessions. This includes game sessions that are not currently accepting players. Reserved player slots are not included.     * __CurrentPlayerSessions__ -- Player slots in active game sessions that are being used by a player or are reserved for a player.      * __IdleInstances__ -- Active instances that are currently hosting zero game sessions.      * __PercentAvailableGameSessions__ -- Unused percentage of the total number of game sessions that a fleet could host simultaneously, given current capacity. Use this metric for a target-based scaling policy.     * __PercentIdleInstances__ -- Percentage of the total number of active instances that are hosting zero game sessions.     * __QueueDepth__ -- Pending game session placement requests, in any queue, where the current fleet is the top-priority destination.     * __WaitTime__ -- Current wait time for pending game session placement requests, in any queue, where the current fleet is the top-priority destination.
spMetricName :: Lens' ScalingPolicy (Maybe MetricName)
spMetricName = lens _spMetricName (\ s a -> s{_spMetricName = a})

-- | Comparison operator to use when measuring a metric against the threshold value.
spComparisonOperator :: Lens' ScalingPolicy (Maybe ComparisonOperatorType)
spComparisonOperator = lens _spComparisonOperator (\ s a -> s{_spComparisonOperator = a})

-- | Descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
spName :: Lens' ScalingPolicy (Maybe Text)
spName = lens _spName (\ s a -> s{_spName = a})

-- | Metric value used to trigger a scaling event.
spThreshold :: Lens' ScalingPolicy (Maybe Double)
spThreshold = lens _spThreshold (\ s a -> s{_spThreshold = a})

-- | Amount of adjustment to make, based on the scaling adjustment type.
spScalingAdjustment :: Lens' ScalingPolicy (Maybe Int)
spScalingAdjustment = lens _spScalingAdjustment (\ s a -> s{_spScalingAdjustment = a})

-- | Unique identifier for a fleet that is associated with this scaling policy.
spFleetId :: Lens' ScalingPolicy (Maybe Text)
spFleetId = lens _spFleetId (\ s a -> s{_spFleetId = a})

-- | Object that contains settings for a target-based scaling policy.
spTargetConfiguration :: Lens' ScalingPolicy (Maybe TargetConfiguration)
spTargetConfiguration = lens _spTargetConfiguration (\ s a -> s{_spTargetConfiguration = a})

instance FromJSON ScalingPolicy where
        parseJSON
          = withObject "ScalingPolicy"
              (\ x ->
                 ScalingPolicy' <$>
                   (x .:? "Status") <*> (x .:? "ScalingAdjustmentType")
                     <*> (x .:? "EvaluationPeriods")
                     <*> (x .:? "PolicyType")
                     <*> (x .:? "MetricName")
                     <*> (x .:? "ComparisonOperator")
                     <*> (x .:? "Name")
                     <*> (x .:? "Threshold")
                     <*> (x .:? "ScalingAdjustment")
                     <*> (x .:? "FleetId")
                     <*> (x .:? "TargetConfiguration"))

instance Hashable ScalingPolicy where

instance NFData ScalingPolicy where

-- | A set of instructions for launching server processes on each instance in a fleet. Each instruction set identifies the location of the server executable, optional launch parameters, and the number of server processes with this configuration to maintain concurrently on the instance. Server process configurations make up a fleet's @'RuntimeConfiguration' @ .
--
--
--
-- /See:/ 'serverProcess' smart constructor.
data ServerProcess = ServerProcess'
  { _spParameters           :: !(Maybe Text)
  , _spLaunchPath           :: !Text
  , _spConcurrentExecutions :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
spParameters = lens _spParameters (\ s a -> s{_spParameters = a})

-- | Location of the server executable in a game build. All game builds are installed on instances at the root : for Windows instances @C:\game@ , and for Linux instances @/local/game@ . A Windows game build with an executable file located at @MyGame\latest\server.exe@ must have a launch path of "@C:\game\MyGame\latest\server.exe@ ". A Linux game build with an executable file located at @MyGame/latest/server.exe@ must have a launch path of "@/local/game/MyGame/latest/server.exe@ ".
spLaunchPath :: Lens' ServerProcess Text
spLaunchPath = lens _spLaunchPath (\ s a -> s{_spLaunchPath = a})

-- | Number of server processes using this configuration to run concurrently on an instance.
spConcurrentExecutions :: Lens' ServerProcess Natural
spConcurrentExecutions = lens _spConcurrentExecutions (\ s a -> s{_spConcurrentExecutions = a}) . _Nat

instance FromJSON ServerProcess where
        parseJSON
          = withObject "ServerProcess"
              (\ x ->
                 ServerProcess' <$>
                   (x .:? "Parameters") <*> (x .: "LaunchPath") <*>
                     (x .: "ConcurrentExecutions"))

instance Hashable ServerProcess where

instance NFData ServerProcess where

instance ToJSON ServerProcess where
        toJSON ServerProcess'{..}
          = object
              (catMaybes
                 [("Parameters" .=) <$> _spParameters,
                  Just ("LaunchPath" .= _spLaunchPath),
                  Just
                    ("ConcurrentExecutions" .= _spConcurrentExecutions)])

-- | Settings for a target-based scaling policy (see 'ScalingPolicy' . A target-based policy tracks a particular fleet metric specifies a target value for the metric. As player usage changes, the policy triggers Amazon GameLift to adjust capacity so that the metric returns to the target value. The target configuration specifies settings as needed for the target based policy, including the target value.
--
--
-- Operations related to fleet capacity scaling include:
--
--     * 'DescribeFleetCapacity'
--
--     * 'UpdateFleetCapacity'
--
--     * 'DescribeEC2InstanceLimits'
--
--     * Manage scaling policies:
--
--     * 'PutScalingPolicy' (auto-scaling)
--
--     * 'DescribeScalingPolicies' (auto-scaling)
--
--     * 'DeleteScalingPolicy' (auto-scaling)
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
-- /See:/ 'targetConfiguration' smart constructor.
newtype TargetConfiguration = TargetConfiguration'
  { _tcTargetValue :: Double
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcTargetValue' - Desired value to use with a target-based scaling policy. The value must be relevant for whatever metric the scaling policy is using. For example, in a policy using the metric PercentAvailableGameSessions, the target value should be the preferred size of the fleet's buffer (the percent of capacity that should be idle and ready for new game sessions).
targetConfiguration
    :: Double -- ^ 'tcTargetValue'
    -> TargetConfiguration
targetConfiguration pTargetValue_ =
  TargetConfiguration' {_tcTargetValue = pTargetValue_}


-- | Desired value to use with a target-based scaling policy. The value must be relevant for whatever metric the scaling policy is using. For example, in a policy using the metric PercentAvailableGameSessions, the target value should be the preferred size of the fleet's buffer (the percent of capacity that should be idle and ready for new game sessions).
tcTargetValue :: Lens' TargetConfiguration Double
tcTargetValue = lens _tcTargetValue (\ s a -> s{_tcTargetValue = a})

instance FromJSON TargetConfiguration where
        parseJSON
          = withObject "TargetConfiguration"
              (\ x ->
                 TargetConfiguration' <$> (x .: "TargetValue"))

instance Hashable TargetConfiguration where

instance NFData TargetConfiguration where

instance ToJSON TargetConfiguration where
        toJSON TargetConfiguration'{..}
          = object
              (catMaybes [Just ("TargetValue" .= _tcTargetValue)])

-- | Represents an authorization for a VPC peering connection between the VPC for an Amazon GameLift fleet and another VPC on an account you have access to. This authorization must exist and be valid for the peering connection to be established. Authorizations are valid for 24 hours after they are issued.
--
--
-- VPC peering connection operations include:
--
--     * 'CreateVpcPeeringAuthorization'
--
--     * 'DescribeVpcPeeringAuthorizations'
--
--     * 'DeleteVpcPeeringAuthorization'
--
--     * 'CreateVpcPeeringConnection'
--
--     * 'DescribeVpcPeeringConnections'
--
--     * 'DeleteVpcPeeringConnection'
--
--
--
--
-- /See:/ 'vpcPeeringAuthorization' smart constructor.
data VPCPeeringAuthorization = VPCPeeringAuthorization'
  { _vpaCreationTime         :: !(Maybe POSIX)
  , _vpaPeerVPCId            :: !(Maybe Text)
  , _vpaPeerVPCAWSAccountId  :: !(Maybe Text)
  , _vpaGameLiftAWSAccountId :: !(Maybe Text)
  , _vpaExpirationTime       :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCPeeringAuthorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpaCreationTime' - Time stamp indicating when this authorization was issued. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'vpaPeerVPCId' - Unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same region where your fleet is deployed. To get VPC information, including IDs, use the Virtual Private Cloud service tools, including the VPC Dashboard in the AWS Management Console.
--
-- * 'vpaPeerVPCAWSAccountId' -
--
-- * 'vpaGameLiftAWSAccountId' - Unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
--
-- * 'vpaExpirationTime' - Time stamp indicating when this authorization expires (24 hours after issuance). Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
vpcPeeringAuthorization
    :: VPCPeeringAuthorization
vpcPeeringAuthorization =
  VPCPeeringAuthorization'
    { _vpaCreationTime = Nothing
    , _vpaPeerVPCId = Nothing
    , _vpaPeerVPCAWSAccountId = Nothing
    , _vpaGameLiftAWSAccountId = Nothing
    , _vpaExpirationTime = Nothing
    }


-- | Time stamp indicating when this authorization was issued. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
vpaCreationTime :: Lens' VPCPeeringAuthorization (Maybe UTCTime)
vpaCreationTime = lens _vpaCreationTime (\ s a -> s{_vpaCreationTime = a}) . mapping _Time

-- | Unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same region where your fleet is deployed. To get VPC information, including IDs, use the Virtual Private Cloud service tools, including the VPC Dashboard in the AWS Management Console.
vpaPeerVPCId :: Lens' VPCPeeringAuthorization (Maybe Text)
vpaPeerVPCId = lens _vpaPeerVPCId (\ s a -> s{_vpaPeerVPCId = a})

-- |
vpaPeerVPCAWSAccountId :: Lens' VPCPeeringAuthorization (Maybe Text)
vpaPeerVPCAWSAccountId = lens _vpaPeerVPCAWSAccountId (\ s a -> s{_vpaPeerVPCAWSAccountId = a})

-- | Unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
vpaGameLiftAWSAccountId :: Lens' VPCPeeringAuthorization (Maybe Text)
vpaGameLiftAWSAccountId = lens _vpaGameLiftAWSAccountId (\ s a -> s{_vpaGameLiftAWSAccountId = a})

-- | Time stamp indicating when this authorization expires (24 hours after issuance). Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
vpaExpirationTime :: Lens' VPCPeeringAuthorization (Maybe UTCTime)
vpaExpirationTime = lens _vpaExpirationTime (\ s a -> s{_vpaExpirationTime = a}) . mapping _Time

instance FromJSON VPCPeeringAuthorization where
        parseJSON
          = withObject "VPCPeeringAuthorization"
              (\ x ->
                 VPCPeeringAuthorization' <$>
                   (x .:? "CreationTime") <*> (x .:? "PeerVpcId") <*>
                     (x .:? "PeerVpcAwsAccountId")
                     <*> (x .:? "GameLiftAwsAccountId")
                     <*> (x .:? "ExpirationTime"))

instance Hashable VPCPeeringAuthorization where

instance NFData VPCPeeringAuthorization where

-- | Represents a peering connection between a VPC on one of your AWS accounts and the VPC for your Amazon GameLift fleets. This record may be for an active peering connection or a pending connection that has not yet been established.
--
--
-- VPC peering connection operations include:
--
--     * 'CreateVpcPeeringAuthorization'
--
--     * 'DescribeVpcPeeringAuthorizations'
--
--     * 'DeleteVpcPeeringAuthorization'
--
--     * 'CreateVpcPeeringConnection'
--
--     * 'DescribeVpcPeeringConnections'
--
--     * 'DeleteVpcPeeringConnection'
--
--
--
--
-- /See:/ 'vpcPeeringConnection' smart constructor.
data VPCPeeringConnection = VPCPeeringConnection'
  { _vpcVPCPeeringConnectionId :: !(Maybe Text)
  , _vpcStatus                 :: !(Maybe VPCPeeringConnectionStatus)
  , _vpcPeerVPCId              :: !(Maybe Text)
  , _vpcIPV4CidrBlock          :: !(Maybe Text)
  , _vpcGameLiftVPCId          :: !(Maybe Text)
  , _vpcFleetId                :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCPeeringConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcVPCPeeringConnectionId' - Unique identifier that is automatically assigned to the connection record. This ID is referenced in VPC peering connection events, and is used when deleting a connection with 'DeleteVpcPeeringConnection' .
--
-- * 'vpcStatus' - Object that contains status information about the connection. Status indicates if a connection is pending, successful, or failed.
--
-- * 'vpcPeerVPCId' - Unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same region where your fleet is deployed. To get VPC information, including IDs, use the Virtual Private Cloud service tools, including the VPC Dashboard in the AWS Management Console.
--
-- * 'vpcIPV4CidrBlock' - CIDR block of IPv4 addresses assigned to the VPC peering connection for the GameLift VPC. The peered VPC also has an IPv4 CIDR block associated with it; these blocks cannot overlap or the peering connection cannot be created.
--
-- * 'vpcGameLiftVPCId' - Unique identifier for the VPC that contains the Amazon GameLift fleet for this connection. This VPC is managed by Amazon GameLift and does not appear in your AWS account.
--
-- * 'vpcFleetId' - Unique identifier for a fleet. This ID determines the ID of the Amazon GameLift VPC for your fleet.
vpcPeeringConnection
    :: VPCPeeringConnection
vpcPeeringConnection =
  VPCPeeringConnection'
    { _vpcVPCPeeringConnectionId = Nothing
    , _vpcStatus = Nothing
    , _vpcPeerVPCId = Nothing
    , _vpcIPV4CidrBlock = Nothing
    , _vpcGameLiftVPCId = Nothing
    , _vpcFleetId = Nothing
    }


-- | Unique identifier that is automatically assigned to the connection record. This ID is referenced in VPC peering connection events, and is used when deleting a connection with 'DeleteVpcPeeringConnection' .
vpcVPCPeeringConnectionId :: Lens' VPCPeeringConnection (Maybe Text)
vpcVPCPeeringConnectionId = lens _vpcVPCPeeringConnectionId (\ s a -> s{_vpcVPCPeeringConnectionId = a})

-- | Object that contains status information about the connection. Status indicates if a connection is pending, successful, or failed.
vpcStatus :: Lens' VPCPeeringConnection (Maybe VPCPeeringConnectionStatus)
vpcStatus = lens _vpcStatus (\ s a -> s{_vpcStatus = a})

-- | Unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same region where your fleet is deployed. To get VPC information, including IDs, use the Virtual Private Cloud service tools, including the VPC Dashboard in the AWS Management Console.
vpcPeerVPCId :: Lens' VPCPeeringConnection (Maybe Text)
vpcPeerVPCId = lens _vpcPeerVPCId (\ s a -> s{_vpcPeerVPCId = a})

-- | CIDR block of IPv4 addresses assigned to the VPC peering connection for the GameLift VPC. The peered VPC also has an IPv4 CIDR block associated with it; these blocks cannot overlap or the peering connection cannot be created.
vpcIPV4CidrBlock :: Lens' VPCPeeringConnection (Maybe Text)
vpcIPV4CidrBlock = lens _vpcIPV4CidrBlock (\ s a -> s{_vpcIPV4CidrBlock = a})

-- | Unique identifier for the VPC that contains the Amazon GameLift fleet for this connection. This VPC is managed by Amazon GameLift and does not appear in your AWS account.
vpcGameLiftVPCId :: Lens' VPCPeeringConnection (Maybe Text)
vpcGameLiftVPCId = lens _vpcGameLiftVPCId (\ s a -> s{_vpcGameLiftVPCId = a})

-- | Unique identifier for a fleet. This ID determines the ID of the Amazon GameLift VPC for your fleet.
vpcFleetId :: Lens' VPCPeeringConnection (Maybe Text)
vpcFleetId = lens _vpcFleetId (\ s a -> s{_vpcFleetId = a})

instance FromJSON VPCPeeringConnection where
        parseJSON
          = withObject "VPCPeeringConnection"
              (\ x ->
                 VPCPeeringConnection' <$>
                   (x .:? "VpcPeeringConnectionId") <*> (x .:? "Status")
                     <*> (x .:? "PeerVpcId")
                     <*> (x .:? "IpV4CidrBlock")
                     <*> (x .:? "GameLiftVpcId")
                     <*> (x .:? "FleetId"))

instance Hashable VPCPeeringConnection where

instance NFData VPCPeeringConnection where

-- | Represents status information for a VPC peering connection. Status is associated with a 'VpcPeeringConnection' object. Status codes and messages are provided from EC2 (see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_VpcPeeringConnectionStateReason.html VpcPeeringConnectionStateReason> ). Connection status information is also communicated as a fleet 'Event' .
--
--
--
-- /See:/ 'vpcPeeringConnectionStatus' smart constructor.
data VPCPeeringConnectionStatus = VPCPeeringConnectionStatus'
  { _vpcsCode    :: !(Maybe Text)
  , _vpcsMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCPeeringConnectionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcsCode' - Code indicating the status of a VPC peering connection.
--
-- * 'vpcsMessage' - Additional messaging associated with the connection status.
vpcPeeringConnectionStatus
    :: VPCPeeringConnectionStatus
vpcPeeringConnectionStatus =
  VPCPeeringConnectionStatus' {_vpcsCode = Nothing, _vpcsMessage = Nothing}


-- | Code indicating the status of a VPC peering connection.
vpcsCode :: Lens' VPCPeeringConnectionStatus (Maybe Text)
vpcsCode = lens _vpcsCode (\ s a -> s{_vpcsCode = a})

-- | Additional messaging associated with the connection status.
vpcsMessage :: Lens' VPCPeeringConnectionStatus (Maybe Text)
vpcsMessage = lens _vpcsMessage (\ s a -> s{_vpcsMessage = a})

instance FromJSON VPCPeeringConnectionStatus where
        parseJSON
          = withObject "VPCPeeringConnectionStatus"
              (\ x ->
                 VPCPeeringConnectionStatus' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable VPCPeeringConnectionStatus where

instance NFData VPCPeeringConnectionStatus where
