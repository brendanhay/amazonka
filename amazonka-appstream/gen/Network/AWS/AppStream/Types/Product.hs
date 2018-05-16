{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.Product where

import Network.AWS.AppStream.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an application in the application catalog.
--
--
--
-- /See:/ 'application' smart constructor.
data Application = Application'
  { _aEnabled          :: !(Maybe Bool)
  , _aLaunchPath       :: !(Maybe Text)
  , _aLaunchParameters :: !(Maybe Text)
  , _aName             :: !(Maybe Text)
  , _aDisplayName      :: !(Maybe Text)
  , _aMetadata         :: !(Maybe (Map Text Text))
  , _aIconURL          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Application' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aEnabled' - If there is a problem, the application can be disabled after image creation.
--
-- * 'aLaunchPath' - The path to the application executable in the instance.
--
-- * 'aLaunchParameters' - The arguments that are passed to the application at launch.
--
-- * 'aName' - The name of the application.
--
-- * 'aDisplayName' - The application name for display.
--
-- * 'aMetadata' - Additional attributes that describe the application.
--
-- * 'aIconURL' - The URL for the application icon. This URL might be time-limited.
application
    :: Application
application =
  Application'
    { _aEnabled = Nothing
    , _aLaunchPath = Nothing
    , _aLaunchParameters = Nothing
    , _aName = Nothing
    , _aDisplayName = Nothing
    , _aMetadata = Nothing
    , _aIconURL = Nothing
    }


-- | If there is a problem, the application can be disabled after image creation.
aEnabled :: Lens' Application (Maybe Bool)
aEnabled = lens _aEnabled (\ s a -> s{_aEnabled = a})

-- | The path to the application executable in the instance.
aLaunchPath :: Lens' Application (Maybe Text)
aLaunchPath = lens _aLaunchPath (\ s a -> s{_aLaunchPath = a})

-- | The arguments that are passed to the application at launch.
aLaunchParameters :: Lens' Application (Maybe Text)
aLaunchParameters = lens _aLaunchParameters (\ s a -> s{_aLaunchParameters = a})

-- | The name of the application.
aName :: Lens' Application (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a})

-- | The application name for display.
aDisplayName :: Lens' Application (Maybe Text)
aDisplayName = lens _aDisplayName (\ s a -> s{_aDisplayName = a})

-- | Additional attributes that describe the application.
aMetadata :: Lens' Application (HashMap Text Text)
aMetadata = lens _aMetadata (\ s a -> s{_aMetadata = a}) . _Default . _Map

-- | The URL for the application icon. This URL might be time-limited.
aIconURL :: Lens' Application (Maybe Text)
aIconURL = lens _aIconURL (\ s a -> s{_aIconURL = a})

instance FromJSON Application where
        parseJSON
          = withObject "Application"
              (\ x ->
                 Application' <$>
                   (x .:? "Enabled") <*> (x .:? "LaunchPath") <*>
                     (x .:? "LaunchParameters")
                     <*> (x .:? "Name")
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "Metadata" .!= mempty)
                     <*> (x .:? "IconURL"))

instance Hashable Application where

instance NFData Application where

-- | Describes the capacity for a fleet.
--
--
--
-- /See:/ 'computeCapacity' smart constructor.
newtype ComputeCapacity = ComputeCapacity'
  { _ccDesiredInstances :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ComputeCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccDesiredInstances' - The desired number of streaming instances.
computeCapacity
    :: Int -- ^ 'ccDesiredInstances'
    -> ComputeCapacity
computeCapacity pDesiredInstances_ =
  ComputeCapacity' {_ccDesiredInstances = pDesiredInstances_}


-- | The desired number of streaming instances.
ccDesiredInstances :: Lens' ComputeCapacity Int
ccDesiredInstances = lens _ccDesiredInstances (\ s a -> s{_ccDesiredInstances = a})

instance Hashable ComputeCapacity where

instance NFData ComputeCapacity where

instance ToJSON ComputeCapacity where
        toJSON ComputeCapacity'{..}
          = object
              (catMaybes
                 [Just ("DesiredInstances" .= _ccDesiredInstances)])

-- | Describes the capacity status for a fleet.
--
--
--
-- /See:/ 'computeCapacityStatus' smart constructor.
data ComputeCapacityStatus = ComputeCapacityStatus'
  { _ccsInUse     :: !(Maybe Int)
  , _ccsRunning   :: !(Maybe Int)
  , _ccsAvailable :: !(Maybe Int)
  , _ccsDesired   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ComputeCapacityStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsInUse' - The number of instances in use for streaming.
--
-- * 'ccsRunning' - The total number of simultaneous streaming instances that are running.
--
-- * 'ccsAvailable' - The number of currently available instances that can be used to stream sessions.
--
-- * 'ccsDesired' - The desired number of streaming instances.
computeCapacityStatus
    :: Int -- ^ 'ccsDesired'
    -> ComputeCapacityStatus
computeCapacityStatus pDesired_ =
  ComputeCapacityStatus'
    { _ccsInUse = Nothing
    , _ccsRunning = Nothing
    , _ccsAvailable = Nothing
    , _ccsDesired = pDesired_
    }


-- | The number of instances in use for streaming.
ccsInUse :: Lens' ComputeCapacityStatus (Maybe Int)
ccsInUse = lens _ccsInUse (\ s a -> s{_ccsInUse = a})

-- | The total number of simultaneous streaming instances that are running.
ccsRunning :: Lens' ComputeCapacityStatus (Maybe Int)
ccsRunning = lens _ccsRunning (\ s a -> s{_ccsRunning = a})

-- | The number of currently available instances that can be used to stream sessions.
ccsAvailable :: Lens' ComputeCapacityStatus (Maybe Int)
ccsAvailable = lens _ccsAvailable (\ s a -> s{_ccsAvailable = a})

-- | The desired number of streaming instances.
ccsDesired :: Lens' ComputeCapacityStatus Int
ccsDesired = lens _ccsDesired (\ s a -> s{_ccsDesired = a})

instance FromJSON ComputeCapacityStatus where
        parseJSON
          = withObject "ComputeCapacityStatus"
              (\ x ->
                 ComputeCapacityStatus' <$>
                   (x .:? "InUse") <*> (x .:? "Running") <*>
                     (x .:? "Available")
                     <*> (x .: "Desired"))

instance Hashable ComputeCapacityStatus where

instance NFData ComputeCapacityStatus where

-- | Configuration information for the directory used to join domains.
--
--
--
-- /See:/ 'directoryConfig' smart constructor.
data DirectoryConfig = DirectoryConfig'
  { _dcCreatedTime :: !(Maybe POSIX)
  , _dcServiceAccountCredentials :: !(Maybe ServiceAccountCredentials)
  , _dcOrganizationalUnitDistinguishedNames :: !(Maybe [Text])
  , _dcDirectoryName :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DirectoryConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcCreatedTime' - The time the directory configuration was created.
--
-- * 'dcServiceAccountCredentials' - The credentials for the service account used by the streaming instance to connect to the directory.
--
-- * 'dcOrganizationalUnitDistinguishedNames' - The distinguished names of the organizational units for computer accounts.
--
-- * 'dcDirectoryName' - The fully qualified name of the directory (for example, corp.example.com).
directoryConfig
    :: Text -- ^ 'dcDirectoryName'
    -> DirectoryConfig
directoryConfig pDirectoryName_ =
  DirectoryConfig'
    { _dcCreatedTime = Nothing
    , _dcServiceAccountCredentials = Nothing
    , _dcOrganizationalUnitDistinguishedNames = Nothing
    , _dcDirectoryName = pDirectoryName_
    }


-- | The time the directory configuration was created.
dcCreatedTime :: Lens' DirectoryConfig (Maybe UTCTime)
dcCreatedTime = lens _dcCreatedTime (\ s a -> s{_dcCreatedTime = a}) . mapping _Time

-- | The credentials for the service account used by the streaming instance to connect to the directory.
dcServiceAccountCredentials :: Lens' DirectoryConfig (Maybe ServiceAccountCredentials)
dcServiceAccountCredentials = lens _dcServiceAccountCredentials (\ s a -> s{_dcServiceAccountCredentials = a})

-- | The distinguished names of the organizational units for computer accounts.
dcOrganizationalUnitDistinguishedNames :: Lens' DirectoryConfig [Text]
dcOrganizationalUnitDistinguishedNames = lens _dcOrganizationalUnitDistinguishedNames (\ s a -> s{_dcOrganizationalUnitDistinguishedNames = a}) . _Default . _Coerce

-- | The fully qualified name of the directory (for example, corp.example.com).
dcDirectoryName :: Lens' DirectoryConfig Text
dcDirectoryName = lens _dcDirectoryName (\ s a -> s{_dcDirectoryName = a})

instance FromJSON DirectoryConfig where
        parseJSON
          = withObject "DirectoryConfig"
              (\ x ->
                 DirectoryConfig' <$>
                   (x .:? "CreatedTime") <*>
                     (x .:? "ServiceAccountCredentials")
                     <*>
                     (x .:? "OrganizationalUnitDistinguishedNames" .!=
                        mempty)
                     <*> (x .: "DirectoryName"))

instance Hashable DirectoryConfig where

instance NFData DirectoryConfig where

-- | Contains the information needed to join a Microsoft Active Directory domain.
--
--
--
-- /See:/ 'domainJoinInfo' smart constructor.
data DomainJoinInfo = DomainJoinInfo'
  { _djiOrganizationalUnitDistinguishedName :: !(Maybe Text)
  , _djiDirectoryName                       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainJoinInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djiOrganizationalUnitDistinguishedName' - The distinguished name of the organizational unit for computer accounts.
--
-- * 'djiDirectoryName' - The fully qualified name of the directory (for example, corp.example.com).
domainJoinInfo
    :: DomainJoinInfo
domainJoinInfo =
  DomainJoinInfo'
    { _djiOrganizationalUnitDistinguishedName = Nothing
    , _djiDirectoryName = Nothing
    }


-- | The distinguished name of the organizational unit for computer accounts.
djiOrganizationalUnitDistinguishedName :: Lens' DomainJoinInfo (Maybe Text)
djiOrganizationalUnitDistinguishedName = lens _djiOrganizationalUnitDistinguishedName (\ s a -> s{_djiOrganizationalUnitDistinguishedName = a})

-- | The fully qualified name of the directory (for example, corp.example.com).
djiDirectoryName :: Lens' DomainJoinInfo (Maybe Text)
djiDirectoryName = lens _djiDirectoryName (\ s a -> s{_djiDirectoryName = a})

instance FromJSON DomainJoinInfo where
        parseJSON
          = withObject "DomainJoinInfo"
              (\ x ->
                 DomainJoinInfo' <$>
                   (x .:? "OrganizationalUnitDistinguishedName") <*>
                     (x .:? "DirectoryName"))

instance Hashable DomainJoinInfo where

instance NFData DomainJoinInfo where

instance ToJSON DomainJoinInfo where
        toJSON DomainJoinInfo'{..}
          = object
              (catMaybes
                 [("OrganizationalUnitDistinguishedName" .=) <$>
                    _djiOrganizationalUnitDistinguishedName,
                  ("DirectoryName" .=) <$> _djiDirectoryName])

-- | Contains the parameters for a fleet.
--
--
--
-- /See:/ 'fleet' smart constructor.
data Fleet = Fleet'
  { _fDomainJoinInfo              :: !(Maybe DomainJoinInfo)
  , _fDisconnectTimeoutInSeconds  :: !(Maybe Int)
  , _fMaxUserDurationInSeconds    :: !(Maybe Int)
  , _fCreatedTime                 :: !(Maybe POSIX)
  , _fFleetType                   :: !(Maybe FleetType)
  , _fVPCConfig                   :: !(Maybe VPCConfig)
  , _fFleetErrors                 :: !(Maybe [FleetError])
  , _fDisplayName                 :: !(Maybe Text)
  , _fEnableDefaultInternetAccess :: !(Maybe Bool)
  , _fDescription                 :: !(Maybe Text)
  , _fARN                         :: !Text
  , _fName                        :: !Text
  , _fImageName                   :: !Text
  , _fInstanceType                :: !Text
  , _fComputeCapacityStatus       :: !ComputeCapacityStatus
  , _fState                       :: !FleetState
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Fleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fDomainJoinInfo' - The information needed to join a Microsoft Active Directory domain.
--
-- * 'fDisconnectTimeoutInSeconds' - The time after disconnection when a session is considered to have ended, in seconds. If a user who was disconnected reconnects within this time interval, the user is connected to their previous session. Specify a value between 60 and 57600.
--
-- * 'fMaxUserDurationInSeconds' - The maximum time that a streaming session can run, in seconds. Specify a value between 600 and 57600.
--
-- * 'fCreatedTime' - The time the fleet was created.
--
-- * 'fFleetType' - The fleet type.     * ALWAYS_ON    * Provides users with instant-on access to their apps. You are charged for all running instances in your fleet, even if no users are streaming apps.     * ON_DEMAND    * Provide users with access to applications after they connect, which takes one to two minutes. You are charged for instance streaming when users are connected and a small hourly fee for instances that are not streaming apps.
--
-- * 'fVPCConfig' - The VPC configuration for the fleet.
--
-- * 'fFleetErrors' - The fleet errors.
--
-- * 'fDisplayName' - The fleet name for display.
--
-- * 'fEnableDefaultInternetAccess' - Indicates whether default internet access is enabled for the fleet.
--
-- * 'fDescription' - The description for display.
--
-- * 'fARN' - The ARN for the fleet.
--
-- * 'fName' - The name of the fleet.
--
-- * 'fImageName' - The name of the image used to create the fleet.
--
-- * 'fInstanceType' - The instance type to use when launching fleet instances.
--
-- * 'fComputeCapacityStatus' - The capacity status for the fleet.
--
-- * 'fState' - The current state for the fleet.
fleet
    :: Text -- ^ 'fARN'
    -> Text -- ^ 'fName'
    -> Text -- ^ 'fImageName'
    -> Text -- ^ 'fInstanceType'
    -> ComputeCapacityStatus -- ^ 'fComputeCapacityStatus'
    -> FleetState -- ^ 'fState'
    -> Fleet
fleet pARN_ pName_ pImageName_ pInstanceType_ pComputeCapacityStatus_ pState_ =
  Fleet'
    { _fDomainJoinInfo = Nothing
    , _fDisconnectTimeoutInSeconds = Nothing
    , _fMaxUserDurationInSeconds = Nothing
    , _fCreatedTime = Nothing
    , _fFleetType = Nothing
    , _fVPCConfig = Nothing
    , _fFleetErrors = Nothing
    , _fDisplayName = Nothing
    , _fEnableDefaultInternetAccess = Nothing
    , _fDescription = Nothing
    , _fARN = pARN_
    , _fName = pName_
    , _fImageName = pImageName_
    , _fInstanceType = pInstanceType_
    , _fComputeCapacityStatus = pComputeCapacityStatus_
    , _fState = pState_
    }


-- | The information needed to join a Microsoft Active Directory domain.
fDomainJoinInfo :: Lens' Fleet (Maybe DomainJoinInfo)
fDomainJoinInfo = lens _fDomainJoinInfo (\ s a -> s{_fDomainJoinInfo = a})

-- | The time after disconnection when a session is considered to have ended, in seconds. If a user who was disconnected reconnects within this time interval, the user is connected to their previous session. Specify a value between 60 and 57600.
fDisconnectTimeoutInSeconds :: Lens' Fleet (Maybe Int)
fDisconnectTimeoutInSeconds = lens _fDisconnectTimeoutInSeconds (\ s a -> s{_fDisconnectTimeoutInSeconds = a})

-- | The maximum time that a streaming session can run, in seconds. Specify a value between 600 and 57600.
fMaxUserDurationInSeconds :: Lens' Fleet (Maybe Int)
fMaxUserDurationInSeconds = lens _fMaxUserDurationInSeconds (\ s a -> s{_fMaxUserDurationInSeconds = a})

-- | The time the fleet was created.
fCreatedTime :: Lens' Fleet (Maybe UTCTime)
fCreatedTime = lens _fCreatedTime (\ s a -> s{_fCreatedTime = a}) . mapping _Time

-- | The fleet type.     * ALWAYS_ON    * Provides users with instant-on access to their apps. You are charged for all running instances in your fleet, even if no users are streaming apps.     * ON_DEMAND    * Provide users with access to applications after they connect, which takes one to two minutes. You are charged for instance streaming when users are connected and a small hourly fee for instances that are not streaming apps.
fFleetType :: Lens' Fleet (Maybe FleetType)
fFleetType = lens _fFleetType (\ s a -> s{_fFleetType = a})

-- | The VPC configuration for the fleet.
fVPCConfig :: Lens' Fleet (Maybe VPCConfig)
fVPCConfig = lens _fVPCConfig (\ s a -> s{_fVPCConfig = a})

-- | The fleet errors.
fFleetErrors :: Lens' Fleet [FleetError]
fFleetErrors = lens _fFleetErrors (\ s a -> s{_fFleetErrors = a}) . _Default . _Coerce

-- | The fleet name for display.
fDisplayName :: Lens' Fleet (Maybe Text)
fDisplayName = lens _fDisplayName (\ s a -> s{_fDisplayName = a})

-- | Indicates whether default internet access is enabled for the fleet.
fEnableDefaultInternetAccess :: Lens' Fleet (Maybe Bool)
fEnableDefaultInternetAccess = lens _fEnableDefaultInternetAccess (\ s a -> s{_fEnableDefaultInternetAccess = a})

-- | The description for display.
fDescription :: Lens' Fleet (Maybe Text)
fDescription = lens _fDescription (\ s a -> s{_fDescription = a})

-- | The ARN for the fleet.
fARN :: Lens' Fleet Text
fARN = lens _fARN (\ s a -> s{_fARN = a})

-- | The name of the fleet.
fName :: Lens' Fleet Text
fName = lens _fName (\ s a -> s{_fName = a})

-- | The name of the image used to create the fleet.
fImageName :: Lens' Fleet Text
fImageName = lens _fImageName (\ s a -> s{_fImageName = a})

-- | The instance type to use when launching fleet instances.
fInstanceType :: Lens' Fleet Text
fInstanceType = lens _fInstanceType (\ s a -> s{_fInstanceType = a})

-- | The capacity status for the fleet.
fComputeCapacityStatus :: Lens' Fleet ComputeCapacityStatus
fComputeCapacityStatus = lens _fComputeCapacityStatus (\ s a -> s{_fComputeCapacityStatus = a})

-- | The current state for the fleet.
fState :: Lens' Fleet FleetState
fState = lens _fState (\ s a -> s{_fState = a})

instance FromJSON Fleet where
        parseJSON
          = withObject "Fleet"
              (\ x ->
                 Fleet' <$>
                   (x .:? "DomainJoinInfo") <*>
                     (x .:? "DisconnectTimeoutInSeconds")
                     <*> (x .:? "MaxUserDurationInSeconds")
                     <*> (x .:? "CreatedTime")
                     <*> (x .:? "FleetType")
                     <*> (x .:? "VpcConfig")
                     <*> (x .:? "FleetErrors" .!= mempty)
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "EnableDefaultInternetAccess")
                     <*> (x .:? "Description")
                     <*> (x .: "Arn")
                     <*> (x .: "Name")
                     <*> (x .: "ImageName")
                     <*> (x .: "InstanceType")
                     <*> (x .: "ComputeCapacityStatus")
                     <*> (x .: "State"))

instance Hashable Fleet where

instance NFData Fleet where

-- | Describes a fleet error.
--
--
--
-- /See:/ 'fleetError' smart constructor.
data FleetError = FleetError'
  { _feErrorCode    :: !(Maybe FleetErrorCode)
  , _feErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FleetError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'feErrorCode' - The error code.
--
-- * 'feErrorMessage' - The error message.
fleetError
    :: FleetError
fleetError = FleetError' {_feErrorCode = Nothing, _feErrorMessage = Nothing}


-- | The error code.
feErrorCode :: Lens' FleetError (Maybe FleetErrorCode)
feErrorCode = lens _feErrorCode (\ s a -> s{_feErrorCode = a})

-- | The error message.
feErrorMessage :: Lens' FleetError (Maybe Text)
feErrorMessage = lens _feErrorMessage (\ s a -> s{_feErrorMessage = a})

instance FromJSON FleetError where
        parseJSON
          = withObject "FleetError"
              (\ x ->
                 FleetError' <$>
                   (x .:? "ErrorCode") <*> (x .:? "ErrorMessage"))

instance Hashable FleetError where

instance NFData FleetError where

-- | Describes an image.
--
--
--
-- /See:/ 'image' smart constructor.
data Image = Image'
  { _iState                       :: !(Maybe ImageState)
  , _iPlatform                    :: !(Maybe PlatformType)
  , _iPublicBaseImageReleasedDate :: !(Maybe POSIX)
  , _iStateChangeReason           :: !(Maybe ImageStateChangeReason)
  , _iARN                         :: !(Maybe Text)
  , _iCreatedTime                 :: !(Maybe POSIX)
  , _iImageBuilderSupported       :: !(Maybe Bool)
  , _iVisibility                  :: !(Maybe VisibilityType)
  , _iBaseImageARN                :: !(Maybe Text)
  , _iDisplayName                 :: !(Maybe Text)
  , _iDescription                 :: !(Maybe Text)
  , _iAppstreamAgentVersion       :: !(Maybe Text)
  , _iApplications                :: !(Maybe [Application])
  , _iName                        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iState' - The image starts in the @PENDING@ state. If image creation succeeds, the state is @AVAILABLE@ . If image creation fails, the state is @FAILED@ .
--
-- * 'iPlatform' - The operating system platform of the image.
--
-- * 'iPublicBaseImageReleasedDate' - The release date of the public base image. For private images, this date is the release date of the base image from which the image was created.
--
-- * 'iStateChangeReason' - The reason why the last state change occurred.
--
-- * 'iARN' - The ARN of the image.
--
-- * 'iCreatedTime' - The time the image was created.
--
-- * 'iImageBuilderSupported' - Indicates whether an image builder can be launched from this image.
--
-- * 'iVisibility' - Indicates whether the image is public or private.
--
-- * 'iBaseImageARN' - The ARN of the image from which this image was created.
--
-- * 'iDisplayName' - The image name for display.
--
-- * 'iDescription' - The description for display.
--
-- * 'iAppstreamAgentVersion' - The version of the AppStream 2.0 agent to use for instances that are launched from this image.
--
-- * 'iApplications' - The applications associated with the image.
--
-- * 'iName' - The name of the image.
image
    :: Text -- ^ 'iName'
    -> Image
image pName_ =
  Image'
    { _iState = Nothing
    , _iPlatform = Nothing
    , _iPublicBaseImageReleasedDate = Nothing
    , _iStateChangeReason = Nothing
    , _iARN = Nothing
    , _iCreatedTime = Nothing
    , _iImageBuilderSupported = Nothing
    , _iVisibility = Nothing
    , _iBaseImageARN = Nothing
    , _iDisplayName = Nothing
    , _iDescription = Nothing
    , _iAppstreamAgentVersion = Nothing
    , _iApplications = Nothing
    , _iName = pName_
    }


-- | The image starts in the @PENDING@ state. If image creation succeeds, the state is @AVAILABLE@ . If image creation fails, the state is @FAILED@ .
iState :: Lens' Image (Maybe ImageState)
iState = lens _iState (\ s a -> s{_iState = a})

-- | The operating system platform of the image.
iPlatform :: Lens' Image (Maybe PlatformType)
iPlatform = lens _iPlatform (\ s a -> s{_iPlatform = a})

-- | The release date of the public base image. For private images, this date is the release date of the base image from which the image was created.
iPublicBaseImageReleasedDate :: Lens' Image (Maybe UTCTime)
iPublicBaseImageReleasedDate = lens _iPublicBaseImageReleasedDate (\ s a -> s{_iPublicBaseImageReleasedDate = a}) . mapping _Time

-- | The reason why the last state change occurred.
iStateChangeReason :: Lens' Image (Maybe ImageStateChangeReason)
iStateChangeReason = lens _iStateChangeReason (\ s a -> s{_iStateChangeReason = a})

-- | The ARN of the image.
iARN :: Lens' Image (Maybe Text)
iARN = lens _iARN (\ s a -> s{_iARN = a})

-- | The time the image was created.
iCreatedTime :: Lens' Image (Maybe UTCTime)
iCreatedTime = lens _iCreatedTime (\ s a -> s{_iCreatedTime = a}) . mapping _Time

-- | Indicates whether an image builder can be launched from this image.
iImageBuilderSupported :: Lens' Image (Maybe Bool)
iImageBuilderSupported = lens _iImageBuilderSupported (\ s a -> s{_iImageBuilderSupported = a})

-- | Indicates whether the image is public or private.
iVisibility :: Lens' Image (Maybe VisibilityType)
iVisibility = lens _iVisibility (\ s a -> s{_iVisibility = a})

-- | The ARN of the image from which this image was created.
iBaseImageARN :: Lens' Image (Maybe Text)
iBaseImageARN = lens _iBaseImageARN (\ s a -> s{_iBaseImageARN = a})

-- | The image name for display.
iDisplayName :: Lens' Image (Maybe Text)
iDisplayName = lens _iDisplayName (\ s a -> s{_iDisplayName = a})

-- | The description for display.
iDescription :: Lens' Image (Maybe Text)
iDescription = lens _iDescription (\ s a -> s{_iDescription = a})

-- | The version of the AppStream 2.0 agent to use for instances that are launched from this image.
iAppstreamAgentVersion :: Lens' Image (Maybe Text)
iAppstreamAgentVersion = lens _iAppstreamAgentVersion (\ s a -> s{_iAppstreamAgentVersion = a})

-- | The applications associated with the image.
iApplications :: Lens' Image [Application]
iApplications = lens _iApplications (\ s a -> s{_iApplications = a}) . _Default . _Coerce

-- | The name of the image.
iName :: Lens' Image Text
iName = lens _iName (\ s a -> s{_iName = a})

instance FromJSON Image where
        parseJSON
          = withObject "Image"
              (\ x ->
                 Image' <$>
                   (x .:? "State") <*> (x .:? "Platform") <*>
                     (x .:? "PublicBaseImageReleasedDate")
                     <*> (x .:? "StateChangeReason")
                     <*> (x .:? "Arn")
                     <*> (x .:? "CreatedTime")
                     <*> (x .:? "ImageBuilderSupported")
                     <*> (x .:? "Visibility")
                     <*> (x .:? "BaseImageArn")
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "Description")
                     <*> (x .:? "AppstreamAgentVersion")
                     <*> (x .:? "Applications" .!= mempty)
                     <*> (x .: "Name"))

instance Hashable Image where

instance NFData Image where

-- | Describes a streaming instance used for editing an image. New images are created from a snapshot through an image builder.
--
--
--
-- /See:/ 'imageBuilder' smart constructor.
data ImageBuilder = ImageBuilder'
  { _ibDomainJoinInfo              :: !(Maybe DomainJoinInfo)
  , _ibState                       :: !(Maybe ImageBuilderState)
  , _ibPlatform                    :: !(Maybe PlatformType)
  , _ibStateChangeReason           :: !(Maybe ImageBuilderStateChangeReason)
  , _ibARN                         :: !(Maybe Text)
  , _ibCreatedTime                 :: !(Maybe POSIX)
  , _ibImageBuilderErrors          :: !(Maybe [ResourceError])
  , _ibInstanceType                :: !(Maybe Text)
  , _ibVPCConfig                   :: !(Maybe VPCConfig)
  , _ibImageARN                    :: !(Maybe Text)
  , _ibDisplayName                 :: !(Maybe Text)
  , _ibEnableDefaultInternetAccess :: !(Maybe Bool)
  , _ibDescription                 :: !(Maybe Text)
  , _ibAppstreamAgentVersion       :: !(Maybe Text)
  , _ibName                        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImageBuilder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibDomainJoinInfo' - The information needed to join a Microsoft Active Directory domain.
--
-- * 'ibState' - The state of the image builder.
--
-- * 'ibPlatform' - The operating system platform of the image builder.
--
-- * 'ibStateChangeReason' - The reason why the last state change occurred.
--
-- * 'ibARN' - The ARN for the image builder.
--
-- * 'ibCreatedTime' - The time stamp when the image builder was created.
--
-- * 'ibImageBuilderErrors' - The image builder errors.
--
-- * 'ibInstanceType' - The instance type for the image builder.
--
-- * 'ibVPCConfig' - The VPC configuration of the image builder.
--
-- * 'ibImageARN' - The ARN of the image from which this builder was created.
--
-- * 'ibDisplayName' - The image builder name for display.
--
-- * 'ibEnableDefaultInternetAccess' - Enables or disables default internet access for the image builder.
--
-- * 'ibDescription' - The description for display.
--
-- * 'ibAppstreamAgentVersion' - The version of the AppStream 2.0 agent that is currently being used by this image builder.
--
-- * 'ibName' - The name of the image builder.
imageBuilder
    :: Text -- ^ 'ibName'
    -> ImageBuilder
imageBuilder pName_ =
  ImageBuilder'
    { _ibDomainJoinInfo = Nothing
    , _ibState = Nothing
    , _ibPlatform = Nothing
    , _ibStateChangeReason = Nothing
    , _ibARN = Nothing
    , _ibCreatedTime = Nothing
    , _ibImageBuilderErrors = Nothing
    , _ibInstanceType = Nothing
    , _ibVPCConfig = Nothing
    , _ibImageARN = Nothing
    , _ibDisplayName = Nothing
    , _ibEnableDefaultInternetAccess = Nothing
    , _ibDescription = Nothing
    , _ibAppstreamAgentVersion = Nothing
    , _ibName = pName_
    }


-- | The information needed to join a Microsoft Active Directory domain.
ibDomainJoinInfo :: Lens' ImageBuilder (Maybe DomainJoinInfo)
ibDomainJoinInfo = lens _ibDomainJoinInfo (\ s a -> s{_ibDomainJoinInfo = a})

-- | The state of the image builder.
ibState :: Lens' ImageBuilder (Maybe ImageBuilderState)
ibState = lens _ibState (\ s a -> s{_ibState = a})

-- | The operating system platform of the image builder.
ibPlatform :: Lens' ImageBuilder (Maybe PlatformType)
ibPlatform = lens _ibPlatform (\ s a -> s{_ibPlatform = a})

-- | The reason why the last state change occurred.
ibStateChangeReason :: Lens' ImageBuilder (Maybe ImageBuilderStateChangeReason)
ibStateChangeReason = lens _ibStateChangeReason (\ s a -> s{_ibStateChangeReason = a})

-- | The ARN for the image builder.
ibARN :: Lens' ImageBuilder (Maybe Text)
ibARN = lens _ibARN (\ s a -> s{_ibARN = a})

-- | The time stamp when the image builder was created.
ibCreatedTime :: Lens' ImageBuilder (Maybe UTCTime)
ibCreatedTime = lens _ibCreatedTime (\ s a -> s{_ibCreatedTime = a}) . mapping _Time

-- | The image builder errors.
ibImageBuilderErrors :: Lens' ImageBuilder [ResourceError]
ibImageBuilderErrors = lens _ibImageBuilderErrors (\ s a -> s{_ibImageBuilderErrors = a}) . _Default . _Coerce

-- | The instance type for the image builder.
ibInstanceType :: Lens' ImageBuilder (Maybe Text)
ibInstanceType = lens _ibInstanceType (\ s a -> s{_ibInstanceType = a})

-- | The VPC configuration of the image builder.
ibVPCConfig :: Lens' ImageBuilder (Maybe VPCConfig)
ibVPCConfig = lens _ibVPCConfig (\ s a -> s{_ibVPCConfig = a})

-- | The ARN of the image from which this builder was created.
ibImageARN :: Lens' ImageBuilder (Maybe Text)
ibImageARN = lens _ibImageARN (\ s a -> s{_ibImageARN = a})

-- | The image builder name for display.
ibDisplayName :: Lens' ImageBuilder (Maybe Text)
ibDisplayName = lens _ibDisplayName (\ s a -> s{_ibDisplayName = a})

-- | Enables or disables default internet access for the image builder.
ibEnableDefaultInternetAccess :: Lens' ImageBuilder (Maybe Bool)
ibEnableDefaultInternetAccess = lens _ibEnableDefaultInternetAccess (\ s a -> s{_ibEnableDefaultInternetAccess = a})

-- | The description for display.
ibDescription :: Lens' ImageBuilder (Maybe Text)
ibDescription = lens _ibDescription (\ s a -> s{_ibDescription = a})

-- | The version of the AppStream 2.0 agent that is currently being used by this image builder.
ibAppstreamAgentVersion :: Lens' ImageBuilder (Maybe Text)
ibAppstreamAgentVersion = lens _ibAppstreamAgentVersion (\ s a -> s{_ibAppstreamAgentVersion = a})

-- | The name of the image builder.
ibName :: Lens' ImageBuilder Text
ibName = lens _ibName (\ s a -> s{_ibName = a})

instance FromJSON ImageBuilder where
        parseJSON
          = withObject "ImageBuilder"
              (\ x ->
                 ImageBuilder' <$>
                   (x .:? "DomainJoinInfo") <*> (x .:? "State") <*>
                     (x .:? "Platform")
                     <*> (x .:? "StateChangeReason")
                     <*> (x .:? "Arn")
                     <*> (x .:? "CreatedTime")
                     <*> (x .:? "ImageBuilderErrors" .!= mempty)
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "VpcConfig")
                     <*> (x .:? "ImageArn")
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "EnableDefaultInternetAccess")
                     <*> (x .:? "Description")
                     <*> (x .:? "AppstreamAgentVersion")
                     <*> (x .: "Name"))

instance Hashable ImageBuilder where

instance NFData ImageBuilder where

-- | Describes the reason why the last image builder state change occurred.
--
--
--
-- /See:/ 'imageBuilderStateChangeReason' smart constructor.
data ImageBuilderStateChangeReason = ImageBuilderStateChangeReason'
  { _ibscrCode    :: !(Maybe ImageBuilderStateChangeReasonCode)
  , _ibscrMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImageBuilderStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibscrCode' - The state change reason code.
--
-- * 'ibscrMessage' - The state change reason message.
imageBuilderStateChangeReason
    :: ImageBuilderStateChangeReason
imageBuilderStateChangeReason =
  ImageBuilderStateChangeReason' {_ibscrCode = Nothing, _ibscrMessage = Nothing}


-- | The state change reason code.
ibscrCode :: Lens' ImageBuilderStateChangeReason (Maybe ImageBuilderStateChangeReasonCode)
ibscrCode = lens _ibscrCode (\ s a -> s{_ibscrCode = a})

-- | The state change reason message.
ibscrMessage :: Lens' ImageBuilderStateChangeReason (Maybe Text)
ibscrMessage = lens _ibscrMessage (\ s a -> s{_ibscrMessage = a})

instance FromJSON ImageBuilderStateChangeReason where
        parseJSON
          = withObject "ImageBuilderStateChangeReason"
              (\ x ->
                 ImageBuilderStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable ImageBuilderStateChangeReason where

instance NFData ImageBuilderStateChangeReason where

-- | Describes the reason why the last image state change occurred.
--
--
--
-- /See:/ 'imageStateChangeReason' smart constructor.
data ImageStateChangeReason = ImageStateChangeReason'
  { _iscrCode    :: !(Maybe ImageStateChangeReasonCode)
  , _iscrMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImageStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscrCode' - The state change reason code.
--
-- * 'iscrMessage' - The state change reason message.
imageStateChangeReason
    :: ImageStateChangeReason
imageStateChangeReason =
  ImageStateChangeReason' {_iscrCode = Nothing, _iscrMessage = Nothing}


-- | The state change reason code.
iscrCode :: Lens' ImageStateChangeReason (Maybe ImageStateChangeReasonCode)
iscrCode = lens _iscrCode (\ s a -> s{_iscrCode = a})

-- | The state change reason message.
iscrMessage :: Lens' ImageStateChangeReason (Maybe Text)
iscrMessage = lens _iscrMessage (\ s a -> s{_iscrMessage = a})

instance FromJSON ImageStateChangeReason where
        parseJSON
          = withObject "ImageStateChangeReason"
              (\ x ->
                 ImageStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable ImageStateChangeReason where

instance NFData ImageStateChangeReason where

-- | Describes a resource error.
--
--
--
-- /See:/ 'resourceError' smart constructor.
data ResourceError = ResourceError'
  { _reErrorCode      :: !(Maybe FleetErrorCode)
  , _reErrorMessage   :: !(Maybe Text)
  , _reErrorTimestamp :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reErrorCode' - The error code.
--
-- * 'reErrorMessage' - The error message.
--
-- * 'reErrorTimestamp' - The time the error occurred.
resourceError
    :: ResourceError
resourceError =
  ResourceError'
    { _reErrorCode = Nothing
    , _reErrorMessage = Nothing
    , _reErrorTimestamp = Nothing
    }


-- | The error code.
reErrorCode :: Lens' ResourceError (Maybe FleetErrorCode)
reErrorCode = lens _reErrorCode (\ s a -> s{_reErrorCode = a})

-- | The error message.
reErrorMessage :: Lens' ResourceError (Maybe Text)
reErrorMessage = lens _reErrorMessage (\ s a -> s{_reErrorMessage = a})

-- | The time the error occurred.
reErrorTimestamp :: Lens' ResourceError (Maybe UTCTime)
reErrorTimestamp = lens _reErrorTimestamp (\ s a -> s{_reErrorTimestamp = a}) . mapping _Time

instance FromJSON ResourceError where
        parseJSON
          = withObject "ResourceError"
              (\ x ->
                 ResourceError' <$>
                   (x .:? "ErrorCode") <*> (x .:? "ErrorMessage") <*>
                     (x .:? "ErrorTimestamp"))

instance Hashable ResourceError where

instance NFData ResourceError where

-- | Describes the credentials for the service account used by the streaming instance to connect to the directory.
--
--
--
-- /See:/ 'serviceAccountCredentials' smart constructor.
data ServiceAccountCredentials = ServiceAccountCredentials'
  { _sacAccountName     :: !(Sensitive Text)
  , _sacAccountPassword :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceAccountCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sacAccountName' - The user name of the account. This account must have the following privileges: create computer objects, join computers to the domain, and change/reset the password on descendant computer objects for the organizational units specified.
--
-- * 'sacAccountPassword' - The password for the account.
serviceAccountCredentials
    :: Text -- ^ 'sacAccountName'
    -> Text -- ^ 'sacAccountPassword'
    -> ServiceAccountCredentials
serviceAccountCredentials pAccountName_ pAccountPassword_ =
  ServiceAccountCredentials'
    { _sacAccountName = _Sensitive # pAccountName_
    , _sacAccountPassword = _Sensitive # pAccountPassword_
    }


-- | The user name of the account. This account must have the following privileges: create computer objects, join computers to the domain, and change/reset the password on descendant computer objects for the organizational units specified.
sacAccountName :: Lens' ServiceAccountCredentials Text
sacAccountName = lens _sacAccountName (\ s a -> s{_sacAccountName = a}) . _Sensitive

-- | The password for the account.
sacAccountPassword :: Lens' ServiceAccountCredentials Text
sacAccountPassword = lens _sacAccountPassword (\ s a -> s{_sacAccountPassword = a}) . _Sensitive

instance FromJSON ServiceAccountCredentials where
        parseJSON
          = withObject "ServiceAccountCredentials"
              (\ x ->
                 ServiceAccountCredentials' <$>
                   (x .: "AccountName") <*> (x .: "AccountPassword"))

instance Hashable ServiceAccountCredentials where

instance NFData ServiceAccountCredentials where

instance ToJSON ServiceAccountCredentials where
        toJSON ServiceAccountCredentials'{..}
          = object
              (catMaybes
                 [Just ("AccountName" .= _sacAccountName),
                  Just ("AccountPassword" .= _sacAccountPassword)])

-- | Describes a streaming session.
--
--
--
-- /See:/ 'session' smart constructor.
data Session = Session'
  { _sAuthenticationType :: !(Maybe AuthenticationType)
  , _sId                 :: !Text
  , _sUserId             :: !Text
  , _sStackName          :: !Text
  , _sFleetName          :: !Text
  , _sState              :: !SessionState
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Session' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAuthenticationType' - The authentication method. The user is authenticated using a streaming URL (@API@ ) or SAML federation (@SAML@ ).
--
-- * 'sId' - The ID of the streaming session.
--
-- * 'sUserId' - The identifier of the user for whom the session was created.
--
-- * 'sStackName' - The name of the stack for the streaming session.
--
-- * 'sFleetName' - The name of the fleet for the streaming session.
--
-- * 'sState' - The current state of the streaming session.
session
    :: Text -- ^ 'sId'
    -> Text -- ^ 'sUserId'
    -> Text -- ^ 'sStackName'
    -> Text -- ^ 'sFleetName'
    -> SessionState -- ^ 'sState'
    -> Session
session pId_ pUserId_ pStackName_ pFleetName_ pState_ =
  Session'
    { _sAuthenticationType = Nothing
    , _sId = pId_
    , _sUserId = pUserId_
    , _sStackName = pStackName_
    , _sFleetName = pFleetName_
    , _sState = pState_
    }


-- | The authentication method. The user is authenticated using a streaming URL (@API@ ) or SAML federation (@SAML@ ).
sAuthenticationType :: Lens' Session (Maybe AuthenticationType)
sAuthenticationType = lens _sAuthenticationType (\ s a -> s{_sAuthenticationType = a})

-- | The ID of the streaming session.
sId :: Lens' Session Text
sId = lens _sId (\ s a -> s{_sId = a})

-- | The identifier of the user for whom the session was created.
sUserId :: Lens' Session Text
sUserId = lens _sUserId (\ s a -> s{_sUserId = a})

-- | The name of the stack for the streaming session.
sStackName :: Lens' Session Text
sStackName = lens _sStackName (\ s a -> s{_sStackName = a})

-- | The name of the fleet for the streaming session.
sFleetName :: Lens' Session Text
sFleetName = lens _sFleetName (\ s a -> s{_sFleetName = a})

-- | The current state of the streaming session.
sState :: Lens' Session SessionState
sState = lens _sState (\ s a -> s{_sState = a})

instance FromJSON Session where
        parseJSON
          = withObject "Session"
              (\ x ->
                 Session' <$>
                   (x .:? "AuthenticationType") <*> (x .: "Id") <*>
                     (x .: "UserId")
                     <*> (x .: "StackName")
                     <*> (x .: "FleetName")
                     <*> (x .: "State"))

instance Hashable Session where

instance NFData Session where

-- | Describes a stack.
--
--
--
-- /See:/ 'stack' smart constructor.
data Stack = Stack'
  { _sFeedbackURL       :: !(Maybe Text)
  , _sARN               :: !(Maybe Text)
  , _sCreatedTime       :: !(Maybe POSIX)
  , _sStorageConnectors :: !(Maybe [StorageConnector])
  , _sDisplayName       :: !(Maybe Text)
  , _sStackErrors       :: !(Maybe [StackError])
  , _sDescription       :: !(Maybe Text)
  , _sRedirectURL       :: !(Maybe Text)
  , _sName              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sFeedbackURL' - The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
--
-- * 'sARN' - The ARN of the stack.
--
-- * 'sCreatedTime' - The time the stack was created.
--
-- * 'sStorageConnectors' - The storage connectors to enable.
--
-- * 'sDisplayName' - The stack name for display.
--
-- * 'sStackErrors' - The errors for the stack.
--
-- * 'sDescription' - The description for display.
--
-- * 'sRedirectURL' - The URL that users are redirected to after their streaming session ends.
--
-- * 'sName' - The name of the stack.
stack
    :: Text -- ^ 'sName'
    -> Stack
stack pName_ =
  Stack'
    { _sFeedbackURL = Nothing
    , _sARN = Nothing
    , _sCreatedTime = Nothing
    , _sStorageConnectors = Nothing
    , _sDisplayName = Nothing
    , _sStackErrors = Nothing
    , _sDescription = Nothing
    , _sRedirectURL = Nothing
    , _sName = pName_
    }


-- | The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
sFeedbackURL :: Lens' Stack (Maybe Text)
sFeedbackURL = lens _sFeedbackURL (\ s a -> s{_sFeedbackURL = a})

-- | The ARN of the stack.
sARN :: Lens' Stack (Maybe Text)
sARN = lens _sARN (\ s a -> s{_sARN = a})

-- | The time the stack was created.
sCreatedTime :: Lens' Stack (Maybe UTCTime)
sCreatedTime = lens _sCreatedTime (\ s a -> s{_sCreatedTime = a}) . mapping _Time

-- | The storage connectors to enable.
sStorageConnectors :: Lens' Stack [StorageConnector]
sStorageConnectors = lens _sStorageConnectors (\ s a -> s{_sStorageConnectors = a}) . _Default . _Coerce

-- | The stack name for display.
sDisplayName :: Lens' Stack (Maybe Text)
sDisplayName = lens _sDisplayName (\ s a -> s{_sDisplayName = a})

-- | The errors for the stack.
sStackErrors :: Lens' Stack [StackError]
sStackErrors = lens _sStackErrors (\ s a -> s{_sStackErrors = a}) . _Default . _Coerce

-- | The description for display.
sDescription :: Lens' Stack (Maybe Text)
sDescription = lens _sDescription (\ s a -> s{_sDescription = a})

-- | The URL that users are redirected to after their streaming session ends.
sRedirectURL :: Lens' Stack (Maybe Text)
sRedirectURL = lens _sRedirectURL (\ s a -> s{_sRedirectURL = a})

-- | The name of the stack.
sName :: Lens' Stack Text
sName = lens _sName (\ s a -> s{_sName = a})

instance FromJSON Stack where
        parseJSON
          = withObject "Stack"
              (\ x ->
                 Stack' <$>
                   (x .:? "FeedbackURL") <*> (x .:? "Arn") <*>
                     (x .:? "CreatedTime")
                     <*> (x .:? "StorageConnectors" .!= mempty)
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "StackErrors" .!= mempty)
                     <*> (x .:? "Description")
                     <*> (x .:? "RedirectURL")
                     <*> (x .: "Name"))

instance Hashable Stack where

instance NFData Stack where

-- | Describes a stack error.
--
--
--
-- /See:/ 'stackError' smart constructor.
data StackError = StackError'
  { _seErrorCode    :: !(Maybe StackErrorCode)
  , _seErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seErrorCode' - The error code.
--
-- * 'seErrorMessage' - The error message.
stackError
    :: StackError
stackError = StackError' {_seErrorCode = Nothing, _seErrorMessage = Nothing}


-- | The error code.
seErrorCode :: Lens' StackError (Maybe StackErrorCode)
seErrorCode = lens _seErrorCode (\ s a -> s{_seErrorCode = a})

-- | The error message.
seErrorMessage :: Lens' StackError (Maybe Text)
seErrorMessage = lens _seErrorMessage (\ s a -> s{_seErrorMessage = a})

instance FromJSON StackError where
        parseJSON
          = withObject "StackError"
              (\ x ->
                 StackError' <$>
                   (x .:? "ErrorCode") <*> (x .:? "ErrorMessage"))

instance Hashable StackError where

instance NFData StackError where

-- | Describes a storage connector.
--
--
--
-- /See:/ 'storageConnector' smart constructor.
data StorageConnector = StorageConnector'
  { _scResourceIdentifier :: !(Maybe Text)
  , _scConnectorType      :: !StorageConnectorType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StorageConnector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scResourceIdentifier' - The ARN of the storage connector.
--
-- * 'scConnectorType' - The type of storage connector.
storageConnector
    :: StorageConnectorType -- ^ 'scConnectorType'
    -> StorageConnector
storageConnector pConnectorType_ =
  StorageConnector'
    {_scResourceIdentifier = Nothing, _scConnectorType = pConnectorType_}


-- | The ARN of the storage connector.
scResourceIdentifier :: Lens' StorageConnector (Maybe Text)
scResourceIdentifier = lens _scResourceIdentifier (\ s a -> s{_scResourceIdentifier = a})

-- | The type of storage connector.
scConnectorType :: Lens' StorageConnector StorageConnectorType
scConnectorType = lens _scConnectorType (\ s a -> s{_scConnectorType = a})

instance FromJSON StorageConnector where
        parseJSON
          = withObject "StorageConnector"
              (\ x ->
                 StorageConnector' <$>
                   (x .:? "ResourceIdentifier") <*>
                     (x .: "ConnectorType"))

instance Hashable StorageConnector where

instance NFData StorageConnector where

instance ToJSON StorageConnector where
        toJSON StorageConnector'{..}
          = object
              (catMaybes
                 [("ResourceIdentifier" .=) <$> _scResourceIdentifier,
                  Just ("ConnectorType" .= _scConnectorType)])

-- | Describes VPC configuration information.
--
--
--
-- /See:/ 'vpcConfig' smart constructor.
data VPCConfig = VPCConfig'
  { _vcSecurityGroupIds :: !(Maybe [Text])
  , _vcSubnetIds        :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSecurityGroupIds' - The security groups for the fleet.
--
-- * 'vcSubnetIds' - The subnets to which a network interface is established from the fleet instance.
vpcConfig
    :: VPCConfig
vpcConfig = VPCConfig' {_vcSecurityGroupIds = Nothing, _vcSubnetIds = Nothing}


-- | The security groups for the fleet.
vcSecurityGroupIds :: Lens' VPCConfig [Text]
vcSecurityGroupIds = lens _vcSecurityGroupIds (\ s a -> s{_vcSecurityGroupIds = a}) . _Default . _Coerce

-- | The subnets to which a network interface is established from the fleet instance.
vcSubnetIds :: Lens' VPCConfig [Text]
vcSubnetIds = lens _vcSubnetIds (\ s a -> s{_vcSubnetIds = a}) . _Default . _Coerce

instance FromJSON VPCConfig where
        parseJSON
          = withObject "VPCConfig"
              (\ x ->
                 VPCConfig' <$>
                   (x .:? "SecurityGroupIds" .!= mempty) <*>
                     (x .:? "SubnetIds" .!= mempty))

instance Hashable VPCConfig where

instance NFData VPCConfig where

instance ToJSON VPCConfig where
        toJSON VPCConfig'{..}
          = object
              (catMaybes
                 [("SecurityGroupIds" .=) <$> _vcSecurityGroupIds,
                  ("SubnetIds" .=) <$> _vcSubnetIds])
