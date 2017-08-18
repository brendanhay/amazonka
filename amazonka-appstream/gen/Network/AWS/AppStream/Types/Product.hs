{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.Product where

import           Network.AWS.AppStream.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | An entry for a single application in the application catalog.
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Application' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aEnabled' - If there is a problem, an application can be disabled after image creation.
--
-- * 'aLaunchPath' - The path to the application executable in the instance.
--
-- * 'aLaunchParameters' - A list of arguments that are passed to the application at launch.
--
-- * 'aName' - The unique identifier for the application.
--
-- * 'aDisplayName' - The name of the application shown to the end users.
--
-- * 'aMetadata' - Additional attributes that describe the application.
--
-- * 'aIconURL' - The URL for the application icon. This URL may be time-limited.
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

-- | If there is a problem, an application can be disabled after image creation.
aEnabled :: Lens' Application (Maybe Bool)
aEnabled = lens _aEnabled (\ s a -> s{_aEnabled = a});

-- | The path to the application executable in the instance.
aLaunchPath :: Lens' Application (Maybe Text)
aLaunchPath = lens _aLaunchPath (\ s a -> s{_aLaunchPath = a});

-- | A list of arguments that are passed to the application at launch.
aLaunchParameters :: Lens' Application (Maybe Text)
aLaunchParameters = lens _aLaunchParameters (\ s a -> s{_aLaunchParameters = a});

-- | The unique identifier for the application.
aName :: Lens' Application (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

-- | The name of the application shown to the end users.
aDisplayName :: Lens' Application (Maybe Text)
aDisplayName = lens _aDisplayName (\ s a -> s{_aDisplayName = a});

-- | Additional attributes that describe the application.
aMetadata :: Lens' Application (HashMap Text Text)
aMetadata = lens _aMetadata (\ s a -> s{_aMetadata = a}) . _Default . _Map;

-- | The URL for the application icon. This URL may be time-limited.
aIconURL :: Lens' Application (Maybe Text)
aIconURL = lens _aIconURL (\ s a -> s{_aIconURL = a});

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

instance Hashable Application

instance NFData Application

-- | The capacity configuration for the fleet.
--
--
--
-- /See:/ 'computeCapacity' smart constructor.
newtype ComputeCapacity = ComputeCapacity'
    { _ccDesiredInstances :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComputeCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccDesiredInstances' - The desired number of streaming instances.
computeCapacity
    :: Int -- ^ 'ccDesiredInstances'
    -> ComputeCapacity
computeCapacity pDesiredInstances_ =
    ComputeCapacity'
    { _ccDesiredInstances = pDesiredInstances_
    }

-- | The desired number of streaming instances.
ccDesiredInstances :: Lens' ComputeCapacity Int
ccDesiredInstances = lens _ccDesiredInstances (\ s a -> s{_ccDesiredInstances = a});

instance Hashable ComputeCapacity

instance NFData ComputeCapacity

instance ToJSON ComputeCapacity where
        toJSON ComputeCapacity'{..}
          = object
              (catMaybes
                 [Just ("DesiredInstances" .= _ccDesiredInstances)])

-- | The capacity information for the fleet.
--
--
--
-- /See:/ 'computeCapacityStatus' smart constructor.
data ComputeCapacityStatus = ComputeCapacityStatus'
    { _ccsInUse     :: !(Maybe Int)
    , _ccsRunning   :: !(Maybe Int)
    , _ccsAvailable :: !(Maybe Int)
    , _ccsDesired   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComputeCapacityStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsInUse' - The number of instances that are being used for streaming.
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

-- | The number of instances that are being used for streaming.
ccsInUse :: Lens' ComputeCapacityStatus (Maybe Int)
ccsInUse = lens _ccsInUse (\ s a -> s{_ccsInUse = a});

-- | The total number of simultaneous streaming instances that are running.
ccsRunning :: Lens' ComputeCapacityStatus (Maybe Int)
ccsRunning = lens _ccsRunning (\ s a -> s{_ccsRunning = a});

-- | The number of currently available instances that can be used to stream sessions.
ccsAvailable :: Lens' ComputeCapacityStatus (Maybe Int)
ccsAvailable = lens _ccsAvailable (\ s a -> s{_ccsAvailable = a});

-- | The desired number of streaming instances.
ccsDesired :: Lens' ComputeCapacityStatus Int
ccsDesired = lens _ccsDesired (\ s a -> s{_ccsDesired = a});

instance FromJSON ComputeCapacityStatus where
        parseJSON
          = withObject "ComputeCapacityStatus"
              (\ x ->
                 ComputeCapacityStatus' <$>
                   (x .:? "InUse") <*> (x .:? "Running") <*>
                     (x .:? "Available")
                     <*> (x .: "Desired"))

instance Hashable ComputeCapacityStatus

instance NFData ComputeCapacityStatus

-- | Full directory configuration details, which are used to join domains for the AppStream 2.0 streaming instances.
--
--
--
-- /See:/ 'directoryConfig' smart constructor.
data DirectoryConfig = DirectoryConfig'
    { _dcCreatedTime                          :: !(Maybe POSIX)
    , _dcServiceAccountCredentials            :: !(Maybe ServiceAccountCredentials)
    , _dcOrganizationalUnitDistinguishedNames :: !(Maybe [Text])
    , _dcDirectoryName                        :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DirectoryConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcCreatedTime' - The time stamp when the directory configuration was created within AppStream 2.0.
--
-- * 'dcServiceAccountCredentials' - The /AccountName/ and /AccountPassword/ of the service account, to be used by the streaming instance to connect to the directory.
--
-- * 'dcOrganizationalUnitDistinguishedNames' - The list of the distinguished names of organizational units in which to place computer accounts.
--
-- * 'dcDirectoryName' - The fully qualified name of the directory, such as corp.example.com
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

-- | The time stamp when the directory configuration was created within AppStream 2.0.
dcCreatedTime :: Lens' DirectoryConfig (Maybe UTCTime)
dcCreatedTime = lens _dcCreatedTime (\ s a -> s{_dcCreatedTime = a}) . mapping _Time;

-- | The /AccountName/ and /AccountPassword/ of the service account, to be used by the streaming instance to connect to the directory.
dcServiceAccountCredentials :: Lens' DirectoryConfig (Maybe ServiceAccountCredentials)
dcServiceAccountCredentials = lens _dcServiceAccountCredentials (\ s a -> s{_dcServiceAccountCredentials = a});

-- | The list of the distinguished names of organizational units in which to place computer accounts.
dcOrganizationalUnitDistinguishedNames :: Lens' DirectoryConfig [Text]
dcOrganizationalUnitDistinguishedNames = lens _dcOrganizationalUnitDistinguishedNames (\ s a -> s{_dcOrganizationalUnitDistinguishedNames = a}) . _Default . _Coerce;

-- | The fully qualified name of the directory, such as corp.example.com
dcDirectoryName :: Lens' DirectoryConfig Text
dcDirectoryName = lens _dcDirectoryName (\ s a -> s{_dcDirectoryName = a});

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

instance Hashable DirectoryConfig

instance NFData DirectoryConfig

-- | The /DirectoryName/ and /OrganizationalUnitDistinguishedName/ values, which are used to join domains for the AppStream 2.0 streaming instances.
--
--
--
-- /See:/ 'domainJoinInfo' smart constructor.
data DomainJoinInfo = DomainJoinInfo'
    { _djiOrganizationalUnitDistinguishedName :: !(Maybe Text)
    , _djiDirectoryName                       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DomainJoinInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djiOrganizationalUnitDistinguishedName' - The distinguished name of the organizational unit to place the computer account in.
--
-- * 'djiDirectoryName' - The fully qualified name of the directory, such as corp.example.com
domainJoinInfo
    :: DomainJoinInfo
domainJoinInfo =
    DomainJoinInfo'
    { _djiOrganizationalUnitDistinguishedName = Nothing
    , _djiDirectoryName = Nothing
    }

-- | The distinguished name of the organizational unit to place the computer account in.
djiOrganizationalUnitDistinguishedName :: Lens' DomainJoinInfo (Maybe Text)
djiOrganizationalUnitDistinguishedName = lens _djiOrganizationalUnitDistinguishedName (\ s a -> s{_djiOrganizationalUnitDistinguishedName = a});

-- | The fully qualified name of the directory, such as corp.example.com
djiDirectoryName :: Lens' DomainJoinInfo (Maybe Text)
djiDirectoryName = lens _djiDirectoryName (\ s a -> s{_djiDirectoryName = a});

instance FromJSON DomainJoinInfo where
        parseJSON
          = withObject "DomainJoinInfo"
              (\ x ->
                 DomainJoinInfo' <$>
                   (x .:? "OrganizationalUnitDistinguishedName") <*>
                     (x .:? "DirectoryName"))

instance Hashable DomainJoinInfo

instance NFData DomainJoinInfo

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Fleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fDomainJoinInfo' - The /DirectoryName/ and /OrganizationalUnitDistinguishedName/ values, which are used to join domains for the AppStream 2.0 streaming instances.
--
-- * 'fDisconnectTimeoutInSeconds' - The time after disconnection when a session is considered to have ended. If a user who got disconnected reconnects within this timeout interval, the user is connected back to their previous session. The input can be any numeric value in seconds between 60 and 57600.
--
-- * 'fMaxUserDurationInSeconds' - The maximum time for which a streaming session can run. The value can be any numeric value in seconds between 600 and 57600.
--
-- * 'fCreatedTime' - The time at which the fleet was created.
--
-- * 'fVPCConfig' - The VPC configuration for the fleet.
--
-- * 'fFleetErrors' - The list of fleet errors is appended to this list.
--
-- * 'fDisplayName' - The name displayed to end users on the AppStream 2.0 portal.
--
-- * 'fEnableDefaultInternetAccess' - Whether default internet access is enabled for the fleet.
--
-- * 'fDescription' - The description displayed to end users on the AppStream 2.0 portal.
--
-- * 'fARN' - The ARN for the fleet.
--
-- * 'fName' - The name of the fleet.
--
-- * 'fImageName' - The image used by the fleet.
--
-- * 'fInstanceType' - The instance type of compute resources for the fleet. The fleet instances are launched from this instance type.
--
-- * 'fComputeCapacityStatus' - The capacity information for the fleet.
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

-- | The /DirectoryName/ and /OrganizationalUnitDistinguishedName/ values, which are used to join domains for the AppStream 2.0 streaming instances.
fDomainJoinInfo :: Lens' Fleet (Maybe DomainJoinInfo)
fDomainJoinInfo = lens _fDomainJoinInfo (\ s a -> s{_fDomainJoinInfo = a});

-- | The time after disconnection when a session is considered to have ended. If a user who got disconnected reconnects within this timeout interval, the user is connected back to their previous session. The input can be any numeric value in seconds between 60 and 57600.
fDisconnectTimeoutInSeconds :: Lens' Fleet (Maybe Int)
fDisconnectTimeoutInSeconds = lens _fDisconnectTimeoutInSeconds (\ s a -> s{_fDisconnectTimeoutInSeconds = a});

-- | The maximum time for which a streaming session can run. The value can be any numeric value in seconds between 600 and 57600.
fMaxUserDurationInSeconds :: Lens' Fleet (Maybe Int)
fMaxUserDurationInSeconds = lens _fMaxUserDurationInSeconds (\ s a -> s{_fMaxUserDurationInSeconds = a});

-- | The time at which the fleet was created.
fCreatedTime :: Lens' Fleet (Maybe UTCTime)
fCreatedTime = lens _fCreatedTime (\ s a -> s{_fCreatedTime = a}) . mapping _Time;

-- | The VPC configuration for the fleet.
fVPCConfig :: Lens' Fleet (Maybe VPCConfig)
fVPCConfig = lens _fVPCConfig (\ s a -> s{_fVPCConfig = a});

-- | The list of fleet errors is appended to this list.
fFleetErrors :: Lens' Fleet [FleetError]
fFleetErrors = lens _fFleetErrors (\ s a -> s{_fFleetErrors = a}) . _Default . _Coerce;

-- | The name displayed to end users on the AppStream 2.0 portal.
fDisplayName :: Lens' Fleet (Maybe Text)
fDisplayName = lens _fDisplayName (\ s a -> s{_fDisplayName = a});

-- | Whether default internet access is enabled for the fleet.
fEnableDefaultInternetAccess :: Lens' Fleet (Maybe Bool)
fEnableDefaultInternetAccess = lens _fEnableDefaultInternetAccess (\ s a -> s{_fEnableDefaultInternetAccess = a});

-- | The description displayed to end users on the AppStream 2.0 portal.
fDescription :: Lens' Fleet (Maybe Text)
fDescription = lens _fDescription (\ s a -> s{_fDescription = a});

-- | The ARN for the fleet.
fARN :: Lens' Fleet Text
fARN = lens _fARN (\ s a -> s{_fARN = a});

-- | The name of the fleet.
fName :: Lens' Fleet Text
fName = lens _fName (\ s a -> s{_fName = a});

-- | The image used by the fleet.
fImageName :: Lens' Fleet Text
fImageName = lens _fImageName (\ s a -> s{_fImageName = a});

-- | The instance type of compute resources for the fleet. The fleet instances are launched from this instance type.
fInstanceType :: Lens' Fleet Text
fInstanceType = lens _fInstanceType (\ s a -> s{_fInstanceType = a});

-- | The capacity information for the fleet.
fComputeCapacityStatus :: Lens' Fleet ComputeCapacityStatus
fComputeCapacityStatus = lens _fComputeCapacityStatus (\ s a -> s{_fComputeCapacityStatus = a});

-- | The current state for the fleet.
fState :: Lens' Fleet FleetState
fState = lens _fState (\ s a -> s{_fState = a});

instance FromJSON Fleet where
        parseJSON
          = withObject "Fleet"
              (\ x ->
                 Fleet' <$>
                   (x .:? "DomainJoinInfo") <*>
                     (x .:? "DisconnectTimeoutInSeconds")
                     <*> (x .:? "MaxUserDurationInSeconds")
                     <*> (x .:? "CreatedTime")
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

instance Hashable Fleet

instance NFData Fleet

-- | The details of the fleet error.
--
--
--
-- /See:/ 'fleetError' smart constructor.
data FleetError = FleetError'
    { _feErrorCode    :: !(Maybe FleetErrorCode)
    , _feErrorMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FleetError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'feErrorCode' - The error code for the fleet error.
--
-- * 'feErrorMessage' - The error message generated when the fleet has errors.
fleetError
    :: FleetError
fleetError =
    FleetError'
    { _feErrorCode = Nothing
    , _feErrorMessage = Nothing
    }

-- | The error code for the fleet error.
feErrorCode :: Lens' FleetError (Maybe FleetErrorCode)
feErrorCode = lens _feErrorCode (\ s a -> s{_feErrorCode = a});

-- | The error message generated when the fleet has errors.
feErrorMessage :: Lens' FleetError (Maybe Text)
feErrorMessage = lens _feErrorMessage (\ s a -> s{_feErrorMessage = a});

instance FromJSON FleetError where
        parseJSON
          = withObject "FleetError"
              (\ x ->
                 FleetError' <$>
                   (x .:? "ErrorCode") <*> (x .:? "ErrorMessage"))

instance Hashable FleetError

instance NFData FleetError

-- | New streaming instances are booted from images. The image stores the application catalog and is connected to fleets.
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
    , _iApplications                :: !(Maybe [Application])
    , _iName                        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iState' - The image starts in the __PENDING__ state. If image creation succeeds, it moves to __AVAILABLE__ . If image creation fails, it moves to __FAILED__ .
--
-- * 'iPlatform' - The operating system platform of the image.
--
-- * 'iPublicBaseImageReleasedDate' - The AWS release date of the public base image. For private images, this date is the release date of the base image from which the image was created.
--
-- * 'iStateChangeReason' - The reason why the last state change occurred.
--
-- * 'iARN' - The ARN for the image.
--
-- * 'iCreatedTime' - The time stamp when the image was created.
--
-- * 'iImageBuilderSupported' - Whether an image builder can be launched from this image.
--
-- * 'iVisibility' - The visibility of an image to the user; images can be public or private.
--
-- * 'iBaseImageARN' - The source image ARN from which this image was created.
--
-- * 'iDisplayName' - The display name for the image.
--
-- * 'iDescription' - A meaningful description for the image.
--
-- * 'iApplications' - The applications associated with an image.
--
-- * 'iName' - The unique identifier for the image.
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
    , _iApplications = Nothing
    , _iName = pName_
    }

-- | The image starts in the __PENDING__ state. If image creation succeeds, it moves to __AVAILABLE__ . If image creation fails, it moves to __FAILED__ .
iState :: Lens' Image (Maybe ImageState)
iState = lens _iState (\ s a -> s{_iState = a});

-- | The operating system platform of the image.
iPlatform :: Lens' Image (Maybe PlatformType)
iPlatform = lens _iPlatform (\ s a -> s{_iPlatform = a});

-- | The AWS release date of the public base image. For private images, this date is the release date of the base image from which the image was created.
iPublicBaseImageReleasedDate :: Lens' Image (Maybe UTCTime)
iPublicBaseImageReleasedDate = lens _iPublicBaseImageReleasedDate (\ s a -> s{_iPublicBaseImageReleasedDate = a}) . mapping _Time;

-- | The reason why the last state change occurred.
iStateChangeReason :: Lens' Image (Maybe ImageStateChangeReason)
iStateChangeReason = lens _iStateChangeReason (\ s a -> s{_iStateChangeReason = a});

-- | The ARN for the image.
iARN :: Lens' Image (Maybe Text)
iARN = lens _iARN (\ s a -> s{_iARN = a});

-- | The time stamp when the image was created.
iCreatedTime :: Lens' Image (Maybe UTCTime)
iCreatedTime = lens _iCreatedTime (\ s a -> s{_iCreatedTime = a}) . mapping _Time;

-- | Whether an image builder can be launched from this image.
iImageBuilderSupported :: Lens' Image (Maybe Bool)
iImageBuilderSupported = lens _iImageBuilderSupported (\ s a -> s{_iImageBuilderSupported = a});

-- | The visibility of an image to the user; images can be public or private.
iVisibility :: Lens' Image (Maybe VisibilityType)
iVisibility = lens _iVisibility (\ s a -> s{_iVisibility = a});

-- | The source image ARN from which this image was created.
iBaseImageARN :: Lens' Image (Maybe Text)
iBaseImageARN = lens _iBaseImageARN (\ s a -> s{_iBaseImageARN = a});

-- | The display name for the image.
iDisplayName :: Lens' Image (Maybe Text)
iDisplayName = lens _iDisplayName (\ s a -> s{_iDisplayName = a});

-- | A meaningful description for the image.
iDescription :: Lens' Image (Maybe Text)
iDescription = lens _iDescription (\ s a -> s{_iDescription = a});

-- | The applications associated with an image.
iApplications :: Lens' Image [Application]
iApplications = lens _iApplications (\ s a -> s{_iApplications = a}) . _Default . _Coerce;

-- | The unique identifier for the image.
iName :: Lens' Image Text
iName = lens _iName (\ s a -> s{_iName = a});

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
                     <*> (x .:? "Applications" .!= mempty)
                     <*> (x .: "Name"))

instance Hashable Image

instance NFData Image

-- | The reason why the last state change occurred.
--
--
--
-- /See:/ 'imageStateChangeReason' smart constructor.
data ImageStateChangeReason = ImageStateChangeReason'
    { _iscrCode    :: !(Maybe ImageStateChangeReasonCode)
    , _iscrMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImageStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscrCode' - The state change reason code of the image.
--
-- * 'iscrMessage' - The state change reason message to the end user.
imageStateChangeReason
    :: ImageStateChangeReason
imageStateChangeReason =
    ImageStateChangeReason'
    { _iscrCode = Nothing
    , _iscrMessage = Nothing
    }

-- | The state change reason code of the image.
iscrCode :: Lens' ImageStateChangeReason (Maybe ImageStateChangeReasonCode)
iscrCode = lens _iscrCode (\ s a -> s{_iscrCode = a});

-- | The state change reason message to the end user.
iscrMessage :: Lens' ImageStateChangeReason (Maybe Text)
iscrMessage = lens _iscrMessage (\ s a -> s{_iscrMessage = a});

instance FromJSON ImageStateChangeReason where
        parseJSON
          = withObject "ImageStateChangeReason"
              (\ x ->
                 ImageStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable ImageStateChangeReason

instance NFData ImageStateChangeReason

-- | The /AccountName/ and /AccountPassword/ of the service account, to be used by the streaming instance to connect to the directory.
--
--
--
-- /See:/ 'serviceAccountCredentials' smart constructor.
data ServiceAccountCredentials = ServiceAccountCredentials'
    { _sacAccountName     :: !(Sensitive Text)
    , _sacAccountPassword :: !(Sensitive Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ServiceAccountCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sacAccountName' - The user name of an account in the directory that is used by AppStream 2.0 streaming instances to connect to the directory. This account must have the following privileges: create computer objects, join computers to the domain, change/reset the password on descendant computer objects for the organizational units specified.
--
-- * 'sacAccountPassword' - The password for the user account for directory actions.
serviceAccountCredentials
    :: Text -- ^ 'sacAccountName'
    -> Text -- ^ 'sacAccountPassword'
    -> ServiceAccountCredentials
serviceAccountCredentials pAccountName_ pAccountPassword_ =
    ServiceAccountCredentials'
    { _sacAccountName = _Sensitive # pAccountName_
    , _sacAccountPassword = _Sensitive # pAccountPassword_
    }

-- | The user name of an account in the directory that is used by AppStream 2.0 streaming instances to connect to the directory. This account must have the following privileges: create computer objects, join computers to the domain, change/reset the password on descendant computer objects for the organizational units specified.
sacAccountName :: Lens' ServiceAccountCredentials Text
sacAccountName = lens _sacAccountName (\ s a -> s{_sacAccountName = a}) . _Sensitive;

-- | The password for the user account for directory actions.
sacAccountPassword :: Lens' ServiceAccountCredentials Text
sacAccountPassword = lens _sacAccountPassword (\ s a -> s{_sacAccountPassword = a}) . _Sensitive;

instance FromJSON ServiceAccountCredentials where
        parseJSON
          = withObject "ServiceAccountCredentials"
              (\ x ->
                 ServiceAccountCredentials' <$>
                   (x .: "AccountName") <*> (x .: "AccountPassword"))

instance Hashable ServiceAccountCredentials

instance NFData ServiceAccountCredentials

instance ToJSON ServiceAccountCredentials where
        toJSON ServiceAccountCredentials'{..}
          = object
              (catMaybes
                 [Just ("AccountName" .= _sacAccountName),
                  Just ("AccountPassword" .= _sacAccountPassword)])

-- | Contains the parameters for a streaming session.
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Session' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAuthenticationType' - The authentication method of the user for whom the session was created. It can be @API@ for a user authenticated using a streaming URL or @SAML@ for a SAML federated user.
--
-- * 'sId' - The unique ID for a streaming session.
--
-- * 'sUserId' - The identifier of the user for whom the session was created.
--
-- * 'sStackName' - The name of the stack for which the streaming session was created.
--
-- * 'sFleetName' - The name of the fleet for which the streaming session was created.
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

-- | The authentication method of the user for whom the session was created. It can be @API@ for a user authenticated using a streaming URL or @SAML@ for a SAML federated user.
sAuthenticationType :: Lens' Session (Maybe AuthenticationType)
sAuthenticationType = lens _sAuthenticationType (\ s a -> s{_sAuthenticationType = a});

-- | The unique ID for a streaming session.
sId :: Lens' Session Text
sId = lens _sId (\ s a -> s{_sId = a});

-- | The identifier of the user for whom the session was created.
sUserId :: Lens' Session Text
sUserId = lens _sUserId (\ s a -> s{_sUserId = a});

-- | The name of the stack for which the streaming session was created.
sStackName :: Lens' Session Text
sStackName = lens _sStackName (\ s a -> s{_sStackName = a});

-- | The name of the fleet for which the streaming session was created.
sFleetName :: Lens' Session Text
sFleetName = lens _sFleetName (\ s a -> s{_sFleetName = a});

-- | The current state of the streaming session.
sState :: Lens' Session SessionState
sState = lens _sState (\ s a -> s{_sState = a});

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

instance Hashable Session

instance NFData Session

-- | Details about a stack.
--
--
--
-- /See:/ 'stack' smart constructor.
data Stack = Stack'
    { _sARN               :: !(Maybe Text)
    , _sCreatedTime       :: !(Maybe POSIX)
    , _sStorageConnectors :: !(Maybe [StorageConnector])
    , _sDisplayName       :: !(Maybe Text)
    , _sStackErrors       :: !(Maybe [StackError])
    , _sDescription       :: !(Maybe Text)
    , _sName              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sARN' - The ARN of the stack.
--
-- * 'sCreatedTime' - The time stamp when the stack was created.
--
-- * 'sStorageConnectors' - The storage connectors to be enabled for the stack.
--
-- * 'sDisplayName' - A display name for the stack.
--
-- * 'sStackErrors' - The list of errors associated with the stack.
--
-- * 'sDescription' - A meaningful description for the stack.
--
-- * 'sName' - The unique identifier of the stack.
stack
    :: Text -- ^ 'sName'
    -> Stack
stack pName_ =
    Stack'
    { _sARN = Nothing
    , _sCreatedTime = Nothing
    , _sStorageConnectors = Nothing
    , _sDisplayName = Nothing
    , _sStackErrors = Nothing
    , _sDescription = Nothing
    , _sName = pName_
    }

-- | The ARN of the stack.
sARN :: Lens' Stack (Maybe Text)
sARN = lens _sARN (\ s a -> s{_sARN = a});

-- | The time stamp when the stack was created.
sCreatedTime :: Lens' Stack (Maybe UTCTime)
sCreatedTime = lens _sCreatedTime (\ s a -> s{_sCreatedTime = a}) . mapping _Time;

-- | The storage connectors to be enabled for the stack.
sStorageConnectors :: Lens' Stack [StorageConnector]
sStorageConnectors = lens _sStorageConnectors (\ s a -> s{_sStorageConnectors = a}) . _Default . _Coerce;

-- | A display name for the stack.
sDisplayName :: Lens' Stack (Maybe Text)
sDisplayName = lens _sDisplayName (\ s a -> s{_sDisplayName = a});

-- | The list of errors associated with the stack.
sStackErrors :: Lens' Stack [StackError]
sStackErrors = lens _sStackErrors (\ s a -> s{_sStackErrors = a}) . _Default . _Coerce;

-- | A meaningful description for the stack.
sDescription :: Lens' Stack (Maybe Text)
sDescription = lens _sDescription (\ s a -> s{_sDescription = a});

-- | The unique identifier of the stack.
sName :: Lens' Stack Text
sName = lens _sName (\ s a -> s{_sName = a});

instance FromJSON Stack where
        parseJSON
          = withObject "Stack"
              (\ x ->
                 Stack' <$>
                   (x .:? "Arn") <*> (x .:? "CreatedTime") <*>
                     (x .:? "StorageConnectors" .!= mempty)
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "StackErrors" .!= mempty)
                     <*> (x .:? "Description")
                     <*> (x .: "Name"))

instance Hashable Stack

instance NFData Stack

-- | Contains the parameters for a stack error.
--
--
--
-- /See:/ 'stackError' smart constructor.
data StackError = StackError'
    { _seErrorCode    :: !(Maybe StackErrorCode)
    , _seErrorMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StackError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seErrorCode' - The error code of a stack error.
--
-- * 'seErrorMessage' - The error message of a stack error.
stackError
    :: StackError
stackError =
    StackError'
    { _seErrorCode = Nothing
    , _seErrorMessage = Nothing
    }

-- | The error code of a stack error.
seErrorCode :: Lens' StackError (Maybe StackErrorCode)
seErrorCode = lens _seErrorCode (\ s a -> s{_seErrorCode = a});

-- | The error message of a stack error.
seErrorMessage :: Lens' StackError (Maybe Text)
seErrorMessage = lens _seErrorMessage (\ s a -> s{_seErrorMessage = a});

instance FromJSON StackError where
        parseJSON
          = withObject "StackError"
              (\ x ->
                 StackError' <$>
                   (x .:? "ErrorCode") <*> (x .:? "ErrorMessage"))

instance Hashable StackError

instance NFData StackError

-- | Contains the parameters for a storage connector.
--
--
--
-- /See:/ 'storageConnector' smart constructor.
data StorageConnector = StorageConnector'
    { _scResourceIdentifier :: !(Maybe Text)
    , _scConnectorType      :: !StorageConnectorType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StorageConnector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scResourceIdentifier' - The ARN associated with the storage connector.
--
-- * 'scConnectorType' - The type of storage connector. The possible values include: HOMEFOLDERS.
storageConnector
    :: StorageConnectorType -- ^ 'scConnectorType'
    -> StorageConnector
storageConnector pConnectorType_ =
    StorageConnector'
    { _scResourceIdentifier = Nothing
    , _scConnectorType = pConnectorType_
    }

-- | The ARN associated with the storage connector.
scResourceIdentifier :: Lens' StorageConnector (Maybe Text)
scResourceIdentifier = lens _scResourceIdentifier (\ s a -> s{_scResourceIdentifier = a});

-- | The type of storage connector. The possible values include: HOMEFOLDERS.
scConnectorType :: Lens' StorageConnector StorageConnectorType
scConnectorType = lens _scConnectorType (\ s a -> s{_scConnectorType = a});

instance FromJSON StorageConnector where
        parseJSON
          = withObject "StorageConnector"
              (\ x ->
                 StorageConnector' <$>
                   (x .:? "ResourceIdentifier") <*>
                     (x .: "ConnectorType"))

instance Hashable StorageConnector

instance NFData StorageConnector

instance ToJSON StorageConnector where
        toJSON StorageConnector'{..}
          = object
              (catMaybes
                 [("ResourceIdentifier" .=) <$> _scResourceIdentifier,
                  Just ("ConnectorType" .= _scConnectorType)])

-- | VPC configuration information.
--
--
--
-- /See:/ 'vpcConfig' smart constructor.
data VPCConfig = VPCConfig'
    { _vcSecurityGroupIds :: !(Maybe [Text])
    , _vcSubnetIds        :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSecurityGroupIds' - Security groups associated with the fleet.
--
-- * 'vcSubnetIds' - The list of subnets to which a network interface is established from the fleet instance.
vpcConfig
    :: VPCConfig
vpcConfig =
    VPCConfig'
    { _vcSecurityGroupIds = Nothing
    , _vcSubnetIds = Nothing
    }

-- | Security groups associated with the fleet.
vcSecurityGroupIds :: Lens' VPCConfig [Text]
vcSecurityGroupIds = lens _vcSecurityGroupIds (\ s a -> s{_vcSecurityGroupIds = a}) . _Default . _Coerce;

-- | The list of subnets to which a network interface is established from the fleet instance.
vcSubnetIds :: Lens' VPCConfig [Text]
vcSubnetIds = lens _vcSubnetIds (\ s a -> s{_vcSubnetIds = a}) . _Default . _Coerce;

instance FromJSON VPCConfig where
        parseJSON
          = withObject "VPCConfig"
              (\ x ->
                 VPCConfig' <$>
                   (x .:? "SecurityGroupIds" .!= mempty) <*>
                     (x .:? "SubnetIds" .!= mempty))

instance Hashable VPCConfig

instance NFData VPCConfig

instance ToJSON VPCConfig where
        toJSON VPCConfig'{..}
          = object
              (catMaybes
                 [("SecurityGroupIds" .=) <$> _vcSecurityGroupIds,
                  ("SubnetIds" .=) <$> _vcSubnetIds])
