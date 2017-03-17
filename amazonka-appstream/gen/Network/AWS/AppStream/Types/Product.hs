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
-- * 'aEnabled' - An application can be disabled after image creation if there is a problem.
--
-- * 'aLaunchPath' - The path to the application executable in the instance.
--
-- * 'aLaunchParameters' - A list of arguments that are passed to the application at launch.
--
-- * 'aName' - The unique identifier for the application.
--
-- * 'aDisplayName' - The name of the application shown to the end users.
--
-- * 'aMetadata' - Additional attributes that describes the application.
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

-- | An application can be disabled after image creation if there is a problem.
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

-- | Additional attributes that describes the application.
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

-- | Contains the parameters for a fleet.
--
--
--
-- /See:/ 'fleet' smart constructor.
data Fleet = Fleet'
    { _fDisconnectTimeoutInSeconds :: !(Maybe Int)
    , _fMaxUserDurationInSeconds   :: !(Maybe Int)
    , _fCreatedTime                :: !(Maybe POSIX)
    , _fVPCConfig                  :: !(Maybe VPCConfig)
    , _fFleetErrors                :: !(Maybe [FleetError])
    , _fDisplayName                :: !(Maybe Text)
    , _fDescription                :: !(Maybe Text)
    , _fARN                        :: !Text
    , _fName                       :: !Text
    , _fImageName                  :: !Text
    , _fInstanceType               :: !Text
    , _fComputeCapacityStatus      :: !ComputeCapacityStatus
    , _fState                      :: !FleetState
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Fleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fDisconnectTimeoutInSeconds' - The time after disconnection when a session is considered to have ended. When a user reconnects after a disconnection, the user is connected to the same session and instance within this time interval.
--
-- * 'fMaxUserDurationInSeconds' - The maximum time during which a streaming session can run.
--
-- * 'fCreatedTime' - The time at which the fleet was created.
--
-- * 'fVPCConfig' - The VPC configuration for the fleet.
--
-- * 'fFleetErrors' - The list of fleet errors is appended to this list.
--
-- * 'fDisplayName' - The name displayed to end users on the AppStream 2.0 portal.
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
    { _fDisconnectTimeoutInSeconds = Nothing
    , _fMaxUserDurationInSeconds = Nothing
    , _fCreatedTime = Nothing
    , _fVPCConfig = Nothing
    , _fFleetErrors = Nothing
    , _fDisplayName = Nothing
    , _fDescription = Nothing
    , _fARN = pARN_
    , _fName = pName_
    , _fImageName = pImageName_
    , _fInstanceType = pInstanceType_
    , _fComputeCapacityStatus = pComputeCapacityStatus_
    , _fState = pState_
    }

-- | The time after disconnection when a session is considered to have ended. When a user reconnects after a disconnection, the user is connected to the same session and instance within this time interval.
fDisconnectTimeoutInSeconds :: Lens' Fleet (Maybe Int)
fDisconnectTimeoutInSeconds = lens _fDisconnectTimeoutInSeconds (\ s a -> s{_fDisconnectTimeoutInSeconds = a});

-- | The maximum time during which a streaming session can run.
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
                   (x .:? "DisconnectTimeoutInSeconds") <*>
                     (x .:? "MaxUserDurationInSeconds")
                     <*> (x .:? "CreatedTime")
                     <*> (x .:? "VpcConfig")
                     <*> (x .:? "FleetErrors" .!= mempty)
                     <*> (x .:? "DisplayName")
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
    { _iState             :: !(Maybe ImageState)
    , _iPlatform          :: !(Maybe PlatformType)
    , _iStateChangeReason :: !(Maybe ImageStateChangeReason)
    , _iARN               :: !(Maybe Text)
    , _iCreatedTime       :: !(Maybe POSIX)
    , _iVisibility        :: !(Maybe VisibilityType)
    , _iBaseImageARN      :: !(Maybe Text)
    , _iDisplayName       :: !(Maybe Text)
    , _iDescription       :: !(Maybe Text)
    , _iApplications      :: !(Maybe [Application])
    , _iName              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iState' - The image starts in the __PENDING__ state, and then moves to __AVAILABLE__ if image creation succeeds and __FAILED__ if image creation has failed.
--
-- * 'iPlatform' - The operating system platform of the image.
--
-- * 'iStateChangeReason' - The reason why the last state change occurred.
--
-- * 'iARN' - The ARN for the image.
--
-- * 'iCreatedTime' - The timestamp when the image was created.
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
    , _iStateChangeReason = Nothing
    , _iARN = Nothing
    , _iCreatedTime = Nothing
    , _iVisibility = Nothing
    , _iBaseImageARN = Nothing
    , _iDisplayName = Nothing
    , _iDescription = Nothing
    , _iApplications = Nothing
    , _iName = pName_
    }

-- | The image starts in the __PENDING__ state, and then moves to __AVAILABLE__ if image creation succeeds and __FAILED__ if image creation has failed.
iState :: Lens' Image (Maybe ImageState)
iState = lens _iState (\ s a -> s{_iState = a});

-- | The operating system platform of the image.
iPlatform :: Lens' Image (Maybe PlatformType)
iPlatform = lens _iPlatform (\ s a -> s{_iPlatform = a});

-- | The reason why the last state change occurred.
iStateChangeReason :: Lens' Image (Maybe ImageStateChangeReason)
iStateChangeReason = lens _iStateChangeReason (\ s a -> s{_iStateChangeReason = a});

-- | The ARN for the image.
iARN :: Lens' Image (Maybe Text)
iARN = lens _iARN (\ s a -> s{_iARN = a});

-- | The timestamp when the image was created.
iCreatedTime :: Lens' Image (Maybe UTCTime)
iCreatedTime = lens _iCreatedTime (\ s a -> s{_iCreatedTime = a}) . mapping _Time;

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
                     (x .:? "StateChangeReason")
                     <*> (x .:? "Arn")
                     <*> (x .:? "CreatedTime")
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

-- | Contains the parameters for a streaming session.
--
--
--
-- /See:/ 'session' smart constructor.
data Session = Session'
    { _sId        :: !Text
    , _sUserId    :: !Text
    , _sStackName :: !Text
    , _sFleetName :: !Text
    , _sState     :: !SessionState
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Session' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
    { _sId = pId_
    , _sUserId = pUserId_
    , _sStackName = pStackName_
    , _sFleetName = pFleetName_
    , _sState = pState_
    }

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
                   (x .: "Id") <*> (x .: "UserId") <*>
                     (x .: "StackName")
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
    { _sARN         :: !(Maybe Text)
    , _sCreatedTime :: !(Maybe POSIX)
    , _sDisplayName :: !(Maybe Text)
    , _sDescription :: !(Maybe Text)
    , _sName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sARN' - The ARN of the stack.
--
-- * 'sCreatedTime' - The timestamp when the stack was created.
--
-- * 'sDisplayName' - A display name for the stack.
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
    , _sDisplayName = Nothing
    , _sDescription = Nothing
    , _sName = pName_
    }

-- | The ARN of the stack.
sARN :: Lens' Stack (Maybe Text)
sARN = lens _sARN (\ s a -> s{_sARN = a});

-- | The timestamp when the stack was created.
sCreatedTime :: Lens' Stack (Maybe UTCTime)
sCreatedTime = lens _sCreatedTime (\ s a -> s{_sCreatedTime = a}) . mapping _Time;

-- | A display name for the stack.
sDisplayName :: Lens' Stack (Maybe Text)
sDisplayName = lens _sDisplayName (\ s a -> s{_sDisplayName = a});

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
                     (x .:? "DisplayName")
                     <*> (x .:? "Description")
                     <*> (x .: "Name"))

instance Hashable Stack

instance NFData Stack

-- | The VPC in which the fleet is launched.
--
--
--
-- /See:/ 'vpcConfig' smart constructor.
newtype VPCConfig = VPCConfig'
    { _vcSubnetIds :: List1 Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSubnetIds' - The list of subnets to which a network interface is established from the fleet instance.
vpcConfig
    :: NonEmpty Text -- ^ 'vcSubnetIds'
    -> VPCConfig
vpcConfig pSubnetIds_ =
    VPCConfig'
    { _vcSubnetIds = _List1 # pSubnetIds_
    }

-- | The list of subnets to which a network interface is established from the fleet instance.
vcSubnetIds :: Lens' VPCConfig (NonEmpty Text)
vcSubnetIds = lens _vcSubnetIds (\ s a -> s{_vcSubnetIds = a}) . _List1;

instance FromJSON VPCConfig where
        parseJSON
          = withObject "VPCConfig"
              (\ x -> VPCConfig' <$> (x .: "SubnetIds"))

instance Hashable VPCConfig

instance NFData VPCConfig

instance ToJSON VPCConfig where
        toJSON VPCConfig'{..}
          = object
              (catMaybes [Just ("SubnetIds" .= _vcSubnetIds)])
