{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.Product where

import           Network.AWS.Greengrass.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Connectivity Info
--
-- /See:/ 'connectivityInfo' smart constructor.
data ConnectivityInfo = ConnectivityInfo'
    { _ciPortNumber  :: !(Maybe Int)
    , _ciId          :: !(Maybe Text)
    , _ciMetadata    :: !(Maybe Text)
    , _ciHostAddress :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConnectivityInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciPortNumber' - Port of the GGC. Usually 8883.
--
-- * 'ciId' - Element Id for this entry in the list.
--
-- * 'ciMetadata' - Metadata for this endpoint.
--
-- * 'ciHostAddress' - Endpoint for the GGC. Can be an IP address or DNS.
connectivityInfo
    :: ConnectivityInfo
connectivityInfo =
    ConnectivityInfo'
    { _ciPortNumber = Nothing
    , _ciId = Nothing
    , _ciMetadata = Nothing
    , _ciHostAddress = Nothing
    }

-- | Port of the GGC. Usually 8883.
ciPortNumber :: Lens' ConnectivityInfo (Maybe Int)
ciPortNumber = lens _ciPortNumber (\ s a -> s{_ciPortNumber = a});

-- | Element Id for this entry in the list.
ciId :: Lens' ConnectivityInfo (Maybe Text)
ciId = lens _ciId (\ s a -> s{_ciId = a});

-- | Metadata for this endpoint.
ciMetadata :: Lens' ConnectivityInfo (Maybe Text)
ciMetadata = lens _ciMetadata (\ s a -> s{_ciMetadata = a});

-- | Endpoint for the GGC. Can be an IP address or DNS.
ciHostAddress :: Lens' ConnectivityInfo (Maybe Text)
ciHostAddress = lens _ciHostAddress (\ s a -> s{_ciHostAddress = a});

instance FromJSON ConnectivityInfo where
        parseJSON
          = withObject "ConnectivityInfo"
              (\ x ->
                 ConnectivityInfo' <$>
                   (x .:? "PortNumber") <*> (x .:? "Id") <*>
                     (x .:? "Metadata")
                     <*> (x .:? "HostAddress"))

instance Hashable ConnectivityInfo

instance NFData ConnectivityInfo

instance ToJSON ConnectivityInfo where
        toJSON ConnectivityInfo'{..}
          = object
              (catMaybes
                 [("PortNumber" .=) <$> _ciPortNumber,
                  ("Id" .=) <$> _ciId, ("Metadata" .=) <$> _ciMetadata,
                  ("HostAddress" .=) <$> _ciHostAddress])

-- | Information on the core
--
-- /See:/ 'core' smart constructor.
data Core = Core'
    { _cCertificateARN :: !(Maybe Text)
    , _cThingARN       :: !(Maybe Text)
    , _cSyncShadow     :: !(Maybe Bool)
    , _cId             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Core' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCertificateARN' - Certificate arn of the core.
--
-- * 'cThingARN' - Thing arn of the core.
--
-- * 'cSyncShadow' - If true, the local shadow value automatically syncs with the cloud's shadow state.
--
-- * 'cId' - Element Id for this entry in the list.
core
    :: Core
core =
    Core'
    { _cCertificateARN = Nothing
    , _cThingARN = Nothing
    , _cSyncShadow = Nothing
    , _cId = Nothing
    }

-- | Certificate arn of the core.
cCertificateARN :: Lens' Core (Maybe Text)
cCertificateARN = lens _cCertificateARN (\ s a -> s{_cCertificateARN = a});

-- | Thing arn of the core.
cThingARN :: Lens' Core (Maybe Text)
cThingARN = lens _cThingARN (\ s a -> s{_cThingARN = a});

-- | If true, the local shadow value automatically syncs with the cloud's shadow state.
cSyncShadow :: Lens' Core (Maybe Bool)
cSyncShadow = lens _cSyncShadow (\ s a -> s{_cSyncShadow = a});

-- | Element Id for this entry in the list.
cId :: Lens' Core (Maybe Text)
cId = lens _cId (\ s a -> s{_cId = a});

instance FromJSON Core where
        parseJSON
          = withObject "Core"
              (\ x ->
                 Core' <$>
                   (x .:? "CertificateArn") <*> (x .:? "ThingArn") <*>
                     (x .:? "SyncShadow")
                     <*> (x .:? "Id"))

instance Hashable Core

instance NFData Core

instance ToJSON Core where
        toJSON Core'{..}
          = object
              (catMaybes
                 [("CertificateArn" .=) <$> _cCertificateARN,
                  ("ThingArn" .=) <$> _cThingARN,
                  ("SyncShadow" .=) <$> _cSyncShadow,
                  ("Id" .=) <$> _cId])

-- | Information on core definition version
--
-- /See:/ 'coreDefinitionVersion' smart constructor.
newtype CoreDefinitionVersion = CoreDefinitionVersion'
    { _cdvCores :: Maybe [Core]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CoreDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdvCores' - Cores in the definition version.
coreDefinitionVersion
    :: CoreDefinitionVersion
coreDefinitionVersion =
    CoreDefinitionVersion'
    { _cdvCores = Nothing
    }

-- | Cores in the definition version.
cdvCores :: Lens' CoreDefinitionVersion [Core]
cdvCores = lens _cdvCores (\ s a -> s{_cdvCores = a}) . _Default . _Coerce;

instance FromJSON CoreDefinitionVersion where
        parseJSON
          = withObject "CoreDefinitionVersion"
              (\ x ->
                 CoreDefinitionVersion' <$>
                   (x .:? "Cores" .!= mempty))

instance Hashable CoreDefinitionVersion

instance NFData CoreDefinitionVersion

instance ToJSON CoreDefinitionVersion where
        toJSON CoreDefinitionVersion'{..}
          = object (catMaybes [("Cores" .=) <$> _cdvCores])

-- | Information on the Definition
--
-- /See:/ 'definitionInformation' smart constructor.
data DefinitionInformation = DefinitionInformation'
    { _diLatestVersionARN     :: !(Maybe Text)
    , _diARN                  :: !(Maybe Text)
    , _diName                 :: !(Maybe Text)
    , _diCreationTimestamp    :: !(Maybe Text)
    , _diId                   :: !(Maybe Text)
    , _diLatestVersion        :: !(Maybe Text)
    , _diLastUpdatedTimestamp :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DefinitionInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diLatestVersionARN' - Latest version arn of the definition.
--
-- * 'diARN' - Arn of the definition.
--
-- * 'diName' - Name of the definition.
--
-- * 'diCreationTimestamp' - Timestamp of when the definition was created.
--
-- * 'diId' - Id of the definition.
--
-- * 'diLatestVersion' - Last version of the definition.
--
-- * 'diLastUpdatedTimestamp' - Last updated timestamp of the definition.
definitionInformation
    :: DefinitionInformation
definitionInformation =
    DefinitionInformation'
    { _diLatestVersionARN = Nothing
    , _diARN = Nothing
    , _diName = Nothing
    , _diCreationTimestamp = Nothing
    , _diId = Nothing
    , _diLatestVersion = Nothing
    , _diLastUpdatedTimestamp = Nothing
    }

-- | Latest version arn of the definition.
diLatestVersionARN :: Lens' DefinitionInformation (Maybe Text)
diLatestVersionARN = lens _diLatestVersionARN (\ s a -> s{_diLatestVersionARN = a});

-- | Arn of the definition.
diARN :: Lens' DefinitionInformation (Maybe Text)
diARN = lens _diARN (\ s a -> s{_diARN = a});

-- | Name of the definition.
diName :: Lens' DefinitionInformation (Maybe Text)
diName = lens _diName (\ s a -> s{_diName = a});

-- | Timestamp of when the definition was created.
diCreationTimestamp :: Lens' DefinitionInformation (Maybe Text)
diCreationTimestamp = lens _diCreationTimestamp (\ s a -> s{_diCreationTimestamp = a});

-- | Id of the definition.
diId :: Lens' DefinitionInformation (Maybe Text)
diId = lens _diId (\ s a -> s{_diId = a});

-- | Last version of the definition.
diLatestVersion :: Lens' DefinitionInformation (Maybe Text)
diLatestVersion = lens _diLatestVersion (\ s a -> s{_diLatestVersion = a});

-- | Last updated timestamp of the definition.
diLastUpdatedTimestamp :: Lens' DefinitionInformation (Maybe Text)
diLastUpdatedTimestamp = lens _diLastUpdatedTimestamp (\ s a -> s{_diLastUpdatedTimestamp = a});

instance FromJSON DefinitionInformation where
        parseJSON
          = withObject "DefinitionInformation"
              (\ x ->
                 DefinitionInformation' <$>
                   (x .:? "LatestVersionArn") <*> (x .:? "Arn") <*>
                     (x .:? "Name")
                     <*> (x .:? "CreationTimestamp")
                     <*> (x .:? "Id")
                     <*> (x .:? "LatestVersion")
                     <*> (x .:? "LastUpdatedTimestamp"))

instance Hashable DefinitionInformation

instance NFData DefinitionInformation

-- | Information on the deployment
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'
    { _dDeploymentId  :: !(Maybe Text)
    , _dDeploymentARN :: !(Maybe Text)
    , _dCreatedAt     :: !(Maybe Text)
    , _dGroupARN      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeploymentId' - Id of the deployment.
--
-- * 'dDeploymentARN' - Arn of the deployment.
--
-- * 'dCreatedAt' - Timestamp when the deployment was created.
--
-- * 'dGroupARN' - Arn of the group for this deployment.
deployment
    :: Deployment
deployment =
    Deployment'
    { _dDeploymentId = Nothing
    , _dDeploymentARN = Nothing
    , _dCreatedAt = Nothing
    , _dGroupARN = Nothing
    }

-- | Id of the deployment.
dDeploymentId :: Lens' Deployment (Maybe Text)
dDeploymentId = lens _dDeploymentId (\ s a -> s{_dDeploymentId = a});

-- | Arn of the deployment.
dDeploymentARN :: Lens' Deployment (Maybe Text)
dDeploymentARN = lens _dDeploymentARN (\ s a -> s{_dDeploymentARN = a});

-- | Timestamp when the deployment was created.
dCreatedAt :: Lens' Deployment (Maybe Text)
dCreatedAt = lens _dCreatedAt (\ s a -> s{_dCreatedAt = a});

-- | Arn of the group for this deployment.
dGroupARN :: Lens' Deployment (Maybe Text)
dGroupARN = lens _dGroupARN (\ s a -> s{_dGroupARN = a});

instance FromJSON Deployment where
        parseJSON
          = withObject "Deployment"
              (\ x ->
                 Deployment' <$>
                   (x .:? "DeploymentId") <*> (x .:? "DeploymentArn")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "GroupArn"))

instance Hashable Deployment

instance NFData Deployment

-- | Information on a Device
--
-- /See:/ 'device' smart constructor.
data Device = Device'
    { _dCertificateARN :: !(Maybe Text)
    , _dThingARN       :: !(Maybe Text)
    , _dSyncShadow     :: !(Maybe Bool)
    , _dId             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCertificateARN' - Certificate arn of the device.
--
-- * 'dThingARN' - Thing arn of the device.
--
-- * 'dSyncShadow' - If true, the local shadow value automatically syncs with the cloud's shadow state.
--
-- * 'dId' - Element Id for this entry in the list.
device
    :: Device
device =
    Device'
    { _dCertificateARN = Nothing
    , _dThingARN = Nothing
    , _dSyncShadow = Nothing
    , _dId = Nothing
    }

-- | Certificate arn of the device.
dCertificateARN :: Lens' Device (Maybe Text)
dCertificateARN = lens _dCertificateARN (\ s a -> s{_dCertificateARN = a});

-- | Thing arn of the device.
dThingARN :: Lens' Device (Maybe Text)
dThingARN = lens _dThingARN (\ s a -> s{_dThingARN = a});

-- | If true, the local shadow value automatically syncs with the cloud's shadow state.
dSyncShadow :: Lens' Device (Maybe Bool)
dSyncShadow = lens _dSyncShadow (\ s a -> s{_dSyncShadow = a});

-- | Element Id for this entry in the list.
dId :: Lens' Device (Maybe Text)
dId = lens _dId (\ s a -> s{_dId = a});

instance FromJSON Device where
        parseJSON
          = withObject "Device"
              (\ x ->
                 Device' <$>
                   (x .:? "CertificateArn") <*> (x .:? "ThingArn") <*>
                     (x .:? "SyncShadow")
                     <*> (x .:? "Id"))

instance Hashable Device

instance NFData Device

instance ToJSON Device where
        toJSON Device'{..}
          = object
              (catMaybes
                 [("CertificateArn" .=) <$> _dCertificateARN,
                  ("ThingArn" .=) <$> _dThingARN,
                  ("SyncShadow" .=) <$> _dSyncShadow,
                  ("Id" .=) <$> _dId])

-- | Information on device definition version
--
-- /See:/ 'deviceDefinitionVersion' smart constructor.
newtype DeviceDefinitionVersion = DeviceDefinitionVersion'
    { _ddvDevices :: Maybe [Device]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeviceDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddvDevices' - Devices in the definition version.
deviceDefinitionVersion
    :: DeviceDefinitionVersion
deviceDefinitionVersion =
    DeviceDefinitionVersion'
    { _ddvDevices = Nothing
    }

-- | Devices in the definition version.
ddvDevices :: Lens' DeviceDefinitionVersion [Device]
ddvDevices = lens _ddvDevices (\ s a -> s{_ddvDevices = a}) . _Default . _Coerce;

instance FromJSON DeviceDefinitionVersion where
        parseJSON
          = withObject "DeviceDefinitionVersion"
              (\ x ->
                 DeviceDefinitionVersion' <$>
                   (x .:? "Devices" .!= mempty))

instance Hashable DeviceDefinitionVersion

instance NFData DeviceDefinitionVersion

instance ToJSON DeviceDefinitionVersion where
        toJSON DeviceDefinitionVersion'{..}
          = object (catMaybes [("Devices" .=) <$> _ddvDevices])

-- | Information on function
--
-- /See:/ 'function' smart constructor.
data Function = Function'
    { _fFunctionARN           :: !(Maybe Text)
    , _fFunctionConfiguration :: !(Maybe FunctionConfiguration)
    , _fId                    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Function' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fFunctionARN' - Arn of the Lambda function.
--
-- * 'fFunctionConfiguration' - Configuration of the function
--
-- * 'fId' - Id of the function in this version.
function
    :: Function
function =
    Function'
    { _fFunctionARN = Nothing
    , _fFunctionConfiguration = Nothing
    , _fId = Nothing
    }

-- | Arn of the Lambda function.
fFunctionARN :: Lens' Function (Maybe Text)
fFunctionARN = lens _fFunctionARN (\ s a -> s{_fFunctionARN = a});

-- | Configuration of the function
fFunctionConfiguration :: Lens' Function (Maybe FunctionConfiguration)
fFunctionConfiguration = lens _fFunctionConfiguration (\ s a -> s{_fFunctionConfiguration = a});

-- | Id of the function in this version.
fId :: Lens' Function (Maybe Text)
fId = lens _fId (\ s a -> s{_fId = a});

instance FromJSON Function where
        parseJSON
          = withObject "Function"
              (\ x ->
                 Function' <$>
                   (x .:? "FunctionArn") <*>
                     (x .:? "FunctionConfiguration")
                     <*> (x .:? "Id"))

instance Hashable Function

instance NFData Function

instance ToJSON Function where
        toJSON Function'{..}
          = object
              (catMaybes
                 [("FunctionArn" .=) <$> _fFunctionARN,
                  ("FunctionConfiguration" .=) <$>
                    _fFunctionConfiguration,
                  ("Id" .=) <$> _fId])

-- | Configuration of the function
--
-- /See:/ 'functionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
    { _fcMemorySize  :: !(Maybe Int)
    , _fcExecArgs    :: !(Maybe Text)
    , _fcEnvironment :: !(Maybe FunctionConfigurationEnvironment)
    , _fcExecutable  :: !(Maybe Text)
    , _fcPinned      :: !(Maybe Bool)
    , _fcTimeout     :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcMemorySize' - The memory size, in KB, you configured for the function.
--
-- * 'fcExecArgs' - Execution Arguments
--
-- * 'fcEnvironment' - Environment of the function configuration
--
-- * 'fcExecutable' - Executable
--
-- * 'fcPinned' - Whether the function is pinned or not. Pinned means the function is long-lived and starts when the core starts.
--
-- * 'fcTimeout' - The function execution time at which Lambda should terminate the function. This timeout still applies to pinned lambdas for each request.
functionConfiguration
    :: FunctionConfiguration
functionConfiguration =
    FunctionConfiguration'
    { _fcMemorySize = Nothing
    , _fcExecArgs = Nothing
    , _fcEnvironment = Nothing
    , _fcExecutable = Nothing
    , _fcPinned = Nothing
    , _fcTimeout = Nothing
    }

-- | The memory size, in KB, you configured for the function.
fcMemorySize :: Lens' FunctionConfiguration (Maybe Int)
fcMemorySize = lens _fcMemorySize (\ s a -> s{_fcMemorySize = a});

-- | Execution Arguments
fcExecArgs :: Lens' FunctionConfiguration (Maybe Text)
fcExecArgs = lens _fcExecArgs (\ s a -> s{_fcExecArgs = a});

-- | Environment of the function configuration
fcEnvironment :: Lens' FunctionConfiguration (Maybe FunctionConfigurationEnvironment)
fcEnvironment = lens _fcEnvironment (\ s a -> s{_fcEnvironment = a});

-- | Executable
fcExecutable :: Lens' FunctionConfiguration (Maybe Text)
fcExecutable = lens _fcExecutable (\ s a -> s{_fcExecutable = a});

-- | Whether the function is pinned or not. Pinned means the function is long-lived and starts when the core starts.
fcPinned :: Lens' FunctionConfiguration (Maybe Bool)
fcPinned = lens _fcPinned (\ s a -> s{_fcPinned = a});

-- | The function execution time at which Lambda should terminate the function. This timeout still applies to pinned lambdas for each request.
fcTimeout :: Lens' FunctionConfiguration (Maybe Int)
fcTimeout = lens _fcTimeout (\ s a -> s{_fcTimeout = a});

instance FromJSON FunctionConfiguration where
        parseJSON
          = withObject "FunctionConfiguration"
              (\ x ->
                 FunctionConfiguration' <$>
                   (x .:? "MemorySize") <*> (x .:? "ExecArgs") <*>
                     (x .:? "Environment")
                     <*> (x .:? "Executable")
                     <*> (x .:? "Pinned")
                     <*> (x .:? "Timeout"))

instance Hashable FunctionConfiguration

instance NFData FunctionConfiguration

instance ToJSON FunctionConfiguration where
        toJSON FunctionConfiguration'{..}
          = object
              (catMaybes
                 [("MemorySize" .=) <$> _fcMemorySize,
                  ("ExecArgs" .=) <$> _fcExecArgs,
                  ("Environment" .=) <$> _fcEnvironment,
                  ("Executable" .=) <$> _fcExecutable,
                  ("Pinned" .=) <$> _fcPinned,
                  ("Timeout" .=) <$> _fcTimeout])

-- | Environment of the function configuration
--
-- /See:/ 'functionConfigurationEnvironment' smart constructor.
newtype FunctionConfigurationEnvironment = FunctionConfigurationEnvironment'
    { _fceVariables :: Maybe (Map Text Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FunctionConfigurationEnvironment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fceVariables' - Environment variables for the lambda function.
functionConfigurationEnvironment
    :: FunctionConfigurationEnvironment
functionConfigurationEnvironment =
    FunctionConfigurationEnvironment'
    { _fceVariables = Nothing
    }

-- | Environment variables for the lambda function.
fceVariables :: Lens' FunctionConfigurationEnvironment (HashMap Text Text)
fceVariables = lens _fceVariables (\ s a -> s{_fceVariables = a}) . _Default . _Map;

instance FromJSON FunctionConfigurationEnvironment
         where
        parseJSON
          = withObject "FunctionConfigurationEnvironment"
              (\ x ->
                 FunctionConfigurationEnvironment' <$>
                   (x .:? "Variables" .!= mempty))

instance Hashable FunctionConfigurationEnvironment

instance NFData FunctionConfigurationEnvironment

instance ToJSON FunctionConfigurationEnvironment
         where
        toJSON FunctionConfigurationEnvironment'{..}
          = object
              (catMaybes [("Variables" .=) <$> _fceVariables])

-- | Information on the function definition version
--
-- /See:/ 'functionDefinitionVersion' smart constructor.
newtype FunctionDefinitionVersion = FunctionDefinitionVersion'
    { _fdvFunctions :: Maybe [Function]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FunctionDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdvFunctions' - Lambda functions in this function definition version.
functionDefinitionVersion
    :: FunctionDefinitionVersion
functionDefinitionVersion =
    FunctionDefinitionVersion'
    { _fdvFunctions = Nothing
    }

-- | Lambda functions in this function definition version.
fdvFunctions :: Lens' FunctionDefinitionVersion [Function]
fdvFunctions = lens _fdvFunctions (\ s a -> s{_fdvFunctions = a}) . _Default . _Coerce;

instance FromJSON FunctionDefinitionVersion where
        parseJSON
          = withObject "FunctionDefinitionVersion"
              (\ x ->
                 FunctionDefinitionVersion' <$>
                   (x .:? "Functions" .!= mempty))

instance Hashable FunctionDefinitionVersion

instance NFData FunctionDefinitionVersion

instance ToJSON FunctionDefinitionVersion where
        toJSON FunctionDefinitionVersion'{..}
          = object
              (catMaybes [("Functions" .=) <$> _fdvFunctions])

-- | Information on the Logger
--
-- /See:/ 'greengrassLogger' smart constructor.
data GreengrassLogger = GreengrassLogger'
    { _glSpace     :: !(Maybe Int)
    , _glComponent :: !(Maybe LoggerComponent)
    , _glId        :: !(Maybe Text)
    , _glType      :: !(Maybe LoggerType)
    , _glLevel     :: !(Maybe LoggerLevel)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GreengrassLogger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glSpace' - Amount of hardware space, in KB, to use if file system is used for logging purposes.
--
-- * 'glComponent' - The component that will be subject to logs
--
-- * 'glId' - Element Id for this entry in the list.
--
-- * 'glType' - The type which will be use for log output
--
-- * 'glLevel' - The level of the logs
greengrassLogger
    :: GreengrassLogger
greengrassLogger =
    GreengrassLogger'
    { _glSpace = Nothing
    , _glComponent = Nothing
    , _glId = Nothing
    , _glType = Nothing
    , _glLevel = Nothing
    }

-- | Amount of hardware space, in KB, to use if file system is used for logging purposes.
glSpace :: Lens' GreengrassLogger (Maybe Int)
glSpace = lens _glSpace (\ s a -> s{_glSpace = a});

-- | The component that will be subject to logs
glComponent :: Lens' GreengrassLogger (Maybe LoggerComponent)
glComponent = lens _glComponent (\ s a -> s{_glComponent = a});

-- | Element Id for this entry in the list.
glId :: Lens' GreengrassLogger (Maybe Text)
glId = lens _glId (\ s a -> s{_glId = a});

-- | The type which will be use for log output
glType :: Lens' GreengrassLogger (Maybe LoggerType)
glType = lens _glType (\ s a -> s{_glType = a});

-- | The level of the logs
glLevel :: Lens' GreengrassLogger (Maybe LoggerLevel)
glLevel = lens _glLevel (\ s a -> s{_glLevel = a});

instance FromJSON GreengrassLogger where
        parseJSON
          = withObject "GreengrassLogger"
              (\ x ->
                 GreengrassLogger' <$>
                   (x .:? "Space") <*> (x .:? "Component") <*>
                     (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "Level"))

instance Hashable GreengrassLogger

instance NFData GreengrassLogger

instance ToJSON GreengrassLogger where
        toJSON GreengrassLogger'{..}
          = object
              (catMaybes
                 [("Space" .=) <$> _glSpace,
                  ("Component" .=) <$> _glComponent,
                  ("Id" .=) <$> _glId, ("Type" .=) <$> _glType,
                  ("Level" .=) <$> _glLevel])

-- | Information on group certificate authority properties
--
-- /See:/ 'groupCertificateAuthorityProperties' smart constructor.
data GroupCertificateAuthorityProperties = GroupCertificateAuthorityProperties'
    { _gcapGroupCertificateAuthorityARN :: !(Maybe Text)
    , _gcapGroupCertificateAuthorityId  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupCertificateAuthorityProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcapGroupCertificateAuthorityARN' - Arn of the certificate authority for the group.
--
-- * 'gcapGroupCertificateAuthorityId' - Id of the certificate authority for the group.
groupCertificateAuthorityProperties
    :: GroupCertificateAuthorityProperties
groupCertificateAuthorityProperties =
    GroupCertificateAuthorityProperties'
    { _gcapGroupCertificateAuthorityARN = Nothing
    , _gcapGroupCertificateAuthorityId = Nothing
    }

-- | Arn of the certificate authority for the group.
gcapGroupCertificateAuthorityARN :: Lens' GroupCertificateAuthorityProperties (Maybe Text)
gcapGroupCertificateAuthorityARN = lens _gcapGroupCertificateAuthorityARN (\ s a -> s{_gcapGroupCertificateAuthorityARN = a});

-- | Id of the certificate authority for the group.
gcapGroupCertificateAuthorityId :: Lens' GroupCertificateAuthorityProperties (Maybe Text)
gcapGroupCertificateAuthorityId = lens _gcapGroupCertificateAuthorityId (\ s a -> s{_gcapGroupCertificateAuthorityId = a});

instance FromJSON GroupCertificateAuthorityProperties
         where
        parseJSON
          = withObject "GroupCertificateAuthorityProperties"
              (\ x ->
                 GroupCertificateAuthorityProperties' <$>
                   (x .:? "GroupCertificateAuthorityArn") <*>
                     (x .:? "GroupCertificateAuthorityId"))

instance Hashable GroupCertificateAuthorityProperties

instance NFData GroupCertificateAuthorityProperties

-- | Information of a group
--
-- /See:/ 'groupInformation' smart constructor.
data GroupInformation = GroupInformation'
    { _giLatestVersionARN     :: !(Maybe Text)
    , _giARN                  :: !(Maybe Text)
    , _giName                 :: !(Maybe Text)
    , _giCreationTimestamp    :: !(Maybe Text)
    , _giId                   :: !(Maybe Text)
    , _giLatestVersion        :: !(Maybe Text)
    , _giLastUpdatedTimestamp :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giLatestVersionARN' - Latest version arn of the group.
--
-- * 'giARN' - Arn of a group.
--
-- * 'giName' - Name of a group.
--
-- * 'giCreationTimestamp' - Timestamp of when the group was created.
--
-- * 'giId' - Id of a group.
--
-- * 'giLatestVersion' - Last version of the group.
--
-- * 'giLastUpdatedTimestamp' - Last updated timestamp of the group.
groupInformation
    :: GroupInformation
groupInformation =
    GroupInformation'
    { _giLatestVersionARN = Nothing
    , _giARN = Nothing
    , _giName = Nothing
    , _giCreationTimestamp = Nothing
    , _giId = Nothing
    , _giLatestVersion = Nothing
    , _giLastUpdatedTimestamp = Nothing
    }

-- | Latest version arn of the group.
giLatestVersionARN :: Lens' GroupInformation (Maybe Text)
giLatestVersionARN = lens _giLatestVersionARN (\ s a -> s{_giLatestVersionARN = a});

-- | Arn of a group.
giARN :: Lens' GroupInformation (Maybe Text)
giARN = lens _giARN (\ s a -> s{_giARN = a});

-- | Name of a group.
giName :: Lens' GroupInformation (Maybe Text)
giName = lens _giName (\ s a -> s{_giName = a});

-- | Timestamp of when the group was created.
giCreationTimestamp :: Lens' GroupInformation (Maybe Text)
giCreationTimestamp = lens _giCreationTimestamp (\ s a -> s{_giCreationTimestamp = a});

-- | Id of a group.
giId :: Lens' GroupInformation (Maybe Text)
giId = lens _giId (\ s a -> s{_giId = a});

-- | Last version of the group.
giLatestVersion :: Lens' GroupInformation (Maybe Text)
giLatestVersion = lens _giLatestVersion (\ s a -> s{_giLatestVersion = a});

-- | Last updated timestamp of the group.
giLastUpdatedTimestamp :: Lens' GroupInformation (Maybe Text)
giLastUpdatedTimestamp = lens _giLastUpdatedTimestamp (\ s a -> s{_giLastUpdatedTimestamp = a});

instance FromJSON GroupInformation where
        parseJSON
          = withObject "GroupInformation"
              (\ x ->
                 GroupInformation' <$>
                   (x .:? "LatestVersionArn") <*> (x .:? "Arn") <*>
                     (x .:? "Name")
                     <*> (x .:? "CreationTimestamp")
                     <*> (x .:? "Id")
                     <*> (x .:? "LatestVersion")
                     <*> (x .:? "LastUpdatedTimestamp"))

instance Hashable GroupInformation

instance NFData GroupInformation

-- | Information on group version
--
-- /See:/ 'groupVersion' smart constructor.
data GroupVersion = GroupVersion'
    { _gvSubscriptionDefinitionVersionARN :: !(Maybe Text)
    , _gvCoreDefinitionVersionARN         :: !(Maybe Text)
    , _gvDeviceDefinitionVersionARN       :: !(Maybe Text)
    , _gvFunctionDefinitionVersionARN     :: !(Maybe Text)
    , _gvLoggerDefinitionVersionARN       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvSubscriptionDefinitionVersionARN' - Subscription definition version arn for this group.
--
-- * 'gvCoreDefinitionVersionARN' - Core definition version arn for this group.
--
-- * 'gvDeviceDefinitionVersionARN' - Device definition version arn for this group.
--
-- * 'gvFunctionDefinitionVersionARN' - Function definition version arn for this group.
--
-- * 'gvLoggerDefinitionVersionARN' - Logger definitionv ersion arn for this group.
groupVersion
    :: GroupVersion
groupVersion =
    GroupVersion'
    { _gvSubscriptionDefinitionVersionARN = Nothing
    , _gvCoreDefinitionVersionARN = Nothing
    , _gvDeviceDefinitionVersionARN = Nothing
    , _gvFunctionDefinitionVersionARN = Nothing
    , _gvLoggerDefinitionVersionARN = Nothing
    }

-- | Subscription definition version arn for this group.
gvSubscriptionDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvSubscriptionDefinitionVersionARN = lens _gvSubscriptionDefinitionVersionARN (\ s a -> s{_gvSubscriptionDefinitionVersionARN = a});

-- | Core definition version arn for this group.
gvCoreDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvCoreDefinitionVersionARN = lens _gvCoreDefinitionVersionARN (\ s a -> s{_gvCoreDefinitionVersionARN = a});

-- | Device definition version arn for this group.
gvDeviceDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvDeviceDefinitionVersionARN = lens _gvDeviceDefinitionVersionARN (\ s a -> s{_gvDeviceDefinitionVersionARN = a});

-- | Function definition version arn for this group.
gvFunctionDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvFunctionDefinitionVersionARN = lens _gvFunctionDefinitionVersionARN (\ s a -> s{_gvFunctionDefinitionVersionARN = a});

-- | Logger definitionv ersion arn for this group.
gvLoggerDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvLoggerDefinitionVersionARN = lens _gvLoggerDefinitionVersionARN (\ s a -> s{_gvLoggerDefinitionVersionARN = a});

instance FromJSON GroupVersion where
        parseJSON
          = withObject "GroupVersion"
              (\ x ->
                 GroupVersion' <$>
                   (x .:? "SubscriptionDefinitionVersionArn") <*>
                     (x .:? "CoreDefinitionVersionArn")
                     <*> (x .:? "DeviceDefinitionVersionArn")
                     <*> (x .:? "FunctionDefinitionVersionArn")
                     <*> (x .:? "LoggerDefinitionVersionArn"))

instance Hashable GroupVersion

instance NFData GroupVersion

instance ToJSON GroupVersion where
        toJSON GroupVersion'{..}
          = object
              (catMaybes
                 [("SubscriptionDefinitionVersionArn" .=) <$>
                    _gvSubscriptionDefinitionVersionARN,
                  ("CoreDefinitionVersionArn" .=) <$>
                    _gvCoreDefinitionVersionARN,
                  ("DeviceDefinitionVersionArn" .=) <$>
                    _gvDeviceDefinitionVersionARN,
                  ("FunctionDefinitionVersionArn" .=) <$>
                    _gvFunctionDefinitionVersionARN,
                  ("LoggerDefinitionVersionArn" .=) <$>
                    _gvLoggerDefinitionVersionARN])

-- | Information on logger definition version
--
-- /See:/ 'loggerDefinitionVersion' smart constructor.
newtype LoggerDefinitionVersion = LoggerDefinitionVersion'
    { _ldvLoggers :: Maybe [GreengrassLogger]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LoggerDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldvLoggers' - List of loggers.
loggerDefinitionVersion
    :: LoggerDefinitionVersion
loggerDefinitionVersion =
    LoggerDefinitionVersion'
    { _ldvLoggers = Nothing
    }

-- | List of loggers.
ldvLoggers :: Lens' LoggerDefinitionVersion [GreengrassLogger]
ldvLoggers = lens _ldvLoggers (\ s a -> s{_ldvLoggers = a}) . _Default . _Coerce;

instance FromJSON LoggerDefinitionVersion where
        parseJSON
          = withObject "LoggerDefinitionVersion"
              (\ x ->
                 LoggerDefinitionVersion' <$>
                   (x .:? "Loggers" .!= mempty))

instance Hashable LoggerDefinitionVersion

instance NFData LoggerDefinitionVersion

instance ToJSON LoggerDefinitionVersion where
        toJSON LoggerDefinitionVersion'{..}
          = object (catMaybes [("Loggers" .=) <$> _ldvLoggers])

-- | Information on subscription
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'
    { _sSubject :: !(Maybe Text)
    , _sSource  :: !(Maybe Text)
    , _sId      :: !(Maybe Text)
    , _sTarget  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubject' - Subject of the message.
--
-- * 'sSource' - Source of the subscription. Can be a thing arn, lambda arn or word 'cloud'
--
-- * 'sId' - Element Id for this entry in the list.
--
-- * 'sTarget' - Where the message is sent to. Can be a thing arn, lambda arn or word 'cloud'.
subscription
    :: Subscription
subscription =
    Subscription'
    { _sSubject = Nothing
    , _sSource = Nothing
    , _sId = Nothing
    , _sTarget = Nothing
    }

-- | Subject of the message.
sSubject :: Lens' Subscription (Maybe Text)
sSubject = lens _sSubject (\ s a -> s{_sSubject = a});

-- | Source of the subscription. Can be a thing arn, lambda arn or word 'cloud'
sSource :: Lens' Subscription (Maybe Text)
sSource = lens _sSource (\ s a -> s{_sSource = a});

-- | Element Id for this entry in the list.
sId :: Lens' Subscription (Maybe Text)
sId = lens _sId (\ s a -> s{_sId = a});

-- | Where the message is sent to. Can be a thing arn, lambda arn or word 'cloud'.
sTarget :: Lens' Subscription (Maybe Text)
sTarget = lens _sTarget (\ s a -> s{_sTarget = a});

instance FromJSON Subscription where
        parseJSON
          = withObject "Subscription"
              (\ x ->
                 Subscription' <$>
                   (x .:? "Subject") <*> (x .:? "Source") <*>
                     (x .:? "Id")
                     <*> (x .:? "Target"))

instance Hashable Subscription

instance NFData Subscription

instance ToJSON Subscription where
        toJSON Subscription'{..}
          = object
              (catMaybes
                 [("Subject" .=) <$> _sSubject,
                  ("Source" .=) <$> _sSource, ("Id" .=) <$> _sId,
                  ("Target" .=) <$> _sTarget])

-- | Information on subscription definition version
--
-- /See:/ 'subscriptionDefinitionVersion' smart constructor.
newtype SubscriptionDefinitionVersion = SubscriptionDefinitionVersion'
    { _sdvSubscriptions :: Maybe [Subscription]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SubscriptionDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdvSubscriptions' - Subscriptions in the version.
subscriptionDefinitionVersion
    :: SubscriptionDefinitionVersion
subscriptionDefinitionVersion =
    SubscriptionDefinitionVersion'
    { _sdvSubscriptions = Nothing
    }

-- | Subscriptions in the version.
sdvSubscriptions :: Lens' SubscriptionDefinitionVersion [Subscription]
sdvSubscriptions = lens _sdvSubscriptions (\ s a -> s{_sdvSubscriptions = a}) . _Default . _Coerce;

instance FromJSON SubscriptionDefinitionVersion where
        parseJSON
          = withObject "SubscriptionDefinitionVersion"
              (\ x ->
                 SubscriptionDefinitionVersion' <$>
                   (x .:? "Subscriptions" .!= mempty))

instance Hashable SubscriptionDefinitionVersion

instance NFData SubscriptionDefinitionVersion

instance ToJSON SubscriptionDefinitionVersion where
        toJSON SubscriptionDefinitionVersion'{..}
          = object
              (catMaybes
                 [("Subscriptions" .=) <$> _sdvSubscriptions])

-- | Information on the version
--
-- /See:/ 'versionInformation' smart constructor.
data VersionInformation = VersionInformation'
    { _viARN               :: !(Maybe Text)
    , _viCreationTimestamp :: !(Maybe Text)
    , _viVersion           :: !(Maybe Text)
    , _viId                :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VersionInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'viARN' - Arn of the version.
--
-- * 'viCreationTimestamp' - Timestamp of when the version was created.
--
-- * 'viVersion' - Unique Id of a version.
--
-- * 'viId' - Id of the resource container.
versionInformation
    :: VersionInformation
versionInformation =
    VersionInformation'
    { _viARN = Nothing
    , _viCreationTimestamp = Nothing
    , _viVersion = Nothing
    , _viId = Nothing
    }

-- | Arn of the version.
viARN :: Lens' VersionInformation (Maybe Text)
viARN = lens _viARN (\ s a -> s{_viARN = a});

-- | Timestamp of when the version was created.
viCreationTimestamp :: Lens' VersionInformation (Maybe Text)
viCreationTimestamp = lens _viCreationTimestamp (\ s a -> s{_viCreationTimestamp = a});

-- | Unique Id of a version.
viVersion :: Lens' VersionInformation (Maybe Text)
viVersion = lens _viVersion (\ s a -> s{_viVersion = a});

-- | Id of the resource container.
viId :: Lens' VersionInformation (Maybe Text)
viId = lens _viId (\ s a -> s{_viId = a});

instance FromJSON VersionInformation where
        parseJSON
          = withObject "VersionInformation"
              (\ x ->
                 VersionInformation' <$>
                   (x .:? "Arn") <*> (x .:? "CreationTimestamp") <*>
                     (x .:? "Version")
                     <*> (x .:? "Id"))

instance Hashable VersionInformation

instance NFData VersionInformation
