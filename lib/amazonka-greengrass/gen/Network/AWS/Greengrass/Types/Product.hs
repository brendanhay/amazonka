{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.Product where

import Network.AWS.Greengrass.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a Greengrass core's connectivity.
--
-- /See:/ 'connectivityInfo' smart constructor.
data ConnectivityInfo = ConnectivityInfo'
  { _ciPortNumber  :: !(Maybe Int)
  , _ciId          :: !(Maybe Text)
  , _ciMetadata    :: !(Maybe Text)
  , _ciHostAddress :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConnectivityInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciPortNumber' - The port of the Greengrass core. Usually 8883.
--
-- * 'ciId' - The ID of the connectivity information.
--
-- * 'ciMetadata' - Metadata for this endpoint.
--
-- * 'ciHostAddress' - The endpoint for the Greengrass core. Can be an IP address or DNS.
connectivityInfo
    :: ConnectivityInfo
connectivityInfo =
  ConnectivityInfo'
    { _ciPortNumber = Nothing
    , _ciId = Nothing
    , _ciMetadata = Nothing
    , _ciHostAddress = Nothing
    }


-- | The port of the Greengrass core. Usually 8883.
ciPortNumber :: Lens' ConnectivityInfo (Maybe Int)
ciPortNumber = lens _ciPortNumber (\ s a -> s{_ciPortNumber = a})

-- | The ID of the connectivity information.
ciId :: Lens' ConnectivityInfo (Maybe Text)
ciId = lens _ciId (\ s a -> s{_ciId = a})

-- | Metadata for this endpoint.
ciMetadata :: Lens' ConnectivityInfo (Maybe Text)
ciMetadata = lens _ciMetadata (\ s a -> s{_ciMetadata = a})

-- | The endpoint for the Greengrass core. Can be an IP address or DNS.
ciHostAddress :: Lens' ConnectivityInfo (Maybe Text)
ciHostAddress = lens _ciHostAddress (\ s a -> s{_ciHostAddress = a})

instance FromJSON ConnectivityInfo where
        parseJSON
          = withObject "ConnectivityInfo"
              (\ x ->
                 ConnectivityInfo' <$>
                   (x .:? "PortNumber") <*> (x .:? "Id") <*>
                     (x .:? "Metadata")
                     <*> (x .:? "HostAddress"))

instance Hashable ConnectivityInfo where

instance NFData ConnectivityInfo where

instance ToJSON ConnectivityInfo where
        toJSON ConnectivityInfo'{..}
          = object
              (catMaybes
                 [("PortNumber" .=) <$> _ciPortNumber,
                  ("Id" .=) <$> _ciId, ("Metadata" .=) <$> _ciMetadata,
                  ("HostAddress" .=) <$> _ciHostAddress])

-- | Information about a core.
--
-- /See:/ 'core' smart constructor.
data Core = Core'
  { _cCertificateARN :: !(Maybe Text)
  , _cThingARN       :: !(Maybe Text)
  , _cSyncShadow     :: !(Maybe Bool)
  , _cId             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Core' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCertificateARN' - The ARN of the certificate associated with the core.
--
-- * 'cThingARN' - The ARN of the thing which is the core.
--
-- * 'cSyncShadow' - If true, the core's local shadow is automatically synced with the cloud.
--
-- * 'cId' - The ID of the core.
core
    :: Core
core =
  Core'
    { _cCertificateARN = Nothing
    , _cThingARN = Nothing
    , _cSyncShadow = Nothing
    , _cId = Nothing
    }


-- | The ARN of the certificate associated with the core.
cCertificateARN :: Lens' Core (Maybe Text)
cCertificateARN = lens _cCertificateARN (\ s a -> s{_cCertificateARN = a})

-- | The ARN of the thing which is the core.
cThingARN :: Lens' Core (Maybe Text)
cThingARN = lens _cThingARN (\ s a -> s{_cThingARN = a})

-- | If true, the core's local shadow is automatically synced with the cloud.
cSyncShadow :: Lens' Core (Maybe Bool)
cSyncShadow = lens _cSyncShadow (\ s a -> s{_cSyncShadow = a})

-- | The ID of the core.
cId :: Lens' Core (Maybe Text)
cId = lens _cId (\ s a -> s{_cId = a})

instance FromJSON Core where
        parseJSON
          = withObject "Core"
              (\ x ->
                 Core' <$>
                   (x .:? "CertificateArn") <*> (x .:? "ThingArn") <*>
                     (x .:? "SyncShadow")
                     <*> (x .:? "Id"))

instance Hashable Core where

instance NFData Core where

instance ToJSON Core where
        toJSON Core'{..}
          = object
              (catMaybes
                 [("CertificateArn" .=) <$> _cCertificateARN,
                  ("ThingArn" .=) <$> _cThingARN,
                  ("SyncShadow" .=) <$> _cSyncShadow,
                  ("Id" .=) <$> _cId])

-- | Information about a core definition version.
--
-- /See:/ 'coreDefinitionVersion' smart constructor.
newtype CoreDefinitionVersion = CoreDefinitionVersion'
  { _cdvCores :: Maybe [Core]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CoreDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdvCores' - A list of cores in the core definition version.
coreDefinitionVersion
    :: CoreDefinitionVersion
coreDefinitionVersion = CoreDefinitionVersion' {_cdvCores = Nothing}


-- | A list of cores in the core definition version.
cdvCores :: Lens' CoreDefinitionVersion [Core]
cdvCores = lens _cdvCores (\ s a -> s{_cdvCores = a}) . _Default . _Coerce

instance FromJSON CoreDefinitionVersion where
        parseJSON
          = withObject "CoreDefinitionVersion"
              (\ x ->
                 CoreDefinitionVersion' <$>
                   (x .:? "Cores" .!= mempty))

instance Hashable CoreDefinitionVersion where

instance NFData CoreDefinitionVersion where

instance ToJSON CoreDefinitionVersion where
        toJSON CoreDefinitionVersion'{..}
          = object (catMaybes [("Cores" .=) <$> _cdvCores])

-- | Information about a definition.
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DefinitionInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diLatestVersionARN' - The ARN of the latest version of the definition.
--
-- * 'diARN' - The ARN of the definition.
--
-- * 'diName' - The name of the definition.
--
-- * 'diCreationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
--
-- * 'diId' - The ID of the definition.
--
-- * 'diLatestVersion' - The latest version of the definition.
--
-- * 'diLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
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


-- | The ARN of the latest version of the definition.
diLatestVersionARN :: Lens' DefinitionInformation (Maybe Text)
diLatestVersionARN = lens _diLatestVersionARN (\ s a -> s{_diLatestVersionARN = a})

-- | The ARN of the definition.
diARN :: Lens' DefinitionInformation (Maybe Text)
diARN = lens _diARN (\ s a -> s{_diARN = a})

-- | The name of the definition.
diName :: Lens' DefinitionInformation (Maybe Text)
diName = lens _diName (\ s a -> s{_diName = a})

-- | The time, in milliseconds since the epoch, when the definition was created.
diCreationTimestamp :: Lens' DefinitionInformation (Maybe Text)
diCreationTimestamp = lens _diCreationTimestamp (\ s a -> s{_diCreationTimestamp = a})

-- | The ID of the definition.
diId :: Lens' DefinitionInformation (Maybe Text)
diId = lens _diId (\ s a -> s{_diId = a})

-- | The latest version of the definition.
diLatestVersion :: Lens' DefinitionInformation (Maybe Text)
diLatestVersion = lens _diLatestVersion (\ s a -> s{_diLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the definition was last updated.
diLastUpdatedTimestamp :: Lens' DefinitionInformation (Maybe Text)
diLastUpdatedTimestamp = lens _diLastUpdatedTimestamp (\ s a -> s{_diLastUpdatedTimestamp = a})

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

instance Hashable DefinitionInformation where

instance NFData DefinitionInformation where

-- | Information about a deployment.
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'
  { _dDeploymentId   :: !(Maybe Text)
  , _dDeploymentARN  :: !(Maybe Text)
  , _dCreatedAt      :: !(Maybe Text)
  , _dDeploymentType :: !(Maybe DeploymentType)
  , _dGroupARN       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeploymentId' - The ID of the deployment.
--
-- * 'dDeploymentARN' - The ARN of the deployment.
--
-- * 'dCreatedAt' - The time, in milliseconds since the epoch, when the deployment was created.
--
-- * 'dDeploymentType' - The type of the deployment.
--
-- * 'dGroupARN' - The ARN of the group for this deployment.
deployment
    :: Deployment
deployment =
  Deployment'
    { _dDeploymentId = Nothing
    , _dDeploymentARN = Nothing
    , _dCreatedAt = Nothing
    , _dDeploymentType = Nothing
    , _dGroupARN = Nothing
    }


-- | The ID of the deployment.
dDeploymentId :: Lens' Deployment (Maybe Text)
dDeploymentId = lens _dDeploymentId (\ s a -> s{_dDeploymentId = a})

-- | The ARN of the deployment.
dDeploymentARN :: Lens' Deployment (Maybe Text)
dDeploymentARN = lens _dDeploymentARN (\ s a -> s{_dDeploymentARN = a})

-- | The time, in milliseconds since the epoch, when the deployment was created.
dCreatedAt :: Lens' Deployment (Maybe Text)
dCreatedAt = lens _dCreatedAt (\ s a -> s{_dCreatedAt = a})

-- | The type of the deployment.
dDeploymentType :: Lens' Deployment (Maybe DeploymentType)
dDeploymentType = lens _dDeploymentType (\ s a -> s{_dDeploymentType = a})

-- | The ARN of the group for this deployment.
dGroupARN :: Lens' Deployment (Maybe Text)
dGroupARN = lens _dGroupARN (\ s a -> s{_dGroupARN = a})

instance FromJSON Deployment where
        parseJSON
          = withObject "Deployment"
              (\ x ->
                 Deployment' <$>
                   (x .:? "DeploymentId") <*> (x .:? "DeploymentArn")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "DeploymentType")
                     <*> (x .:? "GroupArn"))

instance Hashable Deployment where

instance NFData Deployment where

-- | Information about a device.
--
-- /See:/ 'device' smart constructor.
data Device = Device'
  { _dCertificateARN :: !(Maybe Text)
  , _dThingARN       :: !(Maybe Text)
  , _dSyncShadow     :: !(Maybe Bool)
  , _dId             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCertificateARN' - The ARN of the certificate associated with the device.
--
-- * 'dThingARN' - The thing ARN of the device.
--
-- * 'dSyncShadow' - If true, the device's local shadow will be automatically synced with the cloud.
--
-- * 'dId' - The ID of the device.
device
    :: Device
device =
  Device'
    { _dCertificateARN = Nothing
    , _dThingARN = Nothing
    , _dSyncShadow = Nothing
    , _dId = Nothing
    }


-- | The ARN of the certificate associated with the device.
dCertificateARN :: Lens' Device (Maybe Text)
dCertificateARN = lens _dCertificateARN (\ s a -> s{_dCertificateARN = a})

-- | The thing ARN of the device.
dThingARN :: Lens' Device (Maybe Text)
dThingARN = lens _dThingARN (\ s a -> s{_dThingARN = a})

-- | If true, the device's local shadow will be automatically synced with the cloud.
dSyncShadow :: Lens' Device (Maybe Bool)
dSyncShadow = lens _dSyncShadow (\ s a -> s{_dSyncShadow = a})

-- | The ID of the device.
dId :: Lens' Device (Maybe Text)
dId = lens _dId (\ s a -> s{_dId = a})

instance FromJSON Device where
        parseJSON
          = withObject "Device"
              (\ x ->
                 Device' <$>
                   (x .:? "CertificateArn") <*> (x .:? "ThingArn") <*>
                     (x .:? "SyncShadow")
                     <*> (x .:? "Id"))

instance Hashable Device where

instance NFData Device where

instance ToJSON Device where
        toJSON Device'{..}
          = object
              (catMaybes
                 [("CertificateArn" .=) <$> _dCertificateARN,
                  ("ThingArn" .=) <$> _dThingARN,
                  ("SyncShadow" .=) <$> _dSyncShadow,
                  ("Id" .=) <$> _dId])

-- | Information about a device definition version.
--
-- /See:/ 'deviceDefinitionVersion' smart constructor.
newtype DeviceDefinitionVersion = DeviceDefinitionVersion'
  { _ddvDevices :: Maybe [Device]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddvDevices' - A list of devices in the definition version.
deviceDefinitionVersion
    :: DeviceDefinitionVersion
deviceDefinitionVersion = DeviceDefinitionVersion' {_ddvDevices = Nothing}


-- | A list of devices in the definition version.
ddvDevices :: Lens' DeviceDefinitionVersion [Device]
ddvDevices = lens _ddvDevices (\ s a -> s{_ddvDevices = a}) . _Default . _Coerce

instance FromJSON DeviceDefinitionVersion where
        parseJSON
          = withObject "DeviceDefinitionVersion"
              (\ x ->
                 DeviceDefinitionVersion' <$>
                   (x .:? "Devices" .!= mempty))

instance Hashable DeviceDefinitionVersion where

instance NFData DeviceDefinitionVersion where

instance ToJSON DeviceDefinitionVersion where
        toJSON DeviceDefinitionVersion'{..}
          = object (catMaybes [("Devices" .=) <$> _ddvDevices])

-- | Details about the error.
--
-- /See:/ 'errorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { _edDetailedErrorCode    :: !(Maybe Text)
  , _edDetailedErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ErrorDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edDetailedErrorCode' - A detailed error code.
--
-- * 'edDetailedErrorMessage' - A detailed error message.
errorDetail
    :: ErrorDetail
errorDetail =
  ErrorDetail'
    {_edDetailedErrorCode = Nothing, _edDetailedErrorMessage = Nothing}


-- | A detailed error code.
edDetailedErrorCode :: Lens' ErrorDetail (Maybe Text)
edDetailedErrorCode = lens _edDetailedErrorCode (\ s a -> s{_edDetailedErrorCode = a})

-- | A detailed error message.
edDetailedErrorMessage :: Lens' ErrorDetail (Maybe Text)
edDetailedErrorMessage = lens _edDetailedErrorMessage (\ s a -> s{_edDetailedErrorMessage = a})

instance FromJSON ErrorDetail where
        parseJSON
          = withObject "ErrorDetail"
              (\ x ->
                 ErrorDetail' <$>
                   (x .:? "DetailedErrorCode") <*>
                     (x .:? "DetailedErrorMessage"))

instance Hashable ErrorDetail where

instance NFData ErrorDetail where

-- | Information about a Lambda function.
--
-- /See:/ 'function' smart constructor.
data Function = Function'
  { _fFunctionARN           :: !(Maybe Text)
  , _fFunctionConfiguration :: !(Maybe FunctionConfiguration)
  , _fId                    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Function' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fFunctionARN' - The ARN of the Lambda function.
--
-- * 'fFunctionConfiguration' - The configuration of the Lambda function.
--
-- * 'fId' - The ID of the Lambda function.
function
    :: Function
function =
  Function'
    {_fFunctionARN = Nothing, _fFunctionConfiguration = Nothing, _fId = Nothing}


-- | The ARN of the Lambda function.
fFunctionARN :: Lens' Function (Maybe Text)
fFunctionARN = lens _fFunctionARN (\ s a -> s{_fFunctionARN = a})

-- | The configuration of the Lambda function.
fFunctionConfiguration :: Lens' Function (Maybe FunctionConfiguration)
fFunctionConfiguration = lens _fFunctionConfiguration (\ s a -> s{_fFunctionConfiguration = a})

-- | The ID of the Lambda function.
fId :: Lens' Function (Maybe Text)
fId = lens _fId (\ s a -> s{_fId = a})

instance FromJSON Function where
        parseJSON
          = withObject "Function"
              (\ x ->
                 Function' <$>
                   (x .:? "FunctionArn") <*>
                     (x .:? "FunctionConfiguration")
                     <*> (x .:? "Id"))

instance Hashable Function where

instance NFData Function where

instance ToJSON Function where
        toJSON Function'{..}
          = object
              (catMaybes
                 [("FunctionArn" .=) <$> _fFunctionARN,
                  ("FunctionConfiguration" .=) <$>
                    _fFunctionConfiguration,
                  ("Id" .=) <$> _fId])

-- | The configuration of the Lambda function.
--
-- /See:/ 'functionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { _fcMemorySize   :: !(Maybe Int)
  , _fcExecArgs     :: !(Maybe Text)
  , _fcEnvironment  :: !(Maybe FunctionConfigurationEnvironment)
  , _fcExecutable   :: !(Maybe Text)
  , _fcPinned       :: !(Maybe Bool)
  , _fcEncodingType :: !(Maybe EncodingType)
  , _fcTimeout      :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcMemorySize' - The memory size, in KB, which the function requires.
--
-- * 'fcExecArgs' - The execution arguments.
--
-- * 'fcEnvironment' - The environment configuration of the function.
--
-- * 'fcExecutable' - The name of the function executable.
--
-- * 'fcPinned' - True if the function is pinned. Pinned means the function is long-lived and starts when the core starts.
--
-- * 'fcEncodingType' - The expected encoding type of the input payload for the function. The default is ''json''.
--
-- * 'fcTimeout' - The allowed function execution time, after which Lambda should terminate the function. This timeout still applies to pinned lambdas for each request.
functionConfiguration
    :: FunctionConfiguration
functionConfiguration =
  FunctionConfiguration'
    { _fcMemorySize = Nothing
    , _fcExecArgs = Nothing
    , _fcEnvironment = Nothing
    , _fcExecutable = Nothing
    , _fcPinned = Nothing
    , _fcEncodingType = Nothing
    , _fcTimeout = Nothing
    }


-- | The memory size, in KB, which the function requires.
fcMemorySize :: Lens' FunctionConfiguration (Maybe Int)
fcMemorySize = lens _fcMemorySize (\ s a -> s{_fcMemorySize = a})

-- | The execution arguments.
fcExecArgs :: Lens' FunctionConfiguration (Maybe Text)
fcExecArgs = lens _fcExecArgs (\ s a -> s{_fcExecArgs = a})

-- | The environment configuration of the function.
fcEnvironment :: Lens' FunctionConfiguration (Maybe FunctionConfigurationEnvironment)
fcEnvironment = lens _fcEnvironment (\ s a -> s{_fcEnvironment = a})

-- | The name of the function executable.
fcExecutable :: Lens' FunctionConfiguration (Maybe Text)
fcExecutable = lens _fcExecutable (\ s a -> s{_fcExecutable = a})

-- | True if the function is pinned. Pinned means the function is long-lived and starts when the core starts.
fcPinned :: Lens' FunctionConfiguration (Maybe Bool)
fcPinned = lens _fcPinned (\ s a -> s{_fcPinned = a})

-- | The expected encoding type of the input payload for the function. The default is ''json''.
fcEncodingType :: Lens' FunctionConfiguration (Maybe EncodingType)
fcEncodingType = lens _fcEncodingType (\ s a -> s{_fcEncodingType = a})

-- | The allowed function execution time, after which Lambda should terminate the function. This timeout still applies to pinned lambdas for each request.
fcTimeout :: Lens' FunctionConfiguration (Maybe Int)
fcTimeout = lens _fcTimeout (\ s a -> s{_fcTimeout = a})

instance FromJSON FunctionConfiguration where
        parseJSON
          = withObject "FunctionConfiguration"
              (\ x ->
                 FunctionConfiguration' <$>
                   (x .:? "MemorySize") <*> (x .:? "ExecArgs") <*>
                     (x .:? "Environment")
                     <*> (x .:? "Executable")
                     <*> (x .:? "Pinned")
                     <*> (x .:? "EncodingType")
                     <*> (x .:? "Timeout"))

instance Hashable FunctionConfiguration where

instance NFData FunctionConfiguration where

instance ToJSON FunctionConfiguration where
        toJSON FunctionConfiguration'{..}
          = object
              (catMaybes
                 [("MemorySize" .=) <$> _fcMemorySize,
                  ("ExecArgs" .=) <$> _fcExecArgs,
                  ("Environment" .=) <$> _fcEnvironment,
                  ("Executable" .=) <$> _fcExecutable,
                  ("Pinned" .=) <$> _fcPinned,
                  ("EncodingType" .=) <$> _fcEncodingType,
                  ("Timeout" .=) <$> _fcTimeout])

-- | The environment configuration of the function.
--
-- /See:/ 'functionConfigurationEnvironment' smart constructor.
data FunctionConfigurationEnvironment = FunctionConfigurationEnvironment'
  { _fceVariables              :: !(Maybe (Map Text Text))
  , _fceResourceAccessPolicies :: !(Maybe [ResourceAccessPolicy])
  , _fceAccessSysfs            :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FunctionConfigurationEnvironment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fceVariables' - Environment variables for the Lambda function's configuration.
--
-- * 'fceResourceAccessPolicies' - A list of the resources, with their permissions, to which the Lambda function will be granted access. A Lambda function can have at most 10 resources.
--
-- * 'fceAccessSysfs' - If true, the Lambda function is allowed to access the host's /sys folder. Use this when the Lambda function needs to read device information from /sys.
functionConfigurationEnvironment
    :: FunctionConfigurationEnvironment
functionConfigurationEnvironment =
  FunctionConfigurationEnvironment'
    { _fceVariables = Nothing
    , _fceResourceAccessPolicies = Nothing
    , _fceAccessSysfs = Nothing
    }


-- | Environment variables for the Lambda function's configuration.
fceVariables :: Lens' FunctionConfigurationEnvironment (HashMap Text Text)
fceVariables = lens _fceVariables (\ s a -> s{_fceVariables = a}) . _Default . _Map

-- | A list of the resources, with their permissions, to which the Lambda function will be granted access. A Lambda function can have at most 10 resources.
fceResourceAccessPolicies :: Lens' FunctionConfigurationEnvironment [ResourceAccessPolicy]
fceResourceAccessPolicies = lens _fceResourceAccessPolicies (\ s a -> s{_fceResourceAccessPolicies = a}) . _Default . _Coerce

-- | If true, the Lambda function is allowed to access the host's /sys folder. Use this when the Lambda function needs to read device information from /sys.
fceAccessSysfs :: Lens' FunctionConfigurationEnvironment (Maybe Bool)
fceAccessSysfs = lens _fceAccessSysfs (\ s a -> s{_fceAccessSysfs = a})

instance FromJSON FunctionConfigurationEnvironment
         where
        parseJSON
          = withObject "FunctionConfigurationEnvironment"
              (\ x ->
                 FunctionConfigurationEnvironment' <$>
                   (x .:? "Variables" .!= mempty) <*>
                     (x .:? "ResourceAccessPolicies" .!= mempty)
                     <*> (x .:? "AccessSysfs"))

instance Hashable FunctionConfigurationEnvironment
         where

instance NFData FunctionConfigurationEnvironment
         where

instance ToJSON FunctionConfigurationEnvironment
         where
        toJSON FunctionConfigurationEnvironment'{..}
          = object
              (catMaybes
                 [("Variables" .=) <$> _fceVariables,
                  ("ResourceAccessPolicies" .=) <$>
                    _fceResourceAccessPolicies,
                  ("AccessSysfs" .=) <$> _fceAccessSysfs])

-- | Information about a function definition version.
--
-- /See:/ 'functionDefinitionVersion' smart constructor.
newtype FunctionDefinitionVersion = FunctionDefinitionVersion'
  { _fdvFunctions :: Maybe [Function]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FunctionDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdvFunctions' - A list of Lambda functions in this function definition version.
functionDefinitionVersion
    :: FunctionDefinitionVersion
functionDefinitionVersion = FunctionDefinitionVersion' {_fdvFunctions = Nothing}


-- | A list of Lambda functions in this function definition version.
fdvFunctions :: Lens' FunctionDefinitionVersion [Function]
fdvFunctions = lens _fdvFunctions (\ s a -> s{_fdvFunctions = a}) . _Default . _Coerce

instance FromJSON FunctionDefinitionVersion where
        parseJSON
          = withObject "FunctionDefinitionVersion"
              (\ x ->
                 FunctionDefinitionVersion' <$>
                   (x .:? "Functions" .!= mempty))

instance Hashable FunctionDefinitionVersion where

instance NFData FunctionDefinitionVersion where

instance ToJSON FunctionDefinitionVersion where
        toJSON FunctionDefinitionVersion'{..}
          = object
              (catMaybes [("Functions" .=) <$> _fdvFunctions])

-- | Information about a logger
--
-- /See:/ 'greengrassLogger' smart constructor.
data GreengrassLogger = GreengrassLogger'
  { _glSpace     :: !(Maybe Int)
  , _glComponent :: !(Maybe LoggerComponent)
  , _glId        :: !(Maybe Text)
  , _glType      :: !(Maybe LoggerType)
  , _glLevel     :: !(Maybe LoggerLevel)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GreengrassLogger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glSpace' - The amount of file space, in KB, to use if the local file system is used for logging purposes.
--
-- * 'glComponent' - The component that will be subject to logging.
--
-- * 'glId' - The id of the logger.
--
-- * 'glType' - The type of log output which will be used.
--
-- * 'glLevel' - The level of the logs.
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


-- | The amount of file space, in KB, to use if the local file system is used for logging purposes.
glSpace :: Lens' GreengrassLogger (Maybe Int)
glSpace = lens _glSpace (\ s a -> s{_glSpace = a})

-- | The component that will be subject to logging.
glComponent :: Lens' GreengrassLogger (Maybe LoggerComponent)
glComponent = lens _glComponent (\ s a -> s{_glComponent = a})

-- | The id of the logger.
glId :: Lens' GreengrassLogger (Maybe Text)
glId = lens _glId (\ s a -> s{_glId = a})

-- | The type of log output which will be used.
glType :: Lens' GreengrassLogger (Maybe LoggerType)
glType = lens _glType (\ s a -> s{_glType = a})

-- | The level of the logs.
glLevel :: Lens' GreengrassLogger (Maybe LoggerLevel)
glLevel = lens _glLevel (\ s a -> s{_glLevel = a})

instance FromJSON GreengrassLogger where
        parseJSON
          = withObject "GreengrassLogger"
              (\ x ->
                 GreengrassLogger' <$>
                   (x .:? "Space") <*> (x .:? "Component") <*>
                     (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "Level"))

instance Hashable GreengrassLogger where

instance NFData GreengrassLogger where

instance ToJSON GreengrassLogger where
        toJSON GreengrassLogger'{..}
          = object
              (catMaybes
                 [("Space" .=) <$> _glSpace,
                  ("Component" .=) <$> _glComponent,
                  ("Id" .=) <$> _glId, ("Type" .=) <$> _glType,
                  ("Level" .=) <$> _glLevel])

-- | Information about a certificate authority for a group.
--
-- /See:/ 'groupCertificateAuthorityProperties' smart constructor.
data GroupCertificateAuthorityProperties = GroupCertificateAuthorityProperties'
  { _gcapGroupCertificateAuthorityARN :: !(Maybe Text)
  , _gcapGroupCertificateAuthorityId  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupCertificateAuthorityProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcapGroupCertificateAuthorityARN' - The ARN of the certificate authority for the group.
--
-- * 'gcapGroupCertificateAuthorityId' - The ID of the certificate authority for the group.
groupCertificateAuthorityProperties
    :: GroupCertificateAuthorityProperties
groupCertificateAuthorityProperties =
  GroupCertificateAuthorityProperties'
    { _gcapGroupCertificateAuthorityARN = Nothing
    , _gcapGroupCertificateAuthorityId = Nothing
    }


-- | The ARN of the certificate authority for the group.
gcapGroupCertificateAuthorityARN :: Lens' GroupCertificateAuthorityProperties (Maybe Text)
gcapGroupCertificateAuthorityARN = lens _gcapGroupCertificateAuthorityARN (\ s a -> s{_gcapGroupCertificateAuthorityARN = a})

-- | The ID of the certificate authority for the group.
gcapGroupCertificateAuthorityId :: Lens' GroupCertificateAuthorityProperties (Maybe Text)
gcapGroupCertificateAuthorityId = lens _gcapGroupCertificateAuthorityId (\ s a -> s{_gcapGroupCertificateAuthorityId = a})

instance FromJSON GroupCertificateAuthorityProperties
         where
        parseJSON
          = withObject "GroupCertificateAuthorityProperties"
              (\ x ->
                 GroupCertificateAuthorityProperties' <$>
                   (x .:? "GroupCertificateAuthorityArn") <*>
                     (x .:? "GroupCertificateAuthorityId"))

instance Hashable GroupCertificateAuthorityProperties
         where

instance NFData GroupCertificateAuthorityProperties
         where

-- | Information about a group.
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giLatestVersionARN' - The ARN of the latest version of the group.
--
-- * 'giARN' - The ARN of the group.
--
-- * 'giName' - The name of the group.
--
-- * 'giCreationTimestamp' - The time, in milliseconds since the epoch, when the group was created.
--
-- * 'giId' - The ID of the group.
--
-- * 'giLatestVersion' - The latest version of the group.
--
-- * 'giLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the group was last updated.
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


-- | The ARN of the latest version of the group.
giLatestVersionARN :: Lens' GroupInformation (Maybe Text)
giLatestVersionARN = lens _giLatestVersionARN (\ s a -> s{_giLatestVersionARN = a})

-- | The ARN of the group.
giARN :: Lens' GroupInformation (Maybe Text)
giARN = lens _giARN (\ s a -> s{_giARN = a})

-- | The name of the group.
giName :: Lens' GroupInformation (Maybe Text)
giName = lens _giName (\ s a -> s{_giName = a})

-- | The time, in milliseconds since the epoch, when the group was created.
giCreationTimestamp :: Lens' GroupInformation (Maybe Text)
giCreationTimestamp = lens _giCreationTimestamp (\ s a -> s{_giCreationTimestamp = a})

-- | The ID of the group.
giId :: Lens' GroupInformation (Maybe Text)
giId = lens _giId (\ s a -> s{_giId = a})

-- | The latest version of the group.
giLatestVersion :: Lens' GroupInformation (Maybe Text)
giLatestVersion = lens _giLatestVersion (\ s a -> s{_giLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the group was last updated.
giLastUpdatedTimestamp :: Lens' GroupInformation (Maybe Text)
giLastUpdatedTimestamp = lens _giLastUpdatedTimestamp (\ s a -> s{_giLastUpdatedTimestamp = a})

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

instance Hashable GroupInformation where

instance NFData GroupInformation where

-- | Group owner related settings for local resources.
--
-- /See:/ 'groupOwnerSetting' smart constructor.
data GroupOwnerSetting = GroupOwnerSetting'
  { _gosAutoAddGroupOwner :: !(Maybe Bool)
  , _gosGroupOwner        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupOwnerSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gosAutoAddGroupOwner' - If true, GreenGrass automatically adds the specified Linux OS group owner of the resource to the Lambda process privileges. Thus the Lambda process will have the file access permissions of the added Linux group.
--
-- * 'gosGroupOwner' - The name of the Linux OS group whose privileges will be added to the Lambda process. This field is optional.
groupOwnerSetting
    :: GroupOwnerSetting
groupOwnerSetting =
  GroupOwnerSetting' {_gosAutoAddGroupOwner = Nothing, _gosGroupOwner = Nothing}


-- | If true, GreenGrass automatically adds the specified Linux OS group owner of the resource to the Lambda process privileges. Thus the Lambda process will have the file access permissions of the added Linux group.
gosAutoAddGroupOwner :: Lens' GroupOwnerSetting (Maybe Bool)
gosAutoAddGroupOwner = lens _gosAutoAddGroupOwner (\ s a -> s{_gosAutoAddGroupOwner = a})

-- | The name of the Linux OS group whose privileges will be added to the Lambda process. This field is optional.
gosGroupOwner :: Lens' GroupOwnerSetting (Maybe Text)
gosGroupOwner = lens _gosGroupOwner (\ s a -> s{_gosGroupOwner = a})

instance FromJSON GroupOwnerSetting where
        parseJSON
          = withObject "GroupOwnerSetting"
              (\ x ->
                 GroupOwnerSetting' <$>
                   (x .:? "AutoAddGroupOwner") <*> (x .:? "GroupOwner"))

instance Hashable GroupOwnerSetting where

instance NFData GroupOwnerSetting where

instance ToJSON GroupOwnerSetting where
        toJSON GroupOwnerSetting'{..}
          = object
              (catMaybes
                 [("AutoAddGroupOwner" .=) <$> _gosAutoAddGroupOwner,
                  ("GroupOwner" .=) <$> _gosGroupOwner])

-- | Information about a group version.
--
-- /See:/ 'groupVersion' smart constructor.
data GroupVersion = GroupVersion'
  { _gvResourceDefinitionVersionARN     :: !(Maybe Text)
  , _gvSubscriptionDefinitionVersionARN :: !(Maybe Text)
  , _gvCoreDefinitionVersionARN         :: !(Maybe Text)
  , _gvDeviceDefinitionVersionARN       :: !(Maybe Text)
  , _gvFunctionDefinitionVersionARN     :: !(Maybe Text)
  , _gvLoggerDefinitionVersionARN       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvResourceDefinitionVersionARN' - The resource definition version ARN for this group.
--
-- * 'gvSubscriptionDefinitionVersionARN' - The ARN of the subscription definition version for this group.
--
-- * 'gvCoreDefinitionVersionARN' - The ARN of the core definition version for this group.
--
-- * 'gvDeviceDefinitionVersionARN' - The ARN of the device definition version for this group.
--
-- * 'gvFunctionDefinitionVersionARN' - The ARN of the function definition version for this group.
--
-- * 'gvLoggerDefinitionVersionARN' - The ARN of the logger definition version for this group.
groupVersion
    :: GroupVersion
groupVersion =
  GroupVersion'
    { _gvResourceDefinitionVersionARN = Nothing
    , _gvSubscriptionDefinitionVersionARN = Nothing
    , _gvCoreDefinitionVersionARN = Nothing
    , _gvDeviceDefinitionVersionARN = Nothing
    , _gvFunctionDefinitionVersionARN = Nothing
    , _gvLoggerDefinitionVersionARN = Nothing
    }


-- | The resource definition version ARN for this group.
gvResourceDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvResourceDefinitionVersionARN = lens _gvResourceDefinitionVersionARN (\ s a -> s{_gvResourceDefinitionVersionARN = a})

-- | The ARN of the subscription definition version for this group.
gvSubscriptionDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvSubscriptionDefinitionVersionARN = lens _gvSubscriptionDefinitionVersionARN (\ s a -> s{_gvSubscriptionDefinitionVersionARN = a})

-- | The ARN of the core definition version for this group.
gvCoreDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvCoreDefinitionVersionARN = lens _gvCoreDefinitionVersionARN (\ s a -> s{_gvCoreDefinitionVersionARN = a})

-- | The ARN of the device definition version for this group.
gvDeviceDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvDeviceDefinitionVersionARN = lens _gvDeviceDefinitionVersionARN (\ s a -> s{_gvDeviceDefinitionVersionARN = a})

-- | The ARN of the function definition version for this group.
gvFunctionDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvFunctionDefinitionVersionARN = lens _gvFunctionDefinitionVersionARN (\ s a -> s{_gvFunctionDefinitionVersionARN = a})

-- | The ARN of the logger definition version for this group.
gvLoggerDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvLoggerDefinitionVersionARN = lens _gvLoggerDefinitionVersionARN (\ s a -> s{_gvLoggerDefinitionVersionARN = a})

instance FromJSON GroupVersion where
        parseJSON
          = withObject "GroupVersion"
              (\ x ->
                 GroupVersion' <$>
                   (x .:? "ResourceDefinitionVersionArn") <*>
                     (x .:? "SubscriptionDefinitionVersionArn")
                     <*> (x .:? "CoreDefinitionVersionArn")
                     <*> (x .:? "DeviceDefinitionVersionArn")
                     <*> (x .:? "FunctionDefinitionVersionArn")
                     <*> (x .:? "LoggerDefinitionVersionArn"))

instance Hashable GroupVersion where

instance NFData GroupVersion where

instance ToJSON GroupVersion where
        toJSON GroupVersion'{..}
          = object
              (catMaybes
                 [("ResourceDefinitionVersionArn" .=) <$>
                    _gvResourceDefinitionVersionARN,
                  ("SubscriptionDefinitionVersionArn" .=) <$>
                    _gvSubscriptionDefinitionVersionARN,
                  ("CoreDefinitionVersionArn" .=) <$>
                    _gvCoreDefinitionVersionARN,
                  ("DeviceDefinitionVersionArn" .=) <$>
                    _gvDeviceDefinitionVersionARN,
                  ("FunctionDefinitionVersionArn" .=) <$>
                    _gvFunctionDefinitionVersionARN,
                  ("LoggerDefinitionVersionArn" .=) <$>
                    _gvLoggerDefinitionVersionARN])

-- | Attributes that define a local device resource.
--
-- /See:/ 'localDeviceResourceData' smart constructor.
data LocalDeviceResourceData = LocalDeviceResourceData'
  { _ldrdGroupOwnerSetting :: !(Maybe GroupOwnerSetting)
  , _ldrdSourcePath        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LocalDeviceResourceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrdGroupOwnerSetting' - Group/owner related settings for local resources.
--
-- * 'ldrdSourcePath' - The local absolute path of the device resource. The source path for a device resource can refer only to a character device or block device under ''/dev''.
localDeviceResourceData
    :: LocalDeviceResourceData
localDeviceResourceData =
  LocalDeviceResourceData'
    {_ldrdGroupOwnerSetting = Nothing, _ldrdSourcePath = Nothing}


-- | Group/owner related settings for local resources.
ldrdGroupOwnerSetting :: Lens' LocalDeviceResourceData (Maybe GroupOwnerSetting)
ldrdGroupOwnerSetting = lens _ldrdGroupOwnerSetting (\ s a -> s{_ldrdGroupOwnerSetting = a})

-- | The local absolute path of the device resource. The source path for a device resource can refer only to a character device or block device under ''/dev''.
ldrdSourcePath :: Lens' LocalDeviceResourceData (Maybe Text)
ldrdSourcePath = lens _ldrdSourcePath (\ s a -> s{_ldrdSourcePath = a})

instance FromJSON LocalDeviceResourceData where
        parseJSON
          = withObject "LocalDeviceResourceData"
              (\ x ->
                 LocalDeviceResourceData' <$>
                   (x .:? "GroupOwnerSetting") <*> (x .:? "SourcePath"))

instance Hashable LocalDeviceResourceData where

instance NFData LocalDeviceResourceData where

instance ToJSON LocalDeviceResourceData where
        toJSON LocalDeviceResourceData'{..}
          = object
              (catMaybes
                 [("GroupOwnerSetting" .=) <$> _ldrdGroupOwnerSetting,
                  ("SourcePath" .=) <$> _ldrdSourcePath])

-- | Attributes that define a local volume resource.
--
-- /See:/ 'localVolumeResourceData' smart constructor.
data LocalVolumeResourceData = LocalVolumeResourceData'
  { _lvrdGroupOwnerSetting :: !(Maybe GroupOwnerSetting)
  , _lvrdDestinationPath   :: !(Maybe Text)
  , _lvrdSourcePath        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LocalVolumeResourceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvrdGroupOwnerSetting' - Allows you to configure additional group privileges for the Lambda process. This field is optional.
--
-- * 'lvrdDestinationPath' - The absolute local path of the resource inside the lambda environment.
--
-- * 'lvrdSourcePath' - The local absolute path of the volume resource on the host. The source path for a volume resource type cannot start with ''/proc'' or ''/sys''.
localVolumeResourceData
    :: LocalVolumeResourceData
localVolumeResourceData =
  LocalVolumeResourceData'
    { _lvrdGroupOwnerSetting = Nothing
    , _lvrdDestinationPath = Nothing
    , _lvrdSourcePath = Nothing
    }


-- | Allows you to configure additional group privileges for the Lambda process. This field is optional.
lvrdGroupOwnerSetting :: Lens' LocalVolumeResourceData (Maybe GroupOwnerSetting)
lvrdGroupOwnerSetting = lens _lvrdGroupOwnerSetting (\ s a -> s{_lvrdGroupOwnerSetting = a})

-- | The absolute local path of the resource inside the lambda environment.
lvrdDestinationPath :: Lens' LocalVolumeResourceData (Maybe Text)
lvrdDestinationPath = lens _lvrdDestinationPath (\ s a -> s{_lvrdDestinationPath = a})

-- | The local absolute path of the volume resource on the host. The source path for a volume resource type cannot start with ''/proc'' or ''/sys''.
lvrdSourcePath :: Lens' LocalVolumeResourceData (Maybe Text)
lvrdSourcePath = lens _lvrdSourcePath (\ s a -> s{_lvrdSourcePath = a})

instance FromJSON LocalVolumeResourceData where
        parseJSON
          = withObject "LocalVolumeResourceData"
              (\ x ->
                 LocalVolumeResourceData' <$>
                   (x .:? "GroupOwnerSetting") <*>
                     (x .:? "DestinationPath")
                     <*> (x .:? "SourcePath"))

instance Hashable LocalVolumeResourceData where

instance NFData LocalVolumeResourceData where

instance ToJSON LocalVolumeResourceData where
        toJSON LocalVolumeResourceData'{..}
          = object
              (catMaybes
                 [("GroupOwnerSetting" .=) <$> _lvrdGroupOwnerSetting,
                  ("DestinationPath" .=) <$> _lvrdDestinationPath,
                  ("SourcePath" .=) <$> _lvrdSourcePath])

-- | Information about a logger definition version.
--
-- /See:/ 'loggerDefinitionVersion' smart constructor.
newtype LoggerDefinitionVersion = LoggerDefinitionVersion'
  { _ldvLoggers :: Maybe [GreengrassLogger]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoggerDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldvLoggers' - A list of loggers.
loggerDefinitionVersion
    :: LoggerDefinitionVersion
loggerDefinitionVersion = LoggerDefinitionVersion' {_ldvLoggers = Nothing}


-- | A list of loggers.
ldvLoggers :: Lens' LoggerDefinitionVersion [GreengrassLogger]
ldvLoggers = lens _ldvLoggers (\ s a -> s{_ldvLoggers = a}) . _Default . _Coerce

instance FromJSON LoggerDefinitionVersion where
        parseJSON
          = withObject "LoggerDefinitionVersion"
              (\ x ->
                 LoggerDefinitionVersion' <$>
                   (x .:? "Loggers" .!= mempty))

instance Hashable LoggerDefinitionVersion where

instance NFData LoggerDefinitionVersion where

instance ToJSON LoggerDefinitionVersion where
        toJSON LoggerDefinitionVersion'{..}
          = object (catMaybes [("Loggers" .=) <$> _ldvLoggers])

-- | Information about a resource.
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
  { _rResourceDataContainer :: !(Maybe ResourceDataContainer)
  , _rName                  :: !(Maybe Text)
  , _rId                    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceDataContainer' - A container of data for all resource types.
--
-- * 'rName' - The descriptive resource name, which is displayed on the Greengrass console. Max length 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
--
-- * 'rId' - The resource ID, used to refer to a resource in the Lambda function configuration. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
resource
    :: Resource
resource =
  Resource'
    {_rResourceDataContainer = Nothing, _rName = Nothing, _rId = Nothing}


-- | A container of data for all resource types.
rResourceDataContainer :: Lens' Resource (Maybe ResourceDataContainer)
rResourceDataContainer = lens _rResourceDataContainer (\ s a -> s{_rResourceDataContainer = a})

-- | The descriptive resource name, which is displayed on the Greengrass console. Max length 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
rName :: Lens' Resource (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a})

-- | The resource ID, used to refer to a resource in the Lambda function configuration. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
rId :: Lens' Resource (Maybe Text)
rId = lens _rId (\ s a -> s{_rId = a})

instance FromJSON Resource where
        parseJSON
          = withObject "Resource"
              (\ x ->
                 Resource' <$>
                   (x .:? "ResourceDataContainer") <*> (x .:? "Name")
                     <*> (x .:? "Id"))

instance Hashable Resource where

instance NFData Resource where

instance ToJSON Resource where
        toJSON Resource'{..}
          = object
              (catMaybes
                 [("ResourceDataContainer" .=) <$>
                    _rResourceDataContainer,
                  ("Name" .=) <$> _rName, ("Id" .=) <$> _rId])

-- | A policy used by the function to access a resource.
--
-- /See:/ 'resourceAccessPolicy' smart constructor.
data ResourceAccessPolicy = ResourceAccessPolicy'
  { _rapResourceId :: !(Maybe Text)
  , _rapPermission :: !(Maybe Permission)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceAccessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rapResourceId' - The ID of the resource. (This ID is assigned to the resource when you create the resource definiton.)
--
-- * 'rapPermission' - The permissions that the Lambda function has to the resource. Can be one of ''rw'' (read/write) or ''ro'' (read-only).
resourceAccessPolicy
    :: ResourceAccessPolicy
resourceAccessPolicy =
  ResourceAccessPolicy' {_rapResourceId = Nothing, _rapPermission = Nothing}


-- | The ID of the resource. (This ID is assigned to the resource when you create the resource definiton.)
rapResourceId :: Lens' ResourceAccessPolicy (Maybe Text)
rapResourceId = lens _rapResourceId (\ s a -> s{_rapResourceId = a})

-- | The permissions that the Lambda function has to the resource. Can be one of ''rw'' (read/write) or ''ro'' (read-only).
rapPermission :: Lens' ResourceAccessPolicy (Maybe Permission)
rapPermission = lens _rapPermission (\ s a -> s{_rapPermission = a})

instance FromJSON ResourceAccessPolicy where
        parseJSON
          = withObject "ResourceAccessPolicy"
              (\ x ->
                 ResourceAccessPolicy' <$>
                   (x .:? "ResourceId") <*> (x .:? "Permission"))

instance Hashable ResourceAccessPolicy where

instance NFData ResourceAccessPolicy where

instance ToJSON ResourceAccessPolicy where
        toJSON ResourceAccessPolicy'{..}
          = object
              (catMaybes
                 [("ResourceId" .=) <$> _rapResourceId,
                  ("Permission" .=) <$> _rapPermission])

-- | A container for resource data. The container takes only one of the following supported resource data types: ''LocalDeviceResourceData'', ''LocalVolumeResourceData'', ''SageMakerMachineLearningModelResourceData'', ''S3MachineLearningModelResourceData''.
--
-- /See:/ 'resourceDataContainer' smart constructor.
data ResourceDataContainer = ResourceDataContainer'
  { _rdcS3MachineLearningModelResourceData :: !(Maybe S3MachineLearningModelResourceData)
  , _rdcSageMakerMachineLearningModelResourceData :: !(Maybe SageMakerMachineLearningModelResourceData)
  , _rdcLocalVolumeResourceData :: !(Maybe LocalVolumeResourceData)
  , _rdcLocalDeviceResourceData :: !(Maybe LocalDeviceResourceData)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceDataContainer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcS3MachineLearningModelResourceData' - Attributes that define an S3 machine learning resource.
--
-- * 'rdcSageMakerMachineLearningModelResourceData' - Attributes that define an SageMaker machine learning resource.
--
-- * 'rdcLocalVolumeResourceData' - Attributes that define the local volume resource.
--
-- * 'rdcLocalDeviceResourceData' - Attributes that define the local device resource.
resourceDataContainer
    :: ResourceDataContainer
resourceDataContainer =
  ResourceDataContainer'
    { _rdcS3MachineLearningModelResourceData = Nothing
    , _rdcSageMakerMachineLearningModelResourceData = Nothing
    , _rdcLocalVolumeResourceData = Nothing
    , _rdcLocalDeviceResourceData = Nothing
    }


-- | Attributes that define an S3 machine learning resource.
rdcS3MachineLearningModelResourceData :: Lens' ResourceDataContainer (Maybe S3MachineLearningModelResourceData)
rdcS3MachineLearningModelResourceData = lens _rdcS3MachineLearningModelResourceData (\ s a -> s{_rdcS3MachineLearningModelResourceData = a})

-- | Attributes that define an SageMaker machine learning resource.
rdcSageMakerMachineLearningModelResourceData :: Lens' ResourceDataContainer (Maybe SageMakerMachineLearningModelResourceData)
rdcSageMakerMachineLearningModelResourceData = lens _rdcSageMakerMachineLearningModelResourceData (\ s a -> s{_rdcSageMakerMachineLearningModelResourceData = a})

-- | Attributes that define the local volume resource.
rdcLocalVolumeResourceData :: Lens' ResourceDataContainer (Maybe LocalVolumeResourceData)
rdcLocalVolumeResourceData = lens _rdcLocalVolumeResourceData (\ s a -> s{_rdcLocalVolumeResourceData = a})

-- | Attributes that define the local device resource.
rdcLocalDeviceResourceData :: Lens' ResourceDataContainer (Maybe LocalDeviceResourceData)
rdcLocalDeviceResourceData = lens _rdcLocalDeviceResourceData (\ s a -> s{_rdcLocalDeviceResourceData = a})

instance FromJSON ResourceDataContainer where
        parseJSON
          = withObject "ResourceDataContainer"
              (\ x ->
                 ResourceDataContainer' <$>
                   (x .:? "S3MachineLearningModelResourceData") <*>
                     (x .:? "SageMakerMachineLearningModelResourceData")
                     <*> (x .:? "LocalVolumeResourceData")
                     <*> (x .:? "LocalDeviceResourceData"))

instance Hashable ResourceDataContainer where

instance NFData ResourceDataContainer where

instance ToJSON ResourceDataContainer where
        toJSON ResourceDataContainer'{..}
          = object
              (catMaybes
                 [("S3MachineLearningModelResourceData" .=) <$>
                    _rdcS3MachineLearningModelResourceData,
                  ("SageMakerMachineLearningModelResourceData" .=) <$>
                    _rdcSageMakerMachineLearningModelResourceData,
                  ("LocalVolumeResourceData" .=) <$>
                    _rdcLocalVolumeResourceData,
                  ("LocalDeviceResourceData" .=) <$>
                    _rdcLocalDeviceResourceData])

-- | Information about a resource definition version.
--
-- /See:/ 'resourceDefinitionVersion' smart constructor.
newtype ResourceDefinitionVersion = ResourceDefinitionVersion'
  { _rdvResources :: Maybe [Resource]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdvResources' - A list of resources.
resourceDefinitionVersion
    :: ResourceDefinitionVersion
resourceDefinitionVersion = ResourceDefinitionVersion' {_rdvResources = Nothing}


-- | A list of resources.
rdvResources :: Lens' ResourceDefinitionVersion [Resource]
rdvResources = lens _rdvResources (\ s a -> s{_rdvResources = a}) . _Default . _Coerce

instance FromJSON ResourceDefinitionVersion where
        parseJSON
          = withObject "ResourceDefinitionVersion"
              (\ x ->
                 ResourceDefinitionVersion' <$>
                   (x .:? "Resources" .!= mempty))

instance Hashable ResourceDefinitionVersion where

instance NFData ResourceDefinitionVersion where

instance ToJSON ResourceDefinitionVersion where
        toJSON ResourceDefinitionVersion'{..}
          = object
              (catMaybes [("Resources" .=) <$> _rdvResources])

-- | Attributes that define an S3 machine learning resource.
--
-- /See:/ 's3MachineLearningModelResourceData' smart constructor.
data S3MachineLearningModelResourceData = S3MachineLearningModelResourceData'
  { _smlmrdDestinationPath :: !(Maybe Text)
  , _smlmrdS3URI           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3MachineLearningModelResourceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smlmrdDestinationPath' - The absolute local path of the resource inside the Lambda environment.
--
-- * 'smlmrdS3URI' - The URI of the source model in an S3 bucket. The model package must be in tar.gz or .zip format.
s3MachineLearningModelResourceData
    :: S3MachineLearningModelResourceData
s3MachineLearningModelResourceData =
  S3MachineLearningModelResourceData'
    {_smlmrdDestinationPath = Nothing, _smlmrdS3URI = Nothing}


-- | The absolute local path of the resource inside the Lambda environment.
smlmrdDestinationPath :: Lens' S3MachineLearningModelResourceData (Maybe Text)
smlmrdDestinationPath = lens _smlmrdDestinationPath (\ s a -> s{_smlmrdDestinationPath = a})

-- | The URI of the source model in an S3 bucket. The model package must be in tar.gz or .zip format.
smlmrdS3URI :: Lens' S3MachineLearningModelResourceData (Maybe Text)
smlmrdS3URI = lens _smlmrdS3URI (\ s a -> s{_smlmrdS3URI = a})

instance FromJSON S3MachineLearningModelResourceData
         where
        parseJSON
          = withObject "S3MachineLearningModelResourceData"
              (\ x ->
                 S3MachineLearningModelResourceData' <$>
                   (x .:? "DestinationPath") <*> (x .:? "S3Uri"))

instance Hashable S3MachineLearningModelResourceData
         where

instance NFData S3MachineLearningModelResourceData
         where

instance ToJSON S3MachineLearningModelResourceData
         where
        toJSON S3MachineLearningModelResourceData'{..}
          = object
              (catMaybes
                 [("DestinationPath" .=) <$> _smlmrdDestinationPath,
                  ("S3Uri" .=) <$> _smlmrdS3URI])

-- | Attributes that define an SageMaker machine learning resource.
--
-- /See:/ 'sageMakerMachineLearningModelResourceData' smart constructor.
data SageMakerMachineLearningModelResourceData = SageMakerMachineLearningModelResourceData'
  { _smmlmrdSageMakerJobARN :: !(Maybe Text)
  , _smmlmrdDestinationPath :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SageMakerMachineLearningModelResourceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smmlmrdSageMakerJobARN' - The ARN of the SageMaker training job that represents the source model.
--
-- * 'smmlmrdDestinationPath' - The absolute local path of the resource inside the Lambda environment.
sageMakerMachineLearningModelResourceData
    :: SageMakerMachineLearningModelResourceData
sageMakerMachineLearningModelResourceData =
  SageMakerMachineLearningModelResourceData'
    {_smmlmrdSageMakerJobARN = Nothing, _smmlmrdDestinationPath = Nothing}


-- | The ARN of the SageMaker training job that represents the source model.
smmlmrdSageMakerJobARN :: Lens' SageMakerMachineLearningModelResourceData (Maybe Text)
smmlmrdSageMakerJobARN = lens _smmlmrdSageMakerJobARN (\ s a -> s{_smmlmrdSageMakerJobARN = a})

-- | The absolute local path of the resource inside the Lambda environment.
smmlmrdDestinationPath :: Lens' SageMakerMachineLearningModelResourceData (Maybe Text)
smmlmrdDestinationPath = lens _smmlmrdDestinationPath (\ s a -> s{_smmlmrdDestinationPath = a})

instance FromJSON
           SageMakerMachineLearningModelResourceData
         where
        parseJSON
          = withObject
              "SageMakerMachineLearningModelResourceData"
              (\ x ->
                 SageMakerMachineLearningModelResourceData' <$>
                   (x .:? "SageMakerJobArn") <*>
                     (x .:? "DestinationPath"))

instance Hashable
           SageMakerMachineLearningModelResourceData
         where

instance NFData
           SageMakerMachineLearningModelResourceData
         where

instance ToJSON
           SageMakerMachineLearningModelResourceData
         where
        toJSON SageMakerMachineLearningModelResourceData'{..}
          = object
              (catMaybes
                 [("SageMakerJobArn" .=) <$> _smmlmrdSageMakerJobARN,
                  ("DestinationPath" .=) <$> _smmlmrdDestinationPath])

-- | Information about a subscription.
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'
  { _sSubject :: !(Maybe Text)
  , _sSource  :: !(Maybe Text)
  , _sId      :: !(Maybe Text)
  , _sTarget  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubject' - The subject of the message.
--
-- * 'sSource' - The source of the subscription. Can be a thing ARN, a Lambda function ARN, 'cloud' (which represents the IoT cloud), or 'GGShadowService'.
--
-- * 'sId' - The id of the subscription.
--
-- * 'sTarget' - Where the message is sent to. Can be a thing ARN, a Lambda function ARN, 'cloud' (which represents the IoT cloud), or 'GGShadowService'.
subscription
    :: Subscription
subscription =
  Subscription'
    { _sSubject = Nothing
    , _sSource = Nothing
    , _sId = Nothing
    , _sTarget = Nothing
    }


-- | The subject of the message.
sSubject :: Lens' Subscription (Maybe Text)
sSubject = lens _sSubject (\ s a -> s{_sSubject = a})

-- | The source of the subscription. Can be a thing ARN, a Lambda function ARN, 'cloud' (which represents the IoT cloud), or 'GGShadowService'.
sSource :: Lens' Subscription (Maybe Text)
sSource = lens _sSource (\ s a -> s{_sSource = a})

-- | The id of the subscription.
sId :: Lens' Subscription (Maybe Text)
sId = lens _sId (\ s a -> s{_sId = a})

-- | Where the message is sent to. Can be a thing ARN, a Lambda function ARN, 'cloud' (which represents the IoT cloud), or 'GGShadowService'.
sTarget :: Lens' Subscription (Maybe Text)
sTarget = lens _sTarget (\ s a -> s{_sTarget = a})

instance FromJSON Subscription where
        parseJSON
          = withObject "Subscription"
              (\ x ->
                 Subscription' <$>
                   (x .:? "Subject") <*> (x .:? "Source") <*>
                     (x .:? "Id")
                     <*> (x .:? "Target"))

instance Hashable Subscription where

instance NFData Subscription where

instance ToJSON Subscription where
        toJSON Subscription'{..}
          = object
              (catMaybes
                 [("Subject" .=) <$> _sSubject,
                  ("Source" .=) <$> _sSource, ("Id" .=) <$> _sId,
                  ("Target" .=) <$> _sTarget])

-- | Information about a subscription definition version.
--
-- /See:/ 'subscriptionDefinitionVersion' smart constructor.
newtype SubscriptionDefinitionVersion = SubscriptionDefinitionVersion'
  { _sdvSubscriptions :: Maybe [Subscription]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubscriptionDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdvSubscriptions' - A list of subscriptions.
subscriptionDefinitionVersion
    :: SubscriptionDefinitionVersion
subscriptionDefinitionVersion =
  SubscriptionDefinitionVersion' {_sdvSubscriptions = Nothing}


-- | A list of subscriptions.
sdvSubscriptions :: Lens' SubscriptionDefinitionVersion [Subscription]
sdvSubscriptions = lens _sdvSubscriptions (\ s a -> s{_sdvSubscriptions = a}) . _Default . _Coerce

instance FromJSON SubscriptionDefinitionVersion where
        parseJSON
          = withObject "SubscriptionDefinitionVersion"
              (\ x ->
                 SubscriptionDefinitionVersion' <$>
                   (x .:? "Subscriptions" .!= mempty))

instance Hashable SubscriptionDefinitionVersion where

instance NFData SubscriptionDefinitionVersion where

instance ToJSON SubscriptionDefinitionVersion where
        toJSON SubscriptionDefinitionVersion'{..}
          = object
              (catMaybes
                 [("Subscriptions" .=) <$> _sdvSubscriptions])

-- | Information about a version.
--
-- /See:/ 'versionInformation' smart constructor.
data VersionInformation = VersionInformation'
  { _viARN               :: !(Maybe Text)
  , _viCreationTimestamp :: !(Maybe Text)
  , _viVersion           :: !(Maybe Text)
  , _viId                :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VersionInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'viARN' - The ARN of the version.
--
-- * 'viCreationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- * 'viVersion' - The unique ID of the version.
--
-- * 'viId' - The ID of the version.
versionInformation
    :: VersionInformation
versionInformation =
  VersionInformation'
    { _viARN = Nothing
    , _viCreationTimestamp = Nothing
    , _viVersion = Nothing
    , _viId = Nothing
    }


-- | The ARN of the version.
viARN :: Lens' VersionInformation (Maybe Text)
viARN = lens _viARN (\ s a -> s{_viARN = a})

-- | The time, in milliseconds since the epoch, when the version was created.
viCreationTimestamp :: Lens' VersionInformation (Maybe Text)
viCreationTimestamp = lens _viCreationTimestamp (\ s a -> s{_viCreationTimestamp = a})

-- | The unique ID of the version.
viVersion :: Lens' VersionInformation (Maybe Text)
viVersion = lens _viVersion (\ s a -> s{_viVersion = a})

-- | The ID of the version.
viId :: Lens' VersionInformation (Maybe Text)
viId = lens _viId (\ s a -> s{_viId = a})

instance FromJSON VersionInformation where
        parseJSON
          = withObject "VersionInformation"
              (\ x ->
                 VersionInformation' <$>
                   (x .:? "Arn") <*> (x .:? "CreationTimestamp") <*>
                     (x .:? "Version")
                     <*> (x .:? "Id"))

instance Hashable VersionInformation where

instance NFData VersionInformation where
