{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Product where

import Network.AWS.DeviceFarm.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A container for account-level settings within AWS Device Farm.
--
--
--
-- /See:/ 'accountSettings' smart constructor.
data AccountSettings = AccountSettings'
  { _asSkipAppResign                :: !(Maybe Bool)
  , _asAwsAccountNumber             :: !(Maybe Text)
  , _asMaxJobTimeoutMinutes         :: !(Maybe Int)
  , _asMaxSlots                     :: !(Maybe (Map Text Int))
  , _asTrialMinutes                 :: !(Maybe TrialMinutes)
  , _asUnmeteredDevices             :: !(Maybe (Map DevicePlatform Int))
  , _asUnmeteredRemoteAccessDevices :: !(Maybe (Map DevicePlatform Int))
  , _asDefaultJobTimeoutMinutes     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asSkipAppResign' - When set to @true@ , for private devices, Device Farm will not sign your app again. For public devices, Device Farm always signs your apps again and this parameter has no effect. For more information about how Device Farm re-signs your app(s), see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- * 'asAwsAccountNumber' - The AWS account number specified in the @AccountSettings@ container.
--
-- * 'asMaxJobTimeoutMinutes' - The maximum number of minutes a test run will execute before it times out.
--
-- * 'asMaxSlots' - The maximum number of device slots that the AWS account can purchase. Each maximum is expressed as an @offering-id:number@ pair, where the @offering-id@ represents one of the IDs returned by the @ListOfferings@ command.
--
-- * 'asTrialMinutes' - Information about an AWS account's usage of free trial device minutes.
--
-- * 'asUnmeteredDevices' - Returns the unmetered devices you have purchased or want to purchase.
--
-- * 'asUnmeteredRemoteAccessDevices' - Returns the unmetered remote access devices you have purchased or want to purchase.
--
-- * 'asDefaultJobTimeoutMinutes' - The default number of minutes (at the account level) a test run will execute before it times out. Default value is 60 minutes.
accountSettings
    :: AccountSettings
accountSettings =
  AccountSettings'
    { _asSkipAppResign = Nothing
    , _asAwsAccountNumber = Nothing
    , _asMaxJobTimeoutMinutes = Nothing
    , _asMaxSlots = Nothing
    , _asTrialMinutes = Nothing
    , _asUnmeteredDevices = Nothing
    , _asUnmeteredRemoteAccessDevices = Nothing
    , _asDefaultJobTimeoutMinutes = Nothing
    }


-- | When set to @true@ , for private devices, Device Farm will not sign your app again. For public devices, Device Farm always signs your apps again and this parameter has no effect. For more information about how Device Farm re-signs your app(s), see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
asSkipAppResign :: Lens' AccountSettings (Maybe Bool)
asSkipAppResign = lens _asSkipAppResign (\ s a -> s{_asSkipAppResign = a})

-- | The AWS account number specified in the @AccountSettings@ container.
asAwsAccountNumber :: Lens' AccountSettings (Maybe Text)
asAwsAccountNumber = lens _asAwsAccountNumber (\ s a -> s{_asAwsAccountNumber = a})

-- | The maximum number of minutes a test run will execute before it times out.
asMaxJobTimeoutMinutes :: Lens' AccountSettings (Maybe Int)
asMaxJobTimeoutMinutes = lens _asMaxJobTimeoutMinutes (\ s a -> s{_asMaxJobTimeoutMinutes = a})

-- | The maximum number of device slots that the AWS account can purchase. Each maximum is expressed as an @offering-id:number@ pair, where the @offering-id@ represents one of the IDs returned by the @ListOfferings@ command.
asMaxSlots :: Lens' AccountSettings (HashMap Text Int)
asMaxSlots = lens _asMaxSlots (\ s a -> s{_asMaxSlots = a}) . _Default . _Map

-- | Information about an AWS account's usage of free trial device minutes.
asTrialMinutes :: Lens' AccountSettings (Maybe TrialMinutes)
asTrialMinutes = lens _asTrialMinutes (\ s a -> s{_asTrialMinutes = a})

-- | Returns the unmetered devices you have purchased or want to purchase.
asUnmeteredDevices :: Lens' AccountSettings (HashMap DevicePlatform Int)
asUnmeteredDevices = lens _asUnmeteredDevices (\ s a -> s{_asUnmeteredDevices = a}) . _Default . _Map

-- | Returns the unmetered remote access devices you have purchased or want to purchase.
asUnmeteredRemoteAccessDevices :: Lens' AccountSettings (HashMap DevicePlatform Int)
asUnmeteredRemoteAccessDevices = lens _asUnmeteredRemoteAccessDevices (\ s a -> s{_asUnmeteredRemoteAccessDevices = a}) . _Default . _Map

-- | The default number of minutes (at the account level) a test run will execute before it times out. Default value is 60 minutes.
asDefaultJobTimeoutMinutes :: Lens' AccountSettings (Maybe Int)
asDefaultJobTimeoutMinutes = lens _asDefaultJobTimeoutMinutes (\ s a -> s{_asDefaultJobTimeoutMinutes = a})

instance FromJSON AccountSettings where
        parseJSON
          = withObject "AccountSettings"
              (\ x ->
                 AccountSettings' <$>
                   (x .:? "skipAppResign") <*>
                     (x .:? "awsAccountNumber")
                     <*> (x .:? "maxJobTimeoutMinutes")
                     <*> (x .:? "maxSlots" .!= mempty)
                     <*> (x .:? "trialMinutes")
                     <*> (x .:? "unmeteredDevices" .!= mempty)
                     <*> (x .:? "unmeteredRemoteAccessDevices" .!= mempty)
                     <*> (x .:? "defaultJobTimeoutMinutes"))

instance Hashable AccountSettings where

instance NFData AccountSettings where

-- | Represents the output of a test. Examples of artifacts include logs and screenshots.
--
--
--
-- /See:/ 'artifact' smart constructor.
data Artifact = Artifact'
  { _aArn       :: !(Maybe Text)
  , _aUrl       :: !(Maybe Text)
  , _aExtension :: !(Maybe Text)
  , _aName      :: !(Maybe Text)
  , _aType      :: !(Maybe ArtifactType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Artifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aArn' - The artifact's ARN.
--
-- * 'aUrl' - The pre-signed Amazon S3 URL that can be used with a corresponding GET request to download the artifact's file.
--
-- * 'aExtension' - The artifact's file extension.
--
-- * 'aName' - The artifact's name.
--
-- * 'aType' - The artifact's type. Allowed values include the following:     * UNKNOWN: An unknown type.     * SCREENSHOT: The screenshot type.     * DEVICE_LOG: The device log type.     * MESSAGE_LOG: The message log type.     * VIDEO_LOG: The video log type.     * RESULT_LOG: The result log type.     * SERVICE_LOG: The service log type.     * WEBKIT_LOG: The web kit log type.     * INSTRUMENTATION_OUTPUT: The instrumentation type.     * EXERCISER_MONKEY_OUTPUT: For Android, the artifact (log) generated by an Android fuzz test.     * CALABASH_JSON_OUTPUT: The Calabash JSON output type.     * CALABASH_PRETTY_OUTPUT: The Calabash pretty output type.     * CALABASH_STANDARD_OUTPUT: The Calabash standard output type.     * CALABASH_JAVA_XML_OUTPUT: The Calabash Java XML output type.     * AUTOMATION_OUTPUT: The automation output type.     * APPIUM_SERVER_OUTPUT: The Appium server output type.     * APPIUM_JAVA_OUTPUT: The Appium Java output type.     * APPIUM_JAVA_XML_OUTPUT: The Appium Java XML output type.     * APPIUM_PYTHON_OUTPUT: The Appium Python output type.     * APPIUM_PYTHON_XML_OUTPUT: The Appium Python XML output type.     * EXPLORER_EVENT_LOG: The Explorer event log output type.     * EXPLORER_SUMMARY_LOG: The Explorer summary log output type.     * APPLICATION_CRASH_REPORT: The application crash report output type.     * XCTEST_LOG: The XCode test output type.     * VIDEO: The Video output type.     * CUSTOMER_ARTIFACT:The Customer Artifact output type.     * CUSTOMER_ARTIFACT_LOG: The Customer Artifact Log output type.     * TESTSPEC_OUTPUT: The Test Spec Output type.
artifact
    :: Artifact
artifact =
  Artifact'
    { _aArn = Nothing
    , _aUrl = Nothing
    , _aExtension = Nothing
    , _aName = Nothing
    , _aType = Nothing
    }


-- | The artifact's ARN.
aArn :: Lens' Artifact (Maybe Text)
aArn = lens _aArn (\ s a -> s{_aArn = a})

-- | The pre-signed Amazon S3 URL that can be used with a corresponding GET request to download the artifact's file.
aUrl :: Lens' Artifact (Maybe Text)
aUrl = lens _aUrl (\ s a -> s{_aUrl = a})

-- | The artifact's file extension.
aExtension :: Lens' Artifact (Maybe Text)
aExtension = lens _aExtension (\ s a -> s{_aExtension = a})

-- | The artifact's name.
aName :: Lens' Artifact (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a})

-- | The artifact's type. Allowed values include the following:     * UNKNOWN: An unknown type.     * SCREENSHOT: The screenshot type.     * DEVICE_LOG: The device log type.     * MESSAGE_LOG: The message log type.     * VIDEO_LOG: The video log type.     * RESULT_LOG: The result log type.     * SERVICE_LOG: The service log type.     * WEBKIT_LOG: The web kit log type.     * INSTRUMENTATION_OUTPUT: The instrumentation type.     * EXERCISER_MONKEY_OUTPUT: For Android, the artifact (log) generated by an Android fuzz test.     * CALABASH_JSON_OUTPUT: The Calabash JSON output type.     * CALABASH_PRETTY_OUTPUT: The Calabash pretty output type.     * CALABASH_STANDARD_OUTPUT: The Calabash standard output type.     * CALABASH_JAVA_XML_OUTPUT: The Calabash Java XML output type.     * AUTOMATION_OUTPUT: The automation output type.     * APPIUM_SERVER_OUTPUT: The Appium server output type.     * APPIUM_JAVA_OUTPUT: The Appium Java output type.     * APPIUM_JAVA_XML_OUTPUT: The Appium Java XML output type.     * APPIUM_PYTHON_OUTPUT: The Appium Python output type.     * APPIUM_PYTHON_XML_OUTPUT: The Appium Python XML output type.     * EXPLORER_EVENT_LOG: The Explorer event log output type.     * EXPLORER_SUMMARY_LOG: The Explorer summary log output type.     * APPLICATION_CRASH_REPORT: The application crash report output type.     * XCTEST_LOG: The XCode test output type.     * VIDEO: The Video output type.     * CUSTOMER_ARTIFACT:The Customer Artifact output type.     * CUSTOMER_ARTIFACT_LOG: The Customer Artifact Log output type.     * TESTSPEC_OUTPUT: The Test Spec Output type.
aType :: Lens' Artifact (Maybe ArtifactType)
aType = lens _aType (\ s a -> s{_aType = a})

instance FromJSON Artifact where
        parseJSON
          = withObject "Artifact"
              (\ x ->
                 Artifact' <$>
                   (x .:? "arn") <*> (x .:? "url") <*>
                     (x .:? "extension")
                     <*> (x .:? "name")
                     <*> (x .:? "type"))

instance Hashable Artifact where

instance NFData Artifact where

-- | Represents the amount of CPU that an app is using on a physical device.
--
--
-- Note that this does not represent system-wide CPU usage.
--
--
-- /See:/ 'cpu' smart constructor.
data CPU = CPU'
  { _cpuFrequency    :: !(Maybe Text)
  , _cpuClock        :: !(Maybe Double)
  , _cpuArchitecture :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CPU' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpuFrequency' - The CPU's frequency.
--
-- * 'cpuClock' - The clock speed of the device's CPU, expressed in hertz (Hz). For example, a 1.2 GHz CPU is expressed as 1200000000.
--
-- * 'cpuArchitecture' - The CPU's architecture, for example x86 or ARM.
cpu
    :: CPU
cpu =
  CPU'
    {_cpuFrequency = Nothing, _cpuClock = Nothing, _cpuArchitecture = Nothing}


-- | The CPU's frequency.
cpuFrequency :: Lens' CPU (Maybe Text)
cpuFrequency = lens _cpuFrequency (\ s a -> s{_cpuFrequency = a})

-- | The clock speed of the device's CPU, expressed in hertz (Hz). For example, a 1.2 GHz CPU is expressed as 1200000000.
cpuClock :: Lens' CPU (Maybe Double)
cpuClock = lens _cpuClock (\ s a -> s{_cpuClock = a})

-- | The CPU's architecture, for example x86 or ARM.
cpuArchitecture :: Lens' CPU (Maybe Text)
cpuArchitecture = lens _cpuArchitecture (\ s a -> s{_cpuArchitecture = a})

instance FromJSON CPU where
        parseJSON
          = withObject "CPU"
              (\ x ->
                 CPU' <$>
                   (x .:? "frequency") <*> (x .:? "clock") <*>
                     (x .:? "architecture"))

instance Hashable CPU where

instance NFData CPU where

-- | Represents entity counters.
--
--
--
-- /See:/ 'counters' smart constructor.
data Counters = Counters'
  { _cPassed  :: !(Maybe Int)
  , _cSkipped :: !(Maybe Int)
  , _cWarned  :: !(Maybe Int)
  , _cStopped :: !(Maybe Int)
  , _cTotal   :: !(Maybe Int)
  , _cFailed  :: !(Maybe Int)
  , _cErrored :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Counters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cPassed' - The number of passed entities.
--
-- * 'cSkipped' - The number of skipped entities.
--
-- * 'cWarned' - The number of warned entities.
--
-- * 'cStopped' - The number of stopped entities.
--
-- * 'cTotal' - The total number of entities.
--
-- * 'cFailed' - The number of failed entities.
--
-- * 'cErrored' - The number of errored entities.
counters
    :: Counters
counters =
  Counters'
    { _cPassed = Nothing
    , _cSkipped = Nothing
    , _cWarned = Nothing
    , _cStopped = Nothing
    , _cTotal = Nothing
    , _cFailed = Nothing
    , _cErrored = Nothing
    }


-- | The number of passed entities.
cPassed :: Lens' Counters (Maybe Int)
cPassed = lens _cPassed (\ s a -> s{_cPassed = a})

-- | The number of skipped entities.
cSkipped :: Lens' Counters (Maybe Int)
cSkipped = lens _cSkipped (\ s a -> s{_cSkipped = a})

-- | The number of warned entities.
cWarned :: Lens' Counters (Maybe Int)
cWarned = lens _cWarned (\ s a -> s{_cWarned = a})

-- | The number of stopped entities.
cStopped :: Lens' Counters (Maybe Int)
cStopped = lens _cStopped (\ s a -> s{_cStopped = a})

-- | The total number of entities.
cTotal :: Lens' Counters (Maybe Int)
cTotal = lens _cTotal (\ s a -> s{_cTotal = a})

-- | The number of failed entities.
cFailed :: Lens' Counters (Maybe Int)
cFailed = lens _cFailed (\ s a -> s{_cFailed = a})

-- | The number of errored entities.
cErrored :: Lens' Counters (Maybe Int)
cErrored = lens _cErrored (\ s a -> s{_cErrored = a})

instance FromJSON Counters where
        parseJSON
          = withObject "Counters"
              (\ x ->
                 Counters' <$>
                   (x .:? "passed") <*> (x .:? "skipped") <*>
                     (x .:? "warned")
                     <*> (x .:? "stopped")
                     <*> (x .:? "total")
                     <*> (x .:? "failed")
                     <*> (x .:? "errored"))

instance Hashable Counters where

instance NFData Counters where

-- | Configuration settings for a remote access session, including billing method.
--
--
--
-- /See:/ 'createRemoteAccessSessionConfiguration' smart constructor.
data CreateRemoteAccessSessionConfiguration = CreateRemoteAccessSessionConfiguration'
  { _crascBillingMethod         :: !(Maybe BillingMethod)
  , _crascVpceConfigurationARNs :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRemoteAccessSessionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crascBillingMethod' - The billing method for the remote access session.
--
-- * 'crascVpceConfigurationARNs' - An array of Amazon Resource Names (ARNs) included in the VPC endpoint configuration.
createRemoteAccessSessionConfiguration
    :: CreateRemoteAccessSessionConfiguration
createRemoteAccessSessionConfiguration =
  CreateRemoteAccessSessionConfiguration'
    {_crascBillingMethod = Nothing, _crascVpceConfigurationARNs = Nothing}


-- | The billing method for the remote access session.
crascBillingMethod :: Lens' CreateRemoteAccessSessionConfiguration (Maybe BillingMethod)
crascBillingMethod = lens _crascBillingMethod (\ s a -> s{_crascBillingMethod = a})

-- | An array of Amazon Resource Names (ARNs) included in the VPC endpoint configuration.
crascVpceConfigurationARNs :: Lens' CreateRemoteAccessSessionConfiguration [Text]
crascVpceConfigurationARNs = lens _crascVpceConfigurationARNs (\ s a -> s{_crascVpceConfigurationARNs = a}) . _Default . _Coerce

instance Hashable
           CreateRemoteAccessSessionConfiguration
         where

instance NFData
           CreateRemoteAccessSessionConfiguration
         where

instance ToJSON
           CreateRemoteAccessSessionConfiguration
         where
        toJSON CreateRemoteAccessSessionConfiguration'{..}
          = object
              (catMaybes
                 [("billingMethod" .=) <$> _crascBillingMethod,
                  ("vpceConfigurationArns" .=) <$>
                    _crascVpceConfigurationARNs])

-- | A JSON object specifying the paths where the artifacts generated by the customer's tests, on the device or in the test environment, will be pulled from.
--
--
-- Specify @deviceHostPaths@ and optionally specify either @iosPaths@ or @androidPaths@ .
--
-- For web app tests, you can specify both @iosPaths@ and @androidPaths@ .
--
--
-- /See:/ 'customerArtifactPaths' smart constructor.
data CustomerArtifactPaths = CustomerArtifactPaths'
  { _capAndroidPaths    :: !(Maybe [Text])
  , _capDeviceHostPaths :: !(Maybe [Text])
  , _capIosPaths        :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomerArtifactPaths' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'capAndroidPaths' - Comma-separated list of paths on the Android device where the artifacts generated by the customer's tests will be pulled from.
--
-- * 'capDeviceHostPaths' - Comma-separated list of paths in the test execution environment where the artifacts generated by the customer's tests will be pulled from.
--
-- * 'capIosPaths' - Comma-separated list of paths on the iOS device where the artifacts generated by the customer's tests will be pulled from.
customerArtifactPaths
    :: CustomerArtifactPaths
customerArtifactPaths =
  CustomerArtifactPaths'
    { _capAndroidPaths = Nothing
    , _capDeviceHostPaths = Nothing
    , _capIosPaths = Nothing
    }


-- | Comma-separated list of paths on the Android device where the artifacts generated by the customer's tests will be pulled from.
capAndroidPaths :: Lens' CustomerArtifactPaths [Text]
capAndroidPaths = lens _capAndroidPaths (\ s a -> s{_capAndroidPaths = a}) . _Default . _Coerce

-- | Comma-separated list of paths in the test execution environment where the artifacts generated by the customer's tests will be pulled from.
capDeviceHostPaths :: Lens' CustomerArtifactPaths [Text]
capDeviceHostPaths = lens _capDeviceHostPaths (\ s a -> s{_capDeviceHostPaths = a}) . _Default . _Coerce

-- | Comma-separated list of paths on the iOS device where the artifacts generated by the customer's tests will be pulled from.
capIosPaths :: Lens' CustomerArtifactPaths [Text]
capIosPaths = lens _capIosPaths (\ s a -> s{_capIosPaths = a}) . _Default . _Coerce

instance FromJSON CustomerArtifactPaths where
        parseJSON
          = withObject "CustomerArtifactPaths"
              (\ x ->
                 CustomerArtifactPaths' <$>
                   (x .:? "androidPaths" .!= mempty) <*>
                     (x .:? "deviceHostPaths" .!= mempty)
                     <*> (x .:? "iosPaths" .!= mempty))

instance Hashable CustomerArtifactPaths where

instance NFData CustomerArtifactPaths where

instance ToJSON CustomerArtifactPaths where
        toJSON CustomerArtifactPaths'{..}
          = object
              (catMaybes
                 [("androidPaths" .=) <$> _capAndroidPaths,
                  ("deviceHostPaths" .=) <$> _capDeviceHostPaths,
                  ("iosPaths" .=) <$> _capIosPaths])

-- | Represents a device type that an app is tested against.
--
--
--
-- /See:/ 'device' smart constructor.
data Device = Device'
  { _dCarrier             :: !(Maybe Text)
  , _dImage               :: !(Maybe Text)
  , _dManufacturer        :: !(Maybe Text)
  , _dPlatform            :: !(Maybe DevicePlatform)
  , _dModelId             :: !(Maybe Text)
  , _dRemoteAccessEnabled :: !(Maybe Bool)
  , _dArn                 :: !(Maybe Text)
  , _dFormFactor          :: !(Maybe DeviceFormFactor)
  , _dFleetType           :: !(Maybe Text)
  , _dResolution          :: !(Maybe Resolution)
  , _dAvailability        :: !(Maybe DeviceAvailability)
  , _dMemory              :: !(Maybe Integer)
  , _dRadio               :: !(Maybe Text)
  , _dOs                  :: !(Maybe Text)
  , _dName                :: !(Maybe Text)
  , _dModel               :: !(Maybe Text)
  , _dInstances           :: !(Maybe [DeviceInstance])
  , _dRemoteDebugEnabled  :: !(Maybe Bool)
  , _dCpu                 :: !(Maybe CPU)
  , _dHeapSize            :: !(Maybe Integer)
  , _dFleetName           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCarrier' - The device's carrier.
--
-- * 'dImage' - The device's image name.
--
-- * 'dManufacturer' - The device's manufacturer name.
--
-- * 'dPlatform' - The device's platform. Allowed values include:     * ANDROID: The Android platform.     * IOS: The iOS platform.
--
-- * 'dModelId' - The device's model ID.
--
-- * 'dRemoteAccessEnabled' - Specifies whether remote access has been enabled for the specified device.
--
-- * 'dArn' - The device's ARN.
--
-- * 'dFormFactor' - The device's form factor. Allowed values include:     * PHONE: The phone form factor.     * TABLET: The tablet form factor.
--
-- * 'dFleetType' - The type of fleet to which this device belongs. Possible values for fleet type are PRIVATE and PUBLIC.
--
-- * 'dResolution' - The resolution of the device.
--
-- * 'dAvailability' - Reflects how likely a device will be available for a test run. It is currently available in the ListDevices and GetDevice API methods.
--
-- * 'dMemory' - The device's total memory size, expressed in bytes.
--
-- * 'dRadio' - The device's radio.
--
-- * 'dOs' - The device's operating system type.
--
-- * 'dName' - The device's display name.
--
-- * 'dModel' - The device's model name.
--
-- * 'dInstances' - The instances belonging to this device.
--
-- * 'dRemoteDebugEnabled' - This flag is set to @true@ if remote debugging is enabled for the device.
--
-- * 'dCpu' - Information about the device's CPU.
--
-- * 'dHeapSize' - The device's heap size, expressed in bytes.
--
-- * 'dFleetName' - The name of the fleet to which this device belongs.
device
    :: Device
device =
  Device'
    { _dCarrier = Nothing
    , _dImage = Nothing
    , _dManufacturer = Nothing
    , _dPlatform = Nothing
    , _dModelId = Nothing
    , _dRemoteAccessEnabled = Nothing
    , _dArn = Nothing
    , _dFormFactor = Nothing
    , _dFleetType = Nothing
    , _dResolution = Nothing
    , _dAvailability = Nothing
    , _dMemory = Nothing
    , _dRadio = Nothing
    , _dOs = Nothing
    , _dName = Nothing
    , _dModel = Nothing
    , _dInstances = Nothing
    , _dRemoteDebugEnabled = Nothing
    , _dCpu = Nothing
    , _dHeapSize = Nothing
    , _dFleetName = Nothing
    }


-- | The device's carrier.
dCarrier :: Lens' Device (Maybe Text)
dCarrier = lens _dCarrier (\ s a -> s{_dCarrier = a})

-- | The device's image name.
dImage :: Lens' Device (Maybe Text)
dImage = lens _dImage (\ s a -> s{_dImage = a})

-- | The device's manufacturer name.
dManufacturer :: Lens' Device (Maybe Text)
dManufacturer = lens _dManufacturer (\ s a -> s{_dManufacturer = a})

-- | The device's platform. Allowed values include:     * ANDROID: The Android platform.     * IOS: The iOS platform.
dPlatform :: Lens' Device (Maybe DevicePlatform)
dPlatform = lens _dPlatform (\ s a -> s{_dPlatform = a})

-- | The device's model ID.
dModelId :: Lens' Device (Maybe Text)
dModelId = lens _dModelId (\ s a -> s{_dModelId = a})

-- | Specifies whether remote access has been enabled for the specified device.
dRemoteAccessEnabled :: Lens' Device (Maybe Bool)
dRemoteAccessEnabled = lens _dRemoteAccessEnabled (\ s a -> s{_dRemoteAccessEnabled = a})

-- | The device's ARN.
dArn :: Lens' Device (Maybe Text)
dArn = lens _dArn (\ s a -> s{_dArn = a})

-- | The device's form factor. Allowed values include:     * PHONE: The phone form factor.     * TABLET: The tablet form factor.
dFormFactor :: Lens' Device (Maybe DeviceFormFactor)
dFormFactor = lens _dFormFactor (\ s a -> s{_dFormFactor = a})

-- | The type of fleet to which this device belongs. Possible values for fleet type are PRIVATE and PUBLIC.
dFleetType :: Lens' Device (Maybe Text)
dFleetType = lens _dFleetType (\ s a -> s{_dFleetType = a})

-- | The resolution of the device.
dResolution :: Lens' Device (Maybe Resolution)
dResolution = lens _dResolution (\ s a -> s{_dResolution = a})

-- | Reflects how likely a device will be available for a test run. It is currently available in the ListDevices and GetDevice API methods.
dAvailability :: Lens' Device (Maybe DeviceAvailability)
dAvailability = lens _dAvailability (\ s a -> s{_dAvailability = a})

-- | The device's total memory size, expressed in bytes.
dMemory :: Lens' Device (Maybe Integer)
dMemory = lens _dMemory (\ s a -> s{_dMemory = a})

-- | The device's radio.
dRadio :: Lens' Device (Maybe Text)
dRadio = lens _dRadio (\ s a -> s{_dRadio = a})

-- | The device's operating system type.
dOs :: Lens' Device (Maybe Text)
dOs = lens _dOs (\ s a -> s{_dOs = a})

-- | The device's display name.
dName :: Lens' Device (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a})

-- | The device's model name.
dModel :: Lens' Device (Maybe Text)
dModel = lens _dModel (\ s a -> s{_dModel = a})

-- | The instances belonging to this device.
dInstances :: Lens' Device [DeviceInstance]
dInstances = lens _dInstances (\ s a -> s{_dInstances = a}) . _Default . _Coerce

-- | This flag is set to @true@ if remote debugging is enabled for the device.
dRemoteDebugEnabled :: Lens' Device (Maybe Bool)
dRemoteDebugEnabled = lens _dRemoteDebugEnabled (\ s a -> s{_dRemoteDebugEnabled = a})

-- | Information about the device's CPU.
dCpu :: Lens' Device (Maybe CPU)
dCpu = lens _dCpu (\ s a -> s{_dCpu = a})

-- | The device's heap size, expressed in bytes.
dHeapSize :: Lens' Device (Maybe Integer)
dHeapSize = lens _dHeapSize (\ s a -> s{_dHeapSize = a})

-- | The name of the fleet to which this device belongs.
dFleetName :: Lens' Device (Maybe Text)
dFleetName = lens _dFleetName (\ s a -> s{_dFleetName = a})

instance FromJSON Device where
        parseJSON
          = withObject "Device"
              (\ x ->
                 Device' <$>
                   (x .:? "carrier") <*> (x .:? "image") <*>
                     (x .:? "manufacturer")
                     <*> (x .:? "platform")
                     <*> (x .:? "modelId")
                     <*> (x .:? "remoteAccessEnabled")
                     <*> (x .:? "arn")
                     <*> (x .:? "formFactor")
                     <*> (x .:? "fleetType")
                     <*> (x .:? "resolution")
                     <*> (x .:? "availability")
                     <*> (x .:? "memory")
                     <*> (x .:? "radio")
                     <*> (x .:? "os")
                     <*> (x .:? "name")
                     <*> (x .:? "model")
                     <*> (x .:? "instances" .!= mempty)
                     <*> (x .:? "remoteDebugEnabled")
                     <*> (x .:? "cpu")
                     <*> (x .:? "heapSize")
                     <*> (x .:? "fleetName"))

instance Hashable Device where

instance NFData Device where

-- | Represents a device filter used to select a set of devices to be included in a test run. This data structure is passed in as the @deviceSelectionConfiguration@ parameter to ScheduleRun. For an example of the JSON request syntax, see 'ScheduleRun' .
--
--
-- It is also passed in as the @filters@ parameter to ListDevices. For an example of the JSON request syntax, see 'ListDevices' .
--
--
-- /See:/ 'deviceFilter' smart constructor.
data DeviceFilter = DeviceFilter'
  { _dfAttribute :: !(Maybe DeviceFilterAttribute)
  , _dfOperator  :: !(Maybe RuleOperator)
  , _dfValues    :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfAttribute' - The aspect of a device such as platform or model used as the selection criteria in a device filter. The supported operators for each attribute are provided in the following list.     * ARN    * The Amazon Resource Name (ARN) of the device. For example, "arn:aws:devicefarm:us-west-2::device:12345Example". /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * PLATFORM    * The device platform. Valid values are "ANDROID" or "IOS". /Supported operators/ : @EQUALS@      * OS_VERSION    * The operating system version. For example, "10.3.2". /Supported operators/ : @EQUALS@ , @GREATER_THAN@ , @GREATER_THAN_OR_EQUALS@ , @IN@ , @LESS_THAN@ , @LESS_THAN_OR_EQUALS@ , @NOT_IN@      * MODEL    * The device model. For example, "iPad 5th Gen". /Supported operators/ : @CONTAINS@ , @EQUALS@ , @IN@ , @NOT_IN@      * AVAILABILITY    * The current availability of the device. Valid values are "AVAILABLE", "HIGHLY_AVAILABLE", "BUSY", or "TEMPORARY_NOT_AVAILABLE". /Supported operators/ : @EQUALS@      * FORM_FACTOR    * The device form factor. Valid values are "PHONE" or "TABLET". /Supported operators/ : @EQUALS@      * MANUFACTURER    * The device manufacturer. For example, "Apple". /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * REMOTE_ACCESS_ENABLED    * Whether the device is enabled for remote access. Valid values are "TRUE" or "FALSE". /Supported operators/ : @EQUALS@      * REMOTE_DEBUG_ENABLED    * Whether the device is enabled for remote debugging. Valid values are "TRUE" or "FALSE". /Supported operators/ : @EQUALS@      * INSTANCE_ARN    * The Amazon Resource Name (ARN) of the device instance. /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * INSTANCE_LABELS    * The label of the device instance. /Supported operators/ : @CONTAINS@      * FLEET_TYPE    * The fleet type. Valid values are "PUBLIC" or "PRIVATE". /Supported operators/ : @EQUALS@
--
-- * 'dfOperator' - Specifies how Device Farm compares the filter's attribute to the value. For the operators that are supported by each attribute, see the attribute descriptions.
--
-- * 'dfValues' - An array of one or more filter values used in a device filter. __Operator Values__      * The IN and NOT_IN operators can take a values array that has more than one element.     * The other operators require an array with a single element. __Attribute Values__      * The PLATFORM attribute can be set to "ANDROID" or "IOS".     * The AVAILABILITY attribute can be set to "AVAILABLE", "HIGHLY_AVAILABLE", "BUSY", or "TEMPORARY_NOT_AVAILABLE".     * The FORM_FACTOR attribute can be set to "PHONE" or "TABLET".     * The FLEET_TYPE attribute can be set to "PUBLIC" or "PRIVATE".
deviceFilter
    :: DeviceFilter
deviceFilter =
  DeviceFilter'
    {_dfAttribute = Nothing, _dfOperator = Nothing, _dfValues = Nothing}


-- | The aspect of a device such as platform or model used as the selection criteria in a device filter. The supported operators for each attribute are provided in the following list.     * ARN    * The Amazon Resource Name (ARN) of the device. For example, "arn:aws:devicefarm:us-west-2::device:12345Example". /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * PLATFORM    * The device platform. Valid values are "ANDROID" or "IOS". /Supported operators/ : @EQUALS@      * OS_VERSION    * The operating system version. For example, "10.3.2". /Supported operators/ : @EQUALS@ , @GREATER_THAN@ , @GREATER_THAN_OR_EQUALS@ , @IN@ , @LESS_THAN@ , @LESS_THAN_OR_EQUALS@ , @NOT_IN@      * MODEL    * The device model. For example, "iPad 5th Gen". /Supported operators/ : @CONTAINS@ , @EQUALS@ , @IN@ , @NOT_IN@      * AVAILABILITY    * The current availability of the device. Valid values are "AVAILABLE", "HIGHLY_AVAILABLE", "BUSY", or "TEMPORARY_NOT_AVAILABLE". /Supported operators/ : @EQUALS@      * FORM_FACTOR    * The device form factor. Valid values are "PHONE" or "TABLET". /Supported operators/ : @EQUALS@      * MANUFACTURER    * The device manufacturer. For example, "Apple". /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * REMOTE_ACCESS_ENABLED    * Whether the device is enabled for remote access. Valid values are "TRUE" or "FALSE". /Supported operators/ : @EQUALS@      * REMOTE_DEBUG_ENABLED    * Whether the device is enabled for remote debugging. Valid values are "TRUE" or "FALSE". /Supported operators/ : @EQUALS@      * INSTANCE_ARN    * The Amazon Resource Name (ARN) of the device instance. /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * INSTANCE_LABELS    * The label of the device instance. /Supported operators/ : @CONTAINS@      * FLEET_TYPE    * The fleet type. Valid values are "PUBLIC" or "PRIVATE". /Supported operators/ : @EQUALS@
dfAttribute :: Lens' DeviceFilter (Maybe DeviceFilterAttribute)
dfAttribute = lens _dfAttribute (\ s a -> s{_dfAttribute = a})

-- | Specifies how Device Farm compares the filter's attribute to the value. For the operators that are supported by each attribute, see the attribute descriptions.
dfOperator :: Lens' DeviceFilter (Maybe RuleOperator)
dfOperator = lens _dfOperator (\ s a -> s{_dfOperator = a})

-- | An array of one or more filter values used in a device filter. __Operator Values__      * The IN and NOT_IN operators can take a values array that has more than one element.     * The other operators require an array with a single element. __Attribute Values__      * The PLATFORM attribute can be set to "ANDROID" or "IOS".     * The AVAILABILITY attribute can be set to "AVAILABLE", "HIGHLY_AVAILABLE", "BUSY", or "TEMPORARY_NOT_AVAILABLE".     * The FORM_FACTOR attribute can be set to "PHONE" or "TABLET".     * The FLEET_TYPE attribute can be set to "PUBLIC" or "PRIVATE".
dfValues :: Lens' DeviceFilter [Text]
dfValues = lens _dfValues (\ s a -> s{_dfValues = a}) . _Default . _Coerce

instance FromJSON DeviceFilter where
        parseJSON
          = withObject "DeviceFilter"
              (\ x ->
                 DeviceFilter' <$>
                   (x .:? "attribute") <*> (x .:? "operator") <*>
                     (x .:? "values" .!= mempty))

instance Hashable DeviceFilter where

instance NFData DeviceFilter where

instance ToJSON DeviceFilter where
        toJSON DeviceFilter'{..}
          = object
              (catMaybes
                 [("attribute" .=) <$> _dfAttribute,
                  ("operator" .=) <$> _dfOperator,
                  ("values" .=) <$> _dfValues])

-- | Represents the device instance.
--
--
--
-- /See:/ 'deviceInstance' smart constructor.
data DeviceInstance = DeviceInstance'
  { _diStatus          :: !(Maybe InstanceStatus)
  , _diUdid            :: !(Maybe Text)
  , _diInstanceProfile :: !(Maybe InstanceProfile)
  , _diArn             :: !(Maybe Text)
  , _diDeviceARN       :: !(Maybe Text)
  , _diLabels          :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diStatus' - The status of the device instance. Valid values are listed below.
--
-- * 'diUdid' - Unique device identifier for the device instance.
--
-- * 'diInstanceProfile' - A object containing information about the instance profile.
--
-- * 'diArn' - The Amazon Resource Name (ARN) of the device instance.
--
-- * 'diDeviceARN' - The Amazon Resource Name (ARN) of the device.
--
-- * 'diLabels' - An array of strings describing the device instance.
deviceInstance
    :: DeviceInstance
deviceInstance =
  DeviceInstance'
    { _diStatus = Nothing
    , _diUdid = Nothing
    , _diInstanceProfile = Nothing
    , _diArn = Nothing
    , _diDeviceARN = Nothing
    , _diLabels = Nothing
    }


-- | The status of the device instance. Valid values are listed below.
diStatus :: Lens' DeviceInstance (Maybe InstanceStatus)
diStatus = lens _diStatus (\ s a -> s{_diStatus = a})

-- | Unique device identifier for the device instance.
diUdid :: Lens' DeviceInstance (Maybe Text)
diUdid = lens _diUdid (\ s a -> s{_diUdid = a})

-- | A object containing information about the instance profile.
diInstanceProfile :: Lens' DeviceInstance (Maybe InstanceProfile)
diInstanceProfile = lens _diInstanceProfile (\ s a -> s{_diInstanceProfile = a})

-- | The Amazon Resource Name (ARN) of the device instance.
diArn :: Lens' DeviceInstance (Maybe Text)
diArn = lens _diArn (\ s a -> s{_diArn = a})

-- | The Amazon Resource Name (ARN) of the device.
diDeviceARN :: Lens' DeviceInstance (Maybe Text)
diDeviceARN = lens _diDeviceARN (\ s a -> s{_diDeviceARN = a})

-- | An array of strings describing the device instance.
diLabels :: Lens' DeviceInstance [Text]
diLabels = lens _diLabels (\ s a -> s{_diLabels = a}) . _Default . _Coerce

instance FromJSON DeviceInstance where
        parseJSON
          = withObject "DeviceInstance"
              (\ x ->
                 DeviceInstance' <$>
                   (x .:? "status") <*> (x .:? "udid") <*>
                     (x .:? "instanceProfile")
                     <*> (x .:? "arn")
                     <*> (x .:? "deviceArn")
                     <*> (x .:? "labels" .!= mempty))

instance Hashable DeviceInstance where

instance NFData DeviceInstance where

-- | Represents the total (metered or unmetered) minutes used by the resource to run tests. Contains the sum of minutes consumed by all children.
--
--
--
-- /See:/ 'deviceMinutes' smart constructor.
data DeviceMinutes = DeviceMinutes'
  { _dmMetered   :: !(Maybe Double)
  , _dmTotal     :: !(Maybe Double)
  , _dmUnmetered :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceMinutes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmMetered' - When specified, represents only the sum of metered minutes used by the resource to run tests.
--
-- * 'dmTotal' - When specified, represents the total minutes used by the resource to run tests.
--
-- * 'dmUnmetered' - When specified, represents only the sum of unmetered minutes used by the resource to run tests.
deviceMinutes
    :: DeviceMinutes
deviceMinutes =
  DeviceMinutes'
    {_dmMetered = Nothing, _dmTotal = Nothing, _dmUnmetered = Nothing}


-- | When specified, represents only the sum of metered minutes used by the resource to run tests.
dmMetered :: Lens' DeviceMinutes (Maybe Double)
dmMetered = lens _dmMetered (\ s a -> s{_dmMetered = a})

-- | When specified, represents the total minutes used by the resource to run tests.
dmTotal :: Lens' DeviceMinutes (Maybe Double)
dmTotal = lens _dmTotal (\ s a -> s{_dmTotal = a})

-- | When specified, represents only the sum of unmetered minutes used by the resource to run tests.
dmUnmetered :: Lens' DeviceMinutes (Maybe Double)
dmUnmetered = lens _dmUnmetered (\ s a -> s{_dmUnmetered = a})

instance FromJSON DeviceMinutes where
        parseJSON
          = withObject "DeviceMinutes"
              (\ x ->
                 DeviceMinutes' <$>
                   (x .:? "metered") <*> (x .:? "total") <*>
                     (x .:? "unmetered"))

instance Hashable DeviceMinutes where

instance NFData DeviceMinutes where

-- | Represents a collection of device types.
--
--
--
-- /See:/ 'devicePool' smart constructor.
data DevicePool = DevicePool'
  { _devArn         :: !(Maybe Text)
  , _devRules       :: !(Maybe [Rule])
  , _devName        :: !(Maybe Text)
  , _devMaxDevices  :: !(Maybe Int)
  , _devType        :: !(Maybe DevicePoolType)
  , _devDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DevicePool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'devArn' - The device pool's ARN.
--
-- * 'devRules' - Information about the device pool's rules.
--
-- * 'devName' - The device pool's name.
--
-- * 'devMaxDevices' - The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and that meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter. By specifying the maximum number of devices, you can control the costs that you incur by running tests.
--
-- * 'devType' - The device pool's type. Allowed values include:     * CURATED: A device pool that is created and managed by AWS Device Farm.     * PRIVATE: A device pool that is created and managed by the device pool developer.
--
-- * 'devDescription' - The device pool's description.
devicePool
    :: DevicePool
devicePool =
  DevicePool'
    { _devArn = Nothing
    , _devRules = Nothing
    , _devName = Nothing
    , _devMaxDevices = Nothing
    , _devType = Nothing
    , _devDescription = Nothing
    }


-- | The device pool's ARN.
devArn :: Lens' DevicePool (Maybe Text)
devArn = lens _devArn (\ s a -> s{_devArn = a})

-- | Information about the device pool's rules.
devRules :: Lens' DevicePool [Rule]
devRules = lens _devRules (\ s a -> s{_devRules = a}) . _Default . _Coerce

-- | The device pool's name.
devName :: Lens' DevicePool (Maybe Text)
devName = lens _devName (\ s a -> s{_devName = a})

-- | The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and that meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter. By specifying the maximum number of devices, you can control the costs that you incur by running tests.
devMaxDevices :: Lens' DevicePool (Maybe Int)
devMaxDevices = lens _devMaxDevices (\ s a -> s{_devMaxDevices = a})

-- | The device pool's type. Allowed values include:     * CURATED: A device pool that is created and managed by AWS Device Farm.     * PRIVATE: A device pool that is created and managed by the device pool developer.
devType :: Lens' DevicePool (Maybe DevicePoolType)
devType = lens _devType (\ s a -> s{_devType = a})

-- | The device pool's description.
devDescription :: Lens' DevicePool (Maybe Text)
devDescription = lens _devDescription (\ s a -> s{_devDescription = a})

instance FromJSON DevicePool where
        parseJSON
          = withObject "DevicePool"
              (\ x ->
                 DevicePool' <$>
                   (x .:? "arn") <*> (x .:? "rules" .!= mempty) <*>
                     (x .:? "name")
                     <*> (x .:? "maxDevices")
                     <*> (x .:? "type")
                     <*> (x .:? "description"))

instance Hashable DevicePool where

instance NFData DevicePool where

-- | Represents a device pool compatibility result.
--
--
--
-- /See:/ 'devicePoolCompatibilityResult' smart constructor.
data DevicePoolCompatibilityResult = DevicePoolCompatibilityResult'
  { _dpcrDevice                  :: !(Maybe Device)
  , _dpcrCompatible              :: !(Maybe Bool)
  , _dpcrIncompatibilityMessages :: !(Maybe [IncompatibilityMessage])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DevicePoolCompatibilityResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpcrDevice' - The device (phone or tablet) that you wish to return information about.
--
-- * 'dpcrCompatible' - Whether the result was compatible with the device pool.
--
-- * 'dpcrIncompatibilityMessages' - Information about the compatibility.
devicePoolCompatibilityResult
    :: DevicePoolCompatibilityResult
devicePoolCompatibilityResult =
  DevicePoolCompatibilityResult'
    { _dpcrDevice = Nothing
    , _dpcrCompatible = Nothing
    , _dpcrIncompatibilityMessages = Nothing
    }


-- | The device (phone or tablet) that you wish to return information about.
dpcrDevice :: Lens' DevicePoolCompatibilityResult (Maybe Device)
dpcrDevice = lens _dpcrDevice (\ s a -> s{_dpcrDevice = a})

-- | Whether the result was compatible with the device pool.
dpcrCompatible :: Lens' DevicePoolCompatibilityResult (Maybe Bool)
dpcrCompatible = lens _dpcrCompatible (\ s a -> s{_dpcrCompatible = a})

-- | Information about the compatibility.
dpcrIncompatibilityMessages :: Lens' DevicePoolCompatibilityResult [IncompatibilityMessage]
dpcrIncompatibilityMessages = lens _dpcrIncompatibilityMessages (\ s a -> s{_dpcrIncompatibilityMessages = a}) . _Default . _Coerce

instance FromJSON DevicePoolCompatibilityResult where
        parseJSON
          = withObject "DevicePoolCompatibilityResult"
              (\ x ->
                 DevicePoolCompatibilityResult' <$>
                   (x .:? "device") <*> (x .:? "compatible") <*>
                     (x .:? "incompatibilityMessages" .!= mempty))

instance Hashable DevicePoolCompatibilityResult where

instance NFData DevicePoolCompatibilityResult where

-- | Represents the device filters used in a test run as well as the maximum number of devices to be included in the run. It is passed in as the @deviceSelectionConfiguration@ request parameter in 'ScheduleRun' .
--
--
--
-- /See:/ 'deviceSelectionConfiguration' smart constructor.
data DeviceSelectionConfiguration = DeviceSelectionConfiguration'
  { _dscFilters    :: ![DeviceFilter]
  , _dscMaxDevices :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceSelectionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscFilters' - Used to dynamically select a set of devices for a test run. A filter is made up of an attribute, an operator, and one or more values.     * __Attribute__  The aspect of a device such as platform or model used as the selection criteria in a device filter. Allowed values include:     * ARN: The Amazon Resource Name (ARN) of the device. For example, "arn:aws:devicefarm:us-west-2::device:12345Example".     * PLATFORM: The device platform. Valid values are "ANDROID" or "IOS".     * OS_VERSION: The operating system version. For example, "10.3.2".     * MODEL: The device model. For example, "iPad 5th Gen".     * AVAILABILITY: The current availability of the device. Valid values are "AVAILABLE", "HIGHLY_AVAILABLE", "BUSY", or "TEMPORARY_NOT_AVAILABLE".     * FORM_FACTOR: The device form factor. Valid values are "PHONE" or "TABLET".     * MANUFACTURER: The device manufacturer. For example, "Apple".     * REMOTE_ACCESS_ENABLED: Whether the device is enabled for remote access. Valid values are "TRUE" or "FALSE".     * REMOTE_DEBUG_ENABLED: Whether the device is enabled for remote debugging. Valid values are "TRUE" or "FALSE".     * INSTANCE_ARN: The Amazon Resource Name (ARN) of the device instance.     * INSTANCE_LABELS: The label of the device instance.     * FLEET_TYPE: The fleet type. Valid values are "PUBLIC" or "PRIVATE".     * __Operator__  The filter operator.     * The EQUALS operator is available for every attribute except INSTANCE_LABELS.     * The CONTAINS operator is available for the INSTANCE_LABELS and MODEL attributes.     * The IN and NOT_IN operators are available for the ARN, OS_VERSION, MODEL, MANUFACTURER, and INSTANCE_ARN attributes.     * The LESS_THAN, GREATER_THAN, LESS_THAN_OR_EQUALS, and GREATER_THAN_OR_EQUALS operators are also available for the OS_VERSION attribute.     * __Values__  An array of one or more filter values. __Operator Values__      * The IN and NOT_IN operators can take a values array that has more than one element.     * The other operators require an array with a single element. __Attribute Values__      * The PLATFORM attribute can be set to "ANDROID" or "IOS".     * The AVAILABILITY attribute can be set to "AVAILABLE", "HIGHLY_AVAILABLE", "BUSY", or "TEMPORARY_NOT_AVAILABLE".     * The FORM_FACTOR attribute can be set to "PHONE" or "TABLET".     * The FLEET_TYPE attribute can be set to "PUBLIC" or "PRIVATE".
--
-- * 'dscMaxDevices' - The maximum number of devices to be included in a test run.
deviceSelectionConfiguration
    :: Int -- ^ 'dscMaxDevices'
    -> DeviceSelectionConfiguration
deviceSelectionConfiguration pMaxDevices_ =
  DeviceSelectionConfiguration'
    {_dscFilters = mempty, _dscMaxDevices = pMaxDevices_}


-- | Used to dynamically select a set of devices for a test run. A filter is made up of an attribute, an operator, and one or more values.     * __Attribute__  The aspect of a device such as platform or model used as the selection criteria in a device filter. Allowed values include:     * ARN: The Amazon Resource Name (ARN) of the device. For example, "arn:aws:devicefarm:us-west-2::device:12345Example".     * PLATFORM: The device platform. Valid values are "ANDROID" or "IOS".     * OS_VERSION: The operating system version. For example, "10.3.2".     * MODEL: The device model. For example, "iPad 5th Gen".     * AVAILABILITY: The current availability of the device. Valid values are "AVAILABLE", "HIGHLY_AVAILABLE", "BUSY", or "TEMPORARY_NOT_AVAILABLE".     * FORM_FACTOR: The device form factor. Valid values are "PHONE" or "TABLET".     * MANUFACTURER: The device manufacturer. For example, "Apple".     * REMOTE_ACCESS_ENABLED: Whether the device is enabled for remote access. Valid values are "TRUE" or "FALSE".     * REMOTE_DEBUG_ENABLED: Whether the device is enabled for remote debugging. Valid values are "TRUE" or "FALSE".     * INSTANCE_ARN: The Amazon Resource Name (ARN) of the device instance.     * INSTANCE_LABELS: The label of the device instance.     * FLEET_TYPE: The fleet type. Valid values are "PUBLIC" or "PRIVATE".     * __Operator__  The filter operator.     * The EQUALS operator is available for every attribute except INSTANCE_LABELS.     * The CONTAINS operator is available for the INSTANCE_LABELS and MODEL attributes.     * The IN and NOT_IN operators are available for the ARN, OS_VERSION, MODEL, MANUFACTURER, and INSTANCE_ARN attributes.     * The LESS_THAN, GREATER_THAN, LESS_THAN_OR_EQUALS, and GREATER_THAN_OR_EQUALS operators are also available for the OS_VERSION attribute.     * __Values__  An array of one or more filter values. __Operator Values__      * The IN and NOT_IN operators can take a values array that has more than one element.     * The other operators require an array with a single element. __Attribute Values__      * The PLATFORM attribute can be set to "ANDROID" or "IOS".     * The AVAILABILITY attribute can be set to "AVAILABLE", "HIGHLY_AVAILABLE", "BUSY", or "TEMPORARY_NOT_AVAILABLE".     * The FORM_FACTOR attribute can be set to "PHONE" or "TABLET".     * The FLEET_TYPE attribute can be set to "PUBLIC" or "PRIVATE".
dscFilters :: Lens' DeviceSelectionConfiguration [DeviceFilter]
dscFilters = lens _dscFilters (\ s a -> s{_dscFilters = a}) . _Coerce

-- | The maximum number of devices to be included in a test run.
dscMaxDevices :: Lens' DeviceSelectionConfiguration Int
dscMaxDevices = lens _dscMaxDevices (\ s a -> s{_dscMaxDevices = a})

instance Hashable DeviceSelectionConfiguration where

instance NFData DeviceSelectionConfiguration where

instance ToJSON DeviceSelectionConfiguration where
        toJSON DeviceSelectionConfiguration'{..}
          = object
              (catMaybes
                 [Just ("filters" .= _dscFilters),
                  Just ("maxDevices" .= _dscMaxDevices)])

-- | Contains the run results requested by the device selection configuration as well as how many devices were returned. For an example of the JSON response syntax, see 'ScheduleRun' .
--
--
--
-- /See:/ 'deviceSelectionResult' smart constructor.
data DeviceSelectionResult = DeviceSelectionResult'
  { _dsrMatchedDevicesCount :: !(Maybe Int)
  , _dsrFilters             :: !(Maybe [DeviceFilter])
  , _dsrMaxDevices          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceSelectionResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrMatchedDevicesCount' - The number of devices that matched the device filter selection criteria.
--
-- * 'dsrFilters' - The filters in a device selection result.
--
-- * 'dsrMaxDevices' - The maximum number of devices to be selected by a device filter and included in a test run.
deviceSelectionResult
    :: DeviceSelectionResult
deviceSelectionResult =
  DeviceSelectionResult'
    { _dsrMatchedDevicesCount = Nothing
    , _dsrFilters = Nothing
    , _dsrMaxDevices = Nothing
    }


-- | The number of devices that matched the device filter selection criteria.
dsrMatchedDevicesCount :: Lens' DeviceSelectionResult (Maybe Int)
dsrMatchedDevicesCount = lens _dsrMatchedDevicesCount (\ s a -> s{_dsrMatchedDevicesCount = a})

-- | The filters in a device selection result.
dsrFilters :: Lens' DeviceSelectionResult [DeviceFilter]
dsrFilters = lens _dsrFilters (\ s a -> s{_dsrFilters = a}) . _Default . _Coerce

-- | The maximum number of devices to be selected by a device filter and included in a test run.
dsrMaxDevices :: Lens' DeviceSelectionResult (Maybe Int)
dsrMaxDevices = lens _dsrMaxDevices (\ s a -> s{_dsrMaxDevices = a})

instance FromJSON DeviceSelectionResult where
        parseJSON
          = withObject "DeviceSelectionResult"
              (\ x ->
                 DeviceSelectionResult' <$>
                   (x .:? "matchedDevicesCount") <*>
                     (x .:? "filters" .!= mempty)
                     <*> (x .:? "maxDevices"))

instance Hashable DeviceSelectionResult where

instance NFData DeviceSelectionResult where

-- | Represents configuration information about a test run, such as the execution timeout (in minutes).
--
--
--
-- /See:/ 'executionConfiguration' smart constructor.
data ExecutionConfiguration = ExecutionConfiguration'
  { _ecSkipAppResign      :: !(Maybe Bool)
  , _ecAccountsCleanup    :: !(Maybe Bool)
  , _ecAppPackagesCleanup :: !(Maybe Bool)
  , _ecJobTimeoutMinutes  :: !(Maybe Int)
  , _ecVideoCapture       :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExecutionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecSkipAppResign' - When set to @true@ , for private devices, Device Farm will not sign your app again. For public devices, Device Farm always signs your apps again and this parameter has no effect. For more information about how Device Farm re-signs your app(s), see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- * 'ecAccountsCleanup' - True if account cleanup is enabled at the beginning of the test; otherwise, false.
--
-- * 'ecAppPackagesCleanup' - True if app package cleanup is enabled at the beginning of the test; otherwise, false.
--
-- * 'ecJobTimeoutMinutes' - The number of minutes a test run will execute before it times out.
--
-- * 'ecVideoCapture' - Set to true to enable video capture; otherwise, set to false. The default is true.
executionConfiguration
    :: ExecutionConfiguration
executionConfiguration =
  ExecutionConfiguration'
    { _ecSkipAppResign = Nothing
    , _ecAccountsCleanup = Nothing
    , _ecAppPackagesCleanup = Nothing
    , _ecJobTimeoutMinutes = Nothing
    , _ecVideoCapture = Nothing
    }


-- | When set to @true@ , for private devices, Device Farm will not sign your app again. For public devices, Device Farm always signs your apps again and this parameter has no effect. For more information about how Device Farm re-signs your app(s), see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
ecSkipAppResign :: Lens' ExecutionConfiguration (Maybe Bool)
ecSkipAppResign = lens _ecSkipAppResign (\ s a -> s{_ecSkipAppResign = a})

-- | True if account cleanup is enabled at the beginning of the test; otherwise, false.
ecAccountsCleanup :: Lens' ExecutionConfiguration (Maybe Bool)
ecAccountsCleanup = lens _ecAccountsCleanup (\ s a -> s{_ecAccountsCleanup = a})

-- | True if app package cleanup is enabled at the beginning of the test; otherwise, false.
ecAppPackagesCleanup :: Lens' ExecutionConfiguration (Maybe Bool)
ecAppPackagesCleanup = lens _ecAppPackagesCleanup (\ s a -> s{_ecAppPackagesCleanup = a})

-- | The number of minutes a test run will execute before it times out.
ecJobTimeoutMinutes :: Lens' ExecutionConfiguration (Maybe Int)
ecJobTimeoutMinutes = lens _ecJobTimeoutMinutes (\ s a -> s{_ecJobTimeoutMinutes = a})

-- | Set to true to enable video capture; otherwise, set to false. The default is true.
ecVideoCapture :: Lens' ExecutionConfiguration (Maybe Bool)
ecVideoCapture = lens _ecVideoCapture (\ s a -> s{_ecVideoCapture = a})

instance Hashable ExecutionConfiguration where

instance NFData ExecutionConfiguration where

instance ToJSON ExecutionConfiguration where
        toJSON ExecutionConfiguration'{..}
          = object
              (catMaybes
                 [("skipAppResign" .=) <$> _ecSkipAppResign,
                  ("accountsCleanup" .=) <$> _ecAccountsCleanup,
                  ("appPackagesCleanup" .=) <$> _ecAppPackagesCleanup,
                  ("jobTimeoutMinutes" .=) <$> _ecJobTimeoutMinutes,
                  ("videoCapture" .=) <$> _ecVideoCapture])

-- | Represents information about incompatibility.
--
--
--
-- /See:/ 'incompatibilityMessage' smart constructor.
data IncompatibilityMessage = IncompatibilityMessage'
  { _imType    :: !(Maybe DeviceAttribute)
  , _imMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IncompatibilityMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imType' - The type of incompatibility. Allowed values include:     * ARN: The ARN.     * FORM_FACTOR: The form factor (for example, phone or tablet).     * MANUFACTURER: The manufacturer.     * PLATFORM: The platform (for example, Android or iOS).     * REMOTE_ACCESS_ENABLED: Whether the device is enabled for remote access.     * APPIUM_VERSION: The Appium version for the test.
--
-- * 'imMessage' - A message about the incompatibility.
incompatibilityMessage
    :: IncompatibilityMessage
incompatibilityMessage =
  IncompatibilityMessage' {_imType = Nothing, _imMessage = Nothing}


-- | The type of incompatibility. Allowed values include:     * ARN: The ARN.     * FORM_FACTOR: The form factor (for example, phone or tablet).     * MANUFACTURER: The manufacturer.     * PLATFORM: The platform (for example, Android or iOS).     * REMOTE_ACCESS_ENABLED: Whether the device is enabled for remote access.     * APPIUM_VERSION: The Appium version for the test.
imType :: Lens' IncompatibilityMessage (Maybe DeviceAttribute)
imType = lens _imType (\ s a -> s{_imType = a})

-- | A message about the incompatibility.
imMessage :: Lens' IncompatibilityMessage (Maybe Text)
imMessage = lens _imMessage (\ s a -> s{_imMessage = a})

instance FromJSON IncompatibilityMessage where
        parseJSON
          = withObject "IncompatibilityMessage"
              (\ x ->
                 IncompatibilityMessage' <$>
                   (x .:? "type") <*> (x .:? "message"))

instance Hashable IncompatibilityMessage where

instance NFData IncompatibilityMessage where

-- | Represents the instance profile.
--
--
--
-- /See:/ 'instanceProfile' smart constructor.
data InstanceProfile = InstanceProfile'
  { _ipArn                           :: !(Maybe Text)
  , _ipRebootAfterUse                :: !(Maybe Bool)
  , _ipName                          :: !(Maybe Text)
  , _ipPackageCleanup                :: !(Maybe Bool)
  , _ipExcludeAppPackagesFromCleanup :: !(Maybe [Text])
  , _ipDescription                   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipArn' - The Amazon Resource Name (ARN) of the instance profile.
--
-- * 'ipRebootAfterUse' - When set to @true@ , Device Farm will reboot the instance after a test run. The default value is @true@ .
--
-- * 'ipName' - The name of the instance profile.
--
-- * 'ipPackageCleanup' - When set to @true@ , Device Farm will remove app packages after a test run. The default value is @false@ for private devices.
--
-- * 'ipExcludeAppPackagesFromCleanup' - An array of strings specifying the list of app packages that should not be cleaned up from the device after a test run is over. The list of packages is only considered if you set @packageCleanup@ to @true@ .
--
-- * 'ipDescription' - The description of the instance profile.
instanceProfile
    :: InstanceProfile
instanceProfile =
  InstanceProfile'
    { _ipArn = Nothing
    , _ipRebootAfterUse = Nothing
    , _ipName = Nothing
    , _ipPackageCleanup = Nothing
    , _ipExcludeAppPackagesFromCleanup = Nothing
    , _ipDescription = Nothing
    }


-- | The Amazon Resource Name (ARN) of the instance profile.
ipArn :: Lens' InstanceProfile (Maybe Text)
ipArn = lens _ipArn (\ s a -> s{_ipArn = a})

-- | When set to @true@ , Device Farm will reboot the instance after a test run. The default value is @true@ .
ipRebootAfterUse :: Lens' InstanceProfile (Maybe Bool)
ipRebootAfterUse = lens _ipRebootAfterUse (\ s a -> s{_ipRebootAfterUse = a})

-- | The name of the instance profile.
ipName :: Lens' InstanceProfile (Maybe Text)
ipName = lens _ipName (\ s a -> s{_ipName = a})

-- | When set to @true@ , Device Farm will remove app packages after a test run. The default value is @false@ for private devices.
ipPackageCleanup :: Lens' InstanceProfile (Maybe Bool)
ipPackageCleanup = lens _ipPackageCleanup (\ s a -> s{_ipPackageCleanup = a})

-- | An array of strings specifying the list of app packages that should not be cleaned up from the device after a test run is over. The list of packages is only considered if you set @packageCleanup@ to @true@ .
ipExcludeAppPackagesFromCleanup :: Lens' InstanceProfile [Text]
ipExcludeAppPackagesFromCleanup = lens _ipExcludeAppPackagesFromCleanup (\ s a -> s{_ipExcludeAppPackagesFromCleanup = a}) . _Default . _Coerce

-- | The description of the instance profile.
ipDescription :: Lens' InstanceProfile (Maybe Text)
ipDescription = lens _ipDescription (\ s a -> s{_ipDescription = a})

instance FromJSON InstanceProfile where
        parseJSON
          = withObject "InstanceProfile"
              (\ x ->
                 InstanceProfile' <$>
                   (x .:? "arn") <*> (x .:? "rebootAfterUse") <*>
                     (x .:? "name")
                     <*> (x .:? "packageCleanup")
                     <*>
                     (x .:? "excludeAppPackagesFromCleanup" .!= mempty)
                     <*> (x .:? "description"))

instance Hashable InstanceProfile where

instance NFData InstanceProfile where

-- | Represents a device.
--
--
--
-- /See:/ 'job' smart constructor.
data Job = Job'
  { _jobInstanceARN   :: !(Maybe Text)
  , _jobStatus        :: !(Maybe ExecutionStatus)
  , _jobCounters      :: !(Maybe Counters)
  , _jobArn           :: !(Maybe Text)
  , _jobCreated       :: !(Maybe POSIX)
  , _jobDevice        :: !(Maybe Device)
  , _jobStopped       :: !(Maybe POSIX)
  , _jobResult        :: !(Maybe ExecutionResult)
  , _jobName          :: !(Maybe Text)
  , _jobVideoEndpoint :: !(Maybe Text)
  , _jobDeviceMinutes :: !(Maybe DeviceMinutes)
  , _jobVideoCapture  :: !(Maybe Bool)
  , _jobType          :: !(Maybe TestType)
  , _jobMessage       :: !(Maybe Text)
  , _jobStarted       :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jobInstanceARN' - The Amazon Resource Name (ARN) of the instance.
--
-- * 'jobStatus' - The job's status. Allowed values include:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
--
-- * 'jobCounters' - The job's result counters.
--
-- * 'jobArn' - The job's ARN.
--
-- * 'jobCreated' - When the job was created.
--
-- * 'jobDevice' - The device (phone or tablet).
--
-- * 'jobStopped' - The job's stop time.
--
-- * 'jobResult' - The job's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
--
-- * 'jobName' - The job's name.
--
-- * 'jobVideoEndpoint' - The endpoint for streaming device video.
--
-- * 'jobDeviceMinutes' - Represents the total (metered or unmetered) minutes used by the job.
--
-- * 'jobVideoCapture' - This value is set to true if video capture is enabled; otherwise, it is set to false.
--
-- * 'jobType' - The job's type. Allowed values include the following:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_NODE: The Appium Node.js type.     * APPIUM_RUBY: The Appium Ruby type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for web apps.     * APPIUM_WEB_NODE: The Appium Node.js type for web apps.     * APPIUM_WEB_RUBY: The Appium Ruby test type for web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
--
-- * 'jobMessage' - A message about the job's result.
--
-- * 'jobStarted' - The job's start time.
job
    :: Job
job =
  Job'
    { _jobInstanceARN = Nothing
    , _jobStatus = Nothing
    , _jobCounters = Nothing
    , _jobArn = Nothing
    , _jobCreated = Nothing
    , _jobDevice = Nothing
    , _jobStopped = Nothing
    , _jobResult = Nothing
    , _jobName = Nothing
    , _jobVideoEndpoint = Nothing
    , _jobDeviceMinutes = Nothing
    , _jobVideoCapture = Nothing
    , _jobType = Nothing
    , _jobMessage = Nothing
    , _jobStarted = Nothing
    }


-- | The Amazon Resource Name (ARN) of the instance.
jobInstanceARN :: Lens' Job (Maybe Text)
jobInstanceARN = lens _jobInstanceARN (\ s a -> s{_jobInstanceARN = a})

-- | The job's status. Allowed values include:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
jobStatus :: Lens' Job (Maybe ExecutionStatus)
jobStatus = lens _jobStatus (\ s a -> s{_jobStatus = a})

-- | The job's result counters.
jobCounters :: Lens' Job (Maybe Counters)
jobCounters = lens _jobCounters (\ s a -> s{_jobCounters = a})

-- | The job's ARN.
jobArn :: Lens' Job (Maybe Text)
jobArn = lens _jobArn (\ s a -> s{_jobArn = a})

-- | When the job was created.
jobCreated :: Lens' Job (Maybe UTCTime)
jobCreated = lens _jobCreated (\ s a -> s{_jobCreated = a}) . mapping _Time

-- | The device (phone or tablet).
jobDevice :: Lens' Job (Maybe Device)
jobDevice = lens _jobDevice (\ s a -> s{_jobDevice = a})

-- | The job's stop time.
jobStopped :: Lens' Job (Maybe UTCTime)
jobStopped = lens _jobStopped (\ s a -> s{_jobStopped = a}) . mapping _Time

-- | The job's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
jobResult :: Lens' Job (Maybe ExecutionResult)
jobResult = lens _jobResult (\ s a -> s{_jobResult = a})

-- | The job's name.
jobName :: Lens' Job (Maybe Text)
jobName = lens _jobName (\ s a -> s{_jobName = a})

-- | The endpoint for streaming device video.
jobVideoEndpoint :: Lens' Job (Maybe Text)
jobVideoEndpoint = lens _jobVideoEndpoint (\ s a -> s{_jobVideoEndpoint = a})

-- | Represents the total (metered or unmetered) minutes used by the job.
jobDeviceMinutes :: Lens' Job (Maybe DeviceMinutes)
jobDeviceMinutes = lens _jobDeviceMinutes (\ s a -> s{_jobDeviceMinutes = a})

-- | This value is set to true if video capture is enabled; otherwise, it is set to false.
jobVideoCapture :: Lens' Job (Maybe Bool)
jobVideoCapture = lens _jobVideoCapture (\ s a -> s{_jobVideoCapture = a})

-- | The job's type. Allowed values include the following:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_NODE: The Appium Node.js type.     * APPIUM_RUBY: The Appium Ruby type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for web apps.     * APPIUM_WEB_NODE: The Appium Node.js type for web apps.     * APPIUM_WEB_RUBY: The Appium Ruby test type for web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
jobType :: Lens' Job (Maybe TestType)
jobType = lens _jobType (\ s a -> s{_jobType = a})

-- | A message about the job's result.
jobMessage :: Lens' Job (Maybe Text)
jobMessage = lens _jobMessage (\ s a -> s{_jobMessage = a})

-- | The job's start time.
jobStarted :: Lens' Job (Maybe UTCTime)
jobStarted = lens _jobStarted (\ s a -> s{_jobStarted = a}) . mapping _Time

instance FromJSON Job where
        parseJSON
          = withObject "Job"
              (\ x ->
                 Job' <$>
                   (x .:? "instanceArn") <*> (x .:? "status") <*>
                     (x .:? "counters")
                     <*> (x .:? "arn")
                     <*> (x .:? "created")
                     <*> (x .:? "device")
                     <*> (x .:? "stopped")
                     <*> (x .:? "result")
                     <*> (x .:? "name")
                     <*> (x .:? "videoEndpoint")
                     <*> (x .:? "deviceMinutes")
                     <*> (x .:? "videoCapture")
                     <*> (x .:? "type")
                     <*> (x .:? "message")
                     <*> (x .:? "started"))

instance Hashable Job where

instance NFData Job where

-- | Represents a latitude and longitude pair, expressed in geographic coordinate system degrees (for example 47.6204, -122.3491).
--
--
-- Elevation is currently not supported.
--
--
-- /See:/ 'location' smart constructor.
data Location = Location'
  { _lLatitude  :: !Double
  , _lLongitude :: !Double
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lLatitude' - The latitude.
--
-- * 'lLongitude' - The longitude.
location
    :: Double -- ^ 'lLatitude'
    -> Double -- ^ 'lLongitude'
    -> Location
location pLatitude_ pLongitude_ =
  Location' {_lLatitude = pLatitude_, _lLongitude = pLongitude_}


-- | The latitude.
lLatitude :: Lens' Location Double
lLatitude = lens _lLatitude (\ s a -> s{_lLatitude = a})

-- | The longitude.
lLongitude :: Lens' Location Double
lLongitude = lens _lLongitude (\ s a -> s{_lLongitude = a})

instance FromJSON Location where
        parseJSON
          = withObject "Location"
              (\ x ->
                 Location' <$>
                   (x .: "latitude") <*> (x .: "longitude"))

instance Hashable Location where

instance NFData Location where

instance ToJSON Location where
        toJSON Location'{..}
          = object
              (catMaybes
                 [Just ("latitude" .= _lLatitude),
                  Just ("longitude" .= _lLongitude)])

-- | A number representing the monetary amount for an offering or transaction.
--
--
--
-- /See:/ 'monetaryAmount' smart constructor.
data MonetaryAmount = MonetaryAmount'
  { _maAmount       :: !(Maybe Double)
  , _maCurrencyCode :: !(Maybe CurrencyCode)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MonetaryAmount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maAmount' - The numerical amount of an offering or transaction.
--
-- * 'maCurrencyCode' - The currency code of a monetary amount. For example, @USD@ means "U.S. dollars."
monetaryAmount
    :: MonetaryAmount
monetaryAmount =
  MonetaryAmount' {_maAmount = Nothing, _maCurrencyCode = Nothing}


-- | The numerical amount of an offering or transaction.
maAmount :: Lens' MonetaryAmount (Maybe Double)
maAmount = lens _maAmount (\ s a -> s{_maAmount = a})

-- | The currency code of a monetary amount. For example, @USD@ means "U.S. dollars."
maCurrencyCode :: Lens' MonetaryAmount (Maybe CurrencyCode)
maCurrencyCode = lens _maCurrencyCode (\ s a -> s{_maCurrencyCode = a})

instance FromJSON MonetaryAmount where
        parseJSON
          = withObject "MonetaryAmount"
              (\ x ->
                 MonetaryAmount' <$>
                   (x .:? "amount") <*> (x .:? "currencyCode"))

instance Hashable MonetaryAmount where

instance NFData MonetaryAmount where

-- | An array of settings that describes characteristics of a network profile.
--
--
--
-- /See:/ 'networkProfile' smart constructor.
data NetworkProfile = NetworkProfile'
  { _npUplinkJitterMs        :: !(Maybe Integer)
  , _npArn                   :: !(Maybe Text)
  , _npUplinkLossPercent     :: !(Maybe Nat)
  , _npDownlinkJitterMs      :: !(Maybe Integer)
  , _npName                  :: !(Maybe Text)
  , _npDownlinkLossPercent   :: !(Maybe Nat)
  , _npType                  :: !(Maybe NetworkProfileType)
  , _npUplinkDelayMs         :: !(Maybe Integer)
  , _npUplinkBandwidthBits   :: !(Maybe Integer)
  , _npDescription           :: !(Maybe Text)
  , _npDownlinkDelayMs       :: !(Maybe Integer)
  , _npDownlinkBandwidthBits :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'npUplinkJitterMs' - Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- * 'npArn' - The Amazon Resource Name (ARN) of the network profile.
--
-- * 'npUplinkLossPercent' - Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
--
-- * 'npDownlinkJitterMs' - Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- * 'npName' - The name of the network profile.
--
-- * 'npDownlinkLossPercent' - Proportion of received packets that fail to arrive from 0 to 100 percent.
--
-- * 'npType' - The type of network profile. Valid values are listed below.
--
-- * 'npUplinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- * 'npUplinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- * 'npDescription' - The description of the network profile.
--
-- * 'npDownlinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- * 'npDownlinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to 104857600.
networkProfile
    :: NetworkProfile
networkProfile =
  NetworkProfile'
    { _npUplinkJitterMs = Nothing
    , _npArn = Nothing
    , _npUplinkLossPercent = Nothing
    , _npDownlinkJitterMs = Nothing
    , _npName = Nothing
    , _npDownlinkLossPercent = Nothing
    , _npType = Nothing
    , _npUplinkDelayMs = Nothing
    , _npUplinkBandwidthBits = Nothing
    , _npDescription = Nothing
    , _npDownlinkDelayMs = Nothing
    , _npDownlinkBandwidthBits = Nothing
    }


-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
npUplinkJitterMs :: Lens' NetworkProfile (Maybe Integer)
npUplinkJitterMs = lens _npUplinkJitterMs (\ s a -> s{_npUplinkJitterMs = a})

-- | The Amazon Resource Name (ARN) of the network profile.
npArn :: Lens' NetworkProfile (Maybe Text)
npArn = lens _npArn (\ s a -> s{_npArn = a})

-- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
npUplinkLossPercent :: Lens' NetworkProfile (Maybe Natural)
npUplinkLossPercent = lens _npUplinkLossPercent (\ s a -> s{_npUplinkLossPercent = a}) . mapping _Nat

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
npDownlinkJitterMs :: Lens' NetworkProfile (Maybe Integer)
npDownlinkJitterMs = lens _npDownlinkJitterMs (\ s a -> s{_npDownlinkJitterMs = a})

-- | The name of the network profile.
npName :: Lens' NetworkProfile (Maybe Text)
npName = lens _npName (\ s a -> s{_npName = a})

-- | Proportion of received packets that fail to arrive from 0 to 100 percent.
npDownlinkLossPercent :: Lens' NetworkProfile (Maybe Natural)
npDownlinkLossPercent = lens _npDownlinkLossPercent (\ s a -> s{_npDownlinkLossPercent = a}) . mapping _Nat

-- | The type of network profile. Valid values are listed below.
npType :: Lens' NetworkProfile (Maybe NetworkProfileType)
npType = lens _npType (\ s a -> s{_npType = a})

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
npUplinkDelayMs :: Lens' NetworkProfile (Maybe Integer)
npUplinkDelayMs = lens _npUplinkDelayMs (\ s a -> s{_npUplinkDelayMs = a})

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
npUplinkBandwidthBits :: Lens' NetworkProfile (Maybe Integer)
npUplinkBandwidthBits = lens _npUplinkBandwidthBits (\ s a -> s{_npUplinkBandwidthBits = a})

-- | The description of the network profile.
npDescription :: Lens' NetworkProfile (Maybe Text)
npDescription = lens _npDescription (\ s a -> s{_npDescription = a})

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
npDownlinkDelayMs :: Lens' NetworkProfile (Maybe Integer)
npDownlinkDelayMs = lens _npDownlinkDelayMs (\ s a -> s{_npDownlinkDelayMs = a})

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
npDownlinkBandwidthBits :: Lens' NetworkProfile (Maybe Integer)
npDownlinkBandwidthBits = lens _npDownlinkBandwidthBits (\ s a -> s{_npDownlinkBandwidthBits = a})

instance FromJSON NetworkProfile where
        parseJSON
          = withObject "NetworkProfile"
              (\ x ->
                 NetworkProfile' <$>
                   (x .:? "uplinkJitterMs") <*> (x .:? "arn") <*>
                     (x .:? "uplinkLossPercent")
                     <*> (x .:? "downlinkJitterMs")
                     <*> (x .:? "name")
                     <*> (x .:? "downlinkLossPercent")
                     <*> (x .:? "type")
                     <*> (x .:? "uplinkDelayMs")
                     <*> (x .:? "uplinkBandwidthBits")
                     <*> (x .:? "description")
                     <*> (x .:? "downlinkDelayMs")
                     <*> (x .:? "downlinkBandwidthBits"))

instance Hashable NetworkProfile where

instance NFData NetworkProfile where

-- | Represents the metadata of a device offering.
--
--
--
-- /See:/ 'offering' smart constructor.
data Offering = Offering'
  { _oPlatform         :: !(Maybe DevicePlatform)
  , _oId               :: !(Maybe Text)
  , _oRecurringCharges :: !(Maybe [RecurringCharge])
  , _oType             :: !(Maybe OfferingType)
  , _oDescription      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Offering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oPlatform' - The platform of the device (e.g., ANDROID or IOS).
--
-- * 'oId' - The ID that corresponds to a device offering.
--
-- * 'oRecurringCharges' - Specifies whether there are recurring charges for the offering.
--
-- * 'oType' - The type of offering (e.g., "RECURRING") for a device.
--
-- * 'oDescription' - A string describing the offering.
offering
    :: Offering
offering =
  Offering'
    { _oPlatform = Nothing
    , _oId = Nothing
    , _oRecurringCharges = Nothing
    , _oType = Nothing
    , _oDescription = Nothing
    }


-- | The platform of the device (e.g., ANDROID or IOS).
oPlatform :: Lens' Offering (Maybe DevicePlatform)
oPlatform = lens _oPlatform (\ s a -> s{_oPlatform = a})

-- | The ID that corresponds to a device offering.
oId :: Lens' Offering (Maybe Text)
oId = lens _oId (\ s a -> s{_oId = a})

-- | Specifies whether there are recurring charges for the offering.
oRecurringCharges :: Lens' Offering [RecurringCharge]
oRecurringCharges = lens _oRecurringCharges (\ s a -> s{_oRecurringCharges = a}) . _Default . _Coerce

-- | The type of offering (e.g., "RECURRING") for a device.
oType :: Lens' Offering (Maybe OfferingType)
oType = lens _oType (\ s a -> s{_oType = a})

-- | A string describing the offering.
oDescription :: Lens' Offering (Maybe Text)
oDescription = lens _oDescription (\ s a -> s{_oDescription = a})

instance FromJSON Offering where
        parseJSON
          = withObject "Offering"
              (\ x ->
                 Offering' <$>
                   (x .:? "platform") <*> (x .:? "id") <*>
                     (x .:? "recurringCharges" .!= mempty)
                     <*> (x .:? "type")
                     <*> (x .:? "description"))

instance Hashable Offering where

instance NFData Offering where

-- | Represents information about an offering promotion.
--
--
--
-- /See:/ 'offeringPromotion' smart constructor.
data OfferingPromotion = OfferingPromotion'
  { _opId          :: !(Maybe Text)
  , _opDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OfferingPromotion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'opId' - The ID of the offering promotion.
--
-- * 'opDescription' - A string describing the offering promotion.
offeringPromotion
    :: OfferingPromotion
offeringPromotion =
  OfferingPromotion' {_opId = Nothing, _opDescription = Nothing}


-- | The ID of the offering promotion.
opId :: Lens' OfferingPromotion (Maybe Text)
opId = lens _opId (\ s a -> s{_opId = a})

-- | A string describing the offering promotion.
opDescription :: Lens' OfferingPromotion (Maybe Text)
opDescription = lens _opDescription (\ s a -> s{_opDescription = a})

instance FromJSON OfferingPromotion where
        parseJSON
          = withObject "OfferingPromotion"
              (\ x ->
                 OfferingPromotion' <$>
                   (x .:? "id") <*> (x .:? "description"))

instance Hashable OfferingPromotion where

instance NFData OfferingPromotion where

-- | The status of the offering.
--
--
--
-- /See:/ 'offeringStatus' smart constructor.
data OfferingStatus = OfferingStatus'
  { _osEffectiveOn :: !(Maybe POSIX)
  , _osOffering    :: !(Maybe Offering)
  , _osQuantity    :: !(Maybe Int)
  , _osType        :: !(Maybe OfferingTransactionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OfferingStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osEffectiveOn' - The date on which the offering is effective.
--
-- * 'osOffering' - Represents the metadata of an offering status.
--
-- * 'osQuantity' - The number of available devices in the offering.
--
-- * 'osType' - The type specified for the offering status.
offeringStatus
    :: OfferingStatus
offeringStatus =
  OfferingStatus'
    { _osEffectiveOn = Nothing
    , _osOffering = Nothing
    , _osQuantity = Nothing
    , _osType = Nothing
    }


-- | The date on which the offering is effective.
osEffectiveOn :: Lens' OfferingStatus (Maybe UTCTime)
osEffectiveOn = lens _osEffectiveOn (\ s a -> s{_osEffectiveOn = a}) . mapping _Time

-- | Represents the metadata of an offering status.
osOffering :: Lens' OfferingStatus (Maybe Offering)
osOffering = lens _osOffering (\ s a -> s{_osOffering = a})

-- | The number of available devices in the offering.
osQuantity :: Lens' OfferingStatus (Maybe Int)
osQuantity = lens _osQuantity (\ s a -> s{_osQuantity = a})

-- | The type specified for the offering status.
osType :: Lens' OfferingStatus (Maybe OfferingTransactionType)
osType = lens _osType (\ s a -> s{_osType = a})

instance FromJSON OfferingStatus where
        parseJSON
          = withObject "OfferingStatus"
              (\ x ->
                 OfferingStatus' <$>
                   (x .:? "effectiveOn") <*> (x .:? "offering") <*>
                     (x .:? "quantity")
                     <*> (x .:? "type"))

instance Hashable OfferingStatus where

instance NFData OfferingStatus where

-- | Represents the metadata of an offering transaction.
--
--
--
-- /See:/ 'offeringTransaction' smart constructor.
data OfferingTransaction = OfferingTransaction'
  { _otOfferingStatus      :: !(Maybe OfferingStatus)
  , _otCost                :: !(Maybe MonetaryAmount)
  , _otTransactionId       :: !(Maybe Text)
  , _otOfferingPromotionId :: !(Maybe Text)
  , _otCreatedOn           :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OfferingTransaction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'otOfferingStatus' - The status of an offering transaction.
--
-- * 'otCost' - The cost of an offering transaction.
--
-- * 'otTransactionId' - The transaction ID of the offering transaction.
--
-- * 'otOfferingPromotionId' - The ID that corresponds to a device offering promotion.
--
-- * 'otCreatedOn' - The date on which an offering transaction was created.
offeringTransaction
    :: OfferingTransaction
offeringTransaction =
  OfferingTransaction'
    { _otOfferingStatus = Nothing
    , _otCost = Nothing
    , _otTransactionId = Nothing
    , _otOfferingPromotionId = Nothing
    , _otCreatedOn = Nothing
    }


-- | The status of an offering transaction.
otOfferingStatus :: Lens' OfferingTransaction (Maybe OfferingStatus)
otOfferingStatus = lens _otOfferingStatus (\ s a -> s{_otOfferingStatus = a})

-- | The cost of an offering transaction.
otCost :: Lens' OfferingTransaction (Maybe MonetaryAmount)
otCost = lens _otCost (\ s a -> s{_otCost = a})

-- | The transaction ID of the offering transaction.
otTransactionId :: Lens' OfferingTransaction (Maybe Text)
otTransactionId = lens _otTransactionId (\ s a -> s{_otTransactionId = a})

-- | The ID that corresponds to a device offering promotion.
otOfferingPromotionId :: Lens' OfferingTransaction (Maybe Text)
otOfferingPromotionId = lens _otOfferingPromotionId (\ s a -> s{_otOfferingPromotionId = a})

-- | The date on which an offering transaction was created.
otCreatedOn :: Lens' OfferingTransaction (Maybe UTCTime)
otCreatedOn = lens _otCreatedOn (\ s a -> s{_otCreatedOn = a}) . mapping _Time

instance FromJSON OfferingTransaction where
        parseJSON
          = withObject "OfferingTransaction"
              (\ x ->
                 OfferingTransaction' <$>
                   (x .:? "offeringStatus") <*> (x .:? "cost") <*>
                     (x .:? "transactionId")
                     <*> (x .:? "offeringPromotionId")
                     <*> (x .:? "createdOn"))

instance Hashable OfferingTransaction where

instance NFData OfferingTransaction where

-- | Represents a specific warning or failure.
--
--
--
-- /See:/ 'problem' smart constructor.
data Problem = Problem'
  { _pDevice  :: !(Maybe Device)
  , _pTest    :: !(Maybe ProblemDetail)
  , _pResult  :: !(Maybe ExecutionResult)
  , _pRun     :: !(Maybe ProblemDetail)
  , _pJob     :: !(Maybe ProblemDetail)
  , _pMessage :: !(Maybe Text)
  , _pSuite   :: !(Maybe ProblemDetail)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Problem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pDevice' - Information about the associated device.
--
-- * 'pTest' - Information about the associated test.
--
-- * 'pResult' - The problem's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
--
-- * 'pRun' - Information about the associated run.
--
-- * 'pJob' - Information about the associated job.
--
-- * 'pMessage' - A message about the problem's result.
--
-- * 'pSuite' - Information about the associated suite.
problem
    :: Problem
problem =
  Problem'
    { _pDevice = Nothing
    , _pTest = Nothing
    , _pResult = Nothing
    , _pRun = Nothing
    , _pJob = Nothing
    , _pMessage = Nothing
    , _pSuite = Nothing
    }


-- | Information about the associated device.
pDevice :: Lens' Problem (Maybe Device)
pDevice = lens _pDevice (\ s a -> s{_pDevice = a})

-- | Information about the associated test.
pTest :: Lens' Problem (Maybe ProblemDetail)
pTest = lens _pTest (\ s a -> s{_pTest = a})

-- | The problem's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
pResult :: Lens' Problem (Maybe ExecutionResult)
pResult = lens _pResult (\ s a -> s{_pResult = a})

-- | Information about the associated run.
pRun :: Lens' Problem (Maybe ProblemDetail)
pRun = lens _pRun (\ s a -> s{_pRun = a})

-- | Information about the associated job.
pJob :: Lens' Problem (Maybe ProblemDetail)
pJob = lens _pJob (\ s a -> s{_pJob = a})

-- | A message about the problem's result.
pMessage :: Lens' Problem (Maybe Text)
pMessage = lens _pMessage (\ s a -> s{_pMessage = a})

-- | Information about the associated suite.
pSuite :: Lens' Problem (Maybe ProblemDetail)
pSuite = lens _pSuite (\ s a -> s{_pSuite = a})

instance FromJSON Problem where
        parseJSON
          = withObject "Problem"
              (\ x ->
                 Problem' <$>
                   (x .:? "device") <*> (x .:? "test") <*>
                     (x .:? "result")
                     <*> (x .:? "run")
                     <*> (x .:? "job")
                     <*> (x .:? "message")
                     <*> (x .:? "suite"))

instance Hashable Problem where

instance NFData Problem where

-- | Information about a problem detail.
--
--
--
-- /See:/ 'problemDetail' smart constructor.
data ProblemDetail = ProblemDetail'
  { _pdArn  :: !(Maybe Text)
  , _pdName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProblemDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdArn' - The problem detail's ARN.
--
-- * 'pdName' - The problem detail's name.
problemDetail
    :: ProblemDetail
problemDetail = ProblemDetail' {_pdArn = Nothing, _pdName = Nothing}


-- | The problem detail's ARN.
pdArn :: Lens' ProblemDetail (Maybe Text)
pdArn = lens _pdArn (\ s a -> s{_pdArn = a})

-- | The problem detail's name.
pdName :: Lens' ProblemDetail (Maybe Text)
pdName = lens _pdName (\ s a -> s{_pdName = a})

instance FromJSON ProblemDetail where
        parseJSON
          = withObject "ProblemDetail"
              (\ x ->
                 ProblemDetail' <$> (x .:? "arn") <*> (x .:? "name"))

instance Hashable ProblemDetail where

instance NFData ProblemDetail where

-- | Represents an operating-system neutral workspace for running and managing tests.
--
--
--
-- /See:/ 'project' smart constructor.
data Project = Project'
  { _pArn                      :: !(Maybe Text)
  , _pCreated                  :: !(Maybe POSIX)
  , _pName                     :: !(Maybe Text)
  , _pDefaultJobTimeoutMinutes :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Project' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pArn' - The project's ARN.
--
-- * 'pCreated' - When the project was created.
--
-- * 'pName' - The project's name.
--
-- * 'pDefaultJobTimeoutMinutes' - The default number of minutes (at the project level) a test run will execute before it times out. Default value is 60 minutes.
project
    :: Project
project =
  Project'
    { _pArn = Nothing
    , _pCreated = Nothing
    , _pName = Nothing
    , _pDefaultJobTimeoutMinutes = Nothing
    }


-- | The project's ARN.
pArn :: Lens' Project (Maybe Text)
pArn = lens _pArn (\ s a -> s{_pArn = a})

-- | When the project was created.
pCreated :: Lens' Project (Maybe UTCTime)
pCreated = lens _pCreated (\ s a -> s{_pCreated = a}) . mapping _Time

-- | The project's name.
pName :: Lens' Project (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a})

-- | The default number of minutes (at the project level) a test run will execute before it times out. Default value is 60 minutes.
pDefaultJobTimeoutMinutes :: Lens' Project (Maybe Int)
pDefaultJobTimeoutMinutes = lens _pDefaultJobTimeoutMinutes (\ s a -> s{_pDefaultJobTimeoutMinutes = a})

instance FromJSON Project where
        parseJSON
          = withObject "Project"
              (\ x ->
                 Project' <$>
                   (x .:? "arn") <*> (x .:? "created") <*>
                     (x .:? "name")
                     <*> (x .:? "defaultJobTimeoutMinutes"))

instance Hashable Project where

instance NFData Project where

-- | Represents the set of radios and their states on a device. Examples of radios include Wi-Fi, GPS, Bluetooth, and NFC.
--
--
--
-- /See:/ 'radios' smart constructor.
data Radios = Radios'
  { _rNfc       :: !(Maybe Bool)
  , _rGps       :: !(Maybe Bool)
  , _rBluetooth :: !(Maybe Bool)
  , _rWifi      :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Radios' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rNfc' - True if NFC is enabled at the beginning of the test; otherwise, false.
--
-- * 'rGps' - True if GPS is enabled at the beginning of the test; otherwise, false.
--
-- * 'rBluetooth' - True if Bluetooth is enabled at the beginning of the test; otherwise, false.
--
-- * 'rWifi' - True if Wi-Fi is enabled at the beginning of the test; otherwise, false.
radios
    :: Radios
radios =
  Radios'
    {_rNfc = Nothing, _rGps = Nothing, _rBluetooth = Nothing, _rWifi = Nothing}


-- | True if NFC is enabled at the beginning of the test; otherwise, false.
rNfc :: Lens' Radios (Maybe Bool)
rNfc = lens _rNfc (\ s a -> s{_rNfc = a})

-- | True if GPS is enabled at the beginning of the test; otherwise, false.
rGps :: Lens' Radios (Maybe Bool)
rGps = lens _rGps (\ s a -> s{_rGps = a})

-- | True if Bluetooth is enabled at the beginning of the test; otherwise, false.
rBluetooth :: Lens' Radios (Maybe Bool)
rBluetooth = lens _rBluetooth (\ s a -> s{_rBluetooth = a})

-- | True if Wi-Fi is enabled at the beginning of the test; otherwise, false.
rWifi :: Lens' Radios (Maybe Bool)
rWifi = lens _rWifi (\ s a -> s{_rWifi = a})

instance FromJSON Radios where
        parseJSON
          = withObject "Radios"
              (\ x ->
                 Radios' <$>
                   (x .:? "nfc") <*> (x .:? "gps") <*>
                     (x .:? "bluetooth")
                     <*> (x .:? "wifi"))

instance Hashable Radios where

instance NFData Radios where

instance ToJSON Radios where
        toJSON Radios'{..}
          = object
              (catMaybes
                 [("nfc" .=) <$> _rNfc, ("gps" .=) <$> _rGps,
                  ("bluetooth" .=) <$> _rBluetooth,
                  ("wifi" .=) <$> _rWifi])

-- | Specifies whether charges for devices will be recurring.
--
--
--
-- /See:/ 'recurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { _rcFrequency :: !(Maybe RecurringChargeFrequency)
  , _rcCost      :: !(Maybe MonetaryAmount)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcFrequency' - The frequency in which charges will recur.
--
-- * 'rcCost' - The cost of the recurring charge.
recurringCharge
    :: RecurringCharge
recurringCharge = RecurringCharge' {_rcFrequency = Nothing, _rcCost = Nothing}


-- | The frequency in which charges will recur.
rcFrequency :: Lens' RecurringCharge (Maybe RecurringChargeFrequency)
rcFrequency = lens _rcFrequency (\ s a -> s{_rcFrequency = a})

-- | The cost of the recurring charge.
rcCost :: Lens' RecurringCharge (Maybe MonetaryAmount)
rcCost = lens _rcCost (\ s a -> s{_rcCost = a})

instance FromJSON RecurringCharge where
        parseJSON
          = withObject "RecurringCharge"
              (\ x ->
                 RecurringCharge' <$>
                   (x .:? "frequency") <*> (x .:? "cost"))

instance Hashable RecurringCharge where

instance NFData RecurringCharge where

-- | Represents information about the remote access session.
--
--
--
-- /See:/ 'remoteAccessSession' smart constructor.
data RemoteAccessSession = RemoteAccessSession'
  { _rasBillingMethod       :: !(Maybe BillingMethod)
  , _rasClientId            :: !(Maybe Text)
  , _rasDeviceUdid          :: !(Maybe Text)
  , _rasSkipAppResign       :: !(Maybe Bool)
  , _rasInstanceARN         :: !(Maybe Text)
  , _rasStatus              :: !(Maybe ExecutionStatus)
  , _rasRemoteRecordEnabled :: !(Maybe Bool)
  , _rasArn                 :: !(Maybe Text)
  , _rasRemoteRecordAppARN  :: !(Maybe Text)
  , _rasCreated             :: !(Maybe POSIX)
  , _rasDevice              :: !(Maybe Device)
  , _rasStopped             :: !(Maybe POSIX)
  , _rasResult              :: !(Maybe ExecutionResult)
  , _rasName                :: !(Maybe Text)
  , _rasDeviceMinutes       :: !(Maybe DeviceMinutes)
  , _rasRemoteDebugEnabled  :: !(Maybe Bool)
  , _rasEndpoint            :: !(Maybe Text)
  , _rasMessage             :: !(Maybe Text)
  , _rasHostAddress         :: !(Maybe Text)
  , _rasInteractionMode     :: !(Maybe InteractionMode)
  , _rasStarted             :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoteAccessSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rasBillingMethod' - The billing method of the remote access session. Possible values include @METERED@ or @UNMETERED@ . For more information about metered devices, see <https://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology> ."
--
-- * 'rasClientId' - Unique identifier of your client for the remote access session. Only returned if remote debugging is enabled for the remote access session.
--
-- * 'rasDeviceUdid' - Unique device identifier for the remote device. Only returned if remote debugging is enabled for the remote access session.
--
-- * 'rasSkipAppResign' - When set to @true@ , for private devices, Device Farm will not sign your app again. For public devices, Device Farm always signs your apps again and this parameter has no effect. For more information about how Device Farm re-signs your app(s), see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- * 'rasInstanceARN' - The Amazon Resource Name (ARN) of the instance.
--
-- * 'rasStatus' - The status of the remote access session. Can be any of the following:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
--
-- * 'rasRemoteRecordEnabled' - This flag is set to @true@ if remote recording is enabled for the remote access session.
--
-- * 'rasArn' - The Amazon Resource Name (ARN) of the remote access session.
--
-- * 'rasRemoteRecordAppARN' - The Amazon Resource Name (ARN) for the app to be recorded in the remote access session.
--
-- * 'rasCreated' - The date and time the remote access session was created.
--
-- * 'rasDevice' - The device (phone or tablet) used in the remote access session.
--
-- * 'rasStopped' - The date and time the remote access session was stopped.
--
-- * 'rasResult' - The result of the remote access session. Can be any of the following:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
--
-- * 'rasName' - The name of the remote access session.
--
-- * 'rasDeviceMinutes' - The number of minutes a device is used in a remote access sesssion (including setup and teardown minutes).
--
-- * 'rasRemoteDebugEnabled' - This flag is set to @true@ if remote debugging is enabled for the remote access session.
--
-- * 'rasEndpoint' - The endpoint for the remote access sesssion.
--
-- * 'rasMessage' - A message about the remote access session.
--
-- * 'rasHostAddress' - IP address of the EC2 host where you need to connect to remotely debug devices. Only returned if remote debugging is enabled for the remote access session.
--
-- * 'rasInteractionMode' - The interaction mode of the remote access session. Valid values are:     * INTERACTIVE: You can interact with the iOS device by viewing, touching, and rotating the screen. You __cannot__ run XCUITest framework-based tests in this mode.     * NO_VIDEO: You are connected to the device but cannot interact with it or view the screen. This mode has the fastest test execution speed. You __can__ run XCUITest framework-based tests in this mode.     * VIDEO_ONLY: You can view the screen but cannot touch or rotate it. You __can__ run XCUITest framework-based tests and watch the screen in this mode.
--
-- * 'rasStarted' - The date and time the remote access session was started.
remoteAccessSession
    :: RemoteAccessSession
remoteAccessSession =
  RemoteAccessSession'
    { _rasBillingMethod = Nothing
    , _rasClientId = Nothing
    , _rasDeviceUdid = Nothing
    , _rasSkipAppResign = Nothing
    , _rasInstanceARN = Nothing
    , _rasStatus = Nothing
    , _rasRemoteRecordEnabled = Nothing
    , _rasArn = Nothing
    , _rasRemoteRecordAppARN = Nothing
    , _rasCreated = Nothing
    , _rasDevice = Nothing
    , _rasStopped = Nothing
    , _rasResult = Nothing
    , _rasName = Nothing
    , _rasDeviceMinutes = Nothing
    , _rasRemoteDebugEnabled = Nothing
    , _rasEndpoint = Nothing
    , _rasMessage = Nothing
    , _rasHostAddress = Nothing
    , _rasInteractionMode = Nothing
    , _rasStarted = Nothing
    }


-- | The billing method of the remote access session. Possible values include @METERED@ or @UNMETERED@ . For more information about metered devices, see <https://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology> ."
rasBillingMethod :: Lens' RemoteAccessSession (Maybe BillingMethod)
rasBillingMethod = lens _rasBillingMethod (\ s a -> s{_rasBillingMethod = a})

-- | Unique identifier of your client for the remote access session. Only returned if remote debugging is enabled for the remote access session.
rasClientId :: Lens' RemoteAccessSession (Maybe Text)
rasClientId = lens _rasClientId (\ s a -> s{_rasClientId = a})

-- | Unique device identifier for the remote device. Only returned if remote debugging is enabled for the remote access session.
rasDeviceUdid :: Lens' RemoteAccessSession (Maybe Text)
rasDeviceUdid = lens _rasDeviceUdid (\ s a -> s{_rasDeviceUdid = a})

-- | When set to @true@ , for private devices, Device Farm will not sign your app again. For public devices, Device Farm always signs your apps again and this parameter has no effect. For more information about how Device Farm re-signs your app(s), see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
rasSkipAppResign :: Lens' RemoteAccessSession (Maybe Bool)
rasSkipAppResign = lens _rasSkipAppResign (\ s a -> s{_rasSkipAppResign = a})

-- | The Amazon Resource Name (ARN) of the instance.
rasInstanceARN :: Lens' RemoteAccessSession (Maybe Text)
rasInstanceARN = lens _rasInstanceARN (\ s a -> s{_rasInstanceARN = a})

-- | The status of the remote access session. Can be any of the following:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
rasStatus :: Lens' RemoteAccessSession (Maybe ExecutionStatus)
rasStatus = lens _rasStatus (\ s a -> s{_rasStatus = a})

-- | This flag is set to @true@ if remote recording is enabled for the remote access session.
rasRemoteRecordEnabled :: Lens' RemoteAccessSession (Maybe Bool)
rasRemoteRecordEnabled = lens _rasRemoteRecordEnabled (\ s a -> s{_rasRemoteRecordEnabled = a})

-- | The Amazon Resource Name (ARN) of the remote access session.
rasArn :: Lens' RemoteAccessSession (Maybe Text)
rasArn = lens _rasArn (\ s a -> s{_rasArn = a})

-- | The Amazon Resource Name (ARN) for the app to be recorded in the remote access session.
rasRemoteRecordAppARN :: Lens' RemoteAccessSession (Maybe Text)
rasRemoteRecordAppARN = lens _rasRemoteRecordAppARN (\ s a -> s{_rasRemoteRecordAppARN = a})

-- | The date and time the remote access session was created.
rasCreated :: Lens' RemoteAccessSession (Maybe UTCTime)
rasCreated = lens _rasCreated (\ s a -> s{_rasCreated = a}) . mapping _Time

-- | The device (phone or tablet) used in the remote access session.
rasDevice :: Lens' RemoteAccessSession (Maybe Device)
rasDevice = lens _rasDevice (\ s a -> s{_rasDevice = a})

-- | The date and time the remote access session was stopped.
rasStopped :: Lens' RemoteAccessSession (Maybe UTCTime)
rasStopped = lens _rasStopped (\ s a -> s{_rasStopped = a}) . mapping _Time

-- | The result of the remote access session. Can be any of the following:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
rasResult :: Lens' RemoteAccessSession (Maybe ExecutionResult)
rasResult = lens _rasResult (\ s a -> s{_rasResult = a})

-- | The name of the remote access session.
rasName :: Lens' RemoteAccessSession (Maybe Text)
rasName = lens _rasName (\ s a -> s{_rasName = a})

-- | The number of minutes a device is used in a remote access sesssion (including setup and teardown minutes).
rasDeviceMinutes :: Lens' RemoteAccessSession (Maybe DeviceMinutes)
rasDeviceMinutes = lens _rasDeviceMinutes (\ s a -> s{_rasDeviceMinutes = a})

-- | This flag is set to @true@ if remote debugging is enabled for the remote access session.
rasRemoteDebugEnabled :: Lens' RemoteAccessSession (Maybe Bool)
rasRemoteDebugEnabled = lens _rasRemoteDebugEnabled (\ s a -> s{_rasRemoteDebugEnabled = a})

-- | The endpoint for the remote access sesssion.
rasEndpoint :: Lens' RemoteAccessSession (Maybe Text)
rasEndpoint = lens _rasEndpoint (\ s a -> s{_rasEndpoint = a})

-- | A message about the remote access session.
rasMessage :: Lens' RemoteAccessSession (Maybe Text)
rasMessage = lens _rasMessage (\ s a -> s{_rasMessage = a})

-- | IP address of the EC2 host where you need to connect to remotely debug devices. Only returned if remote debugging is enabled for the remote access session.
rasHostAddress :: Lens' RemoteAccessSession (Maybe Text)
rasHostAddress = lens _rasHostAddress (\ s a -> s{_rasHostAddress = a})

-- | The interaction mode of the remote access session. Valid values are:     * INTERACTIVE: You can interact with the iOS device by viewing, touching, and rotating the screen. You __cannot__ run XCUITest framework-based tests in this mode.     * NO_VIDEO: You are connected to the device but cannot interact with it or view the screen. This mode has the fastest test execution speed. You __can__ run XCUITest framework-based tests in this mode.     * VIDEO_ONLY: You can view the screen but cannot touch or rotate it. You __can__ run XCUITest framework-based tests and watch the screen in this mode.
rasInteractionMode :: Lens' RemoteAccessSession (Maybe InteractionMode)
rasInteractionMode = lens _rasInteractionMode (\ s a -> s{_rasInteractionMode = a})

-- | The date and time the remote access session was started.
rasStarted :: Lens' RemoteAccessSession (Maybe UTCTime)
rasStarted = lens _rasStarted (\ s a -> s{_rasStarted = a}) . mapping _Time

instance FromJSON RemoteAccessSession where
        parseJSON
          = withObject "RemoteAccessSession"
              (\ x ->
                 RemoteAccessSession' <$>
                   (x .:? "billingMethod") <*> (x .:? "clientId") <*>
                     (x .:? "deviceUdid")
                     <*> (x .:? "skipAppResign")
                     <*> (x .:? "instanceArn")
                     <*> (x .:? "status")
                     <*> (x .:? "remoteRecordEnabled")
                     <*> (x .:? "arn")
                     <*> (x .:? "remoteRecordAppArn")
                     <*> (x .:? "created")
                     <*> (x .:? "device")
                     <*> (x .:? "stopped")
                     <*> (x .:? "result")
                     <*> (x .:? "name")
                     <*> (x .:? "deviceMinutes")
                     <*> (x .:? "remoteDebugEnabled")
                     <*> (x .:? "endpoint")
                     <*> (x .:? "message")
                     <*> (x .:? "hostAddress")
                     <*> (x .:? "interactionMode")
                     <*> (x .:? "started"))

instance Hashable RemoteAccessSession where

instance NFData RemoteAccessSession where

-- | Represents the screen resolution of a device in height and width, expressed in pixels.
--
--
--
-- /See:/ 'resolution' smart constructor.
data Resolution = Resolution'
  { _rHeight :: !(Maybe Int)
  , _rWidth  :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Resolution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rHeight' - The screen resolution's height, expressed in pixels.
--
-- * 'rWidth' - The screen resolution's width, expressed in pixels.
resolution
    :: Resolution
resolution = Resolution' {_rHeight = Nothing, _rWidth = Nothing}


-- | The screen resolution's height, expressed in pixels.
rHeight :: Lens' Resolution (Maybe Int)
rHeight = lens _rHeight (\ s a -> s{_rHeight = a})

-- | The screen resolution's width, expressed in pixels.
rWidth :: Lens' Resolution (Maybe Int)
rWidth = lens _rWidth (\ s a -> s{_rWidth = a})

instance FromJSON Resolution where
        parseJSON
          = withObject "Resolution"
              (\ x ->
                 Resolution' <$> (x .:? "height") <*> (x .:? "width"))

instance Hashable Resolution where

instance NFData Resolution where

-- | Represents a condition for a device pool.
--
--
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
  { _rAttribute :: !(Maybe DeviceAttribute)
  , _rOperator  :: !(Maybe RuleOperator)
  , _rValue     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rAttribute' - The rule's stringified attribute. For example, specify the value as @"\"abc\""@ . The supported operators for each attribute are provided in the following list.     * APPIUM_VERSION    * The Appium version for the test. /Supported operators/ : @CONTAINS@      * ARN    * The Amazon Resource Name (ARN) of the device. For example, "arn:aws:devicefarm:us-west-2::device:12345Example". /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * AVAILABILITY    * The current availability of the device. Valid values are "AVAILABLE", "HIGHLY_AVAILABLE", "BUSY", or "TEMPORARY_NOT_AVAILABLE". /Supported operators/ : @EQUALS@      * FLEET_TYPE    * The fleet type. Valid values are "PUBLIC" or "PRIVATE". /Supported operators/ : @EQUALS@      * FORM_FACTOR    * The device form factor. Valid values are "PHONE" or "TABLET". /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * INSTANCE_ARN    * The Amazon Resource Name (ARN) of the device instance. /Supported operators/ : @IN@ , @NOT_IN@      * INSTANCE_LABELS    * The label of the device instance. /Supported operators/ : @CONTAINS@      * MANUFACTURER    * The device manufacturer. For example, "Apple". /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * MODEL    * The device model, such as "Apple iPad Air 2" or "Google Pixel". /Supported operators/ : @CONTAINS@ , @EQUALS@ , @IN@ , @NOT_IN@      * OS_VERSION    * The operating system version. For example, "10.3.2". /Supported operators/ : @EQUALS@ , @GREATER_THAN@ , @GREATER_THAN_OR_EQUALS@ , @IN@ , @LESS_THAN@ , @LESS_THAN_OR_EQUALS@ , @NOT_IN@      * PLATFORM    * The device platform. Valid values are "ANDROID" or "IOS". /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * REMOTE_ACCESS_ENABLED    * Whether the device is enabled for remote access. Valid values are "TRUE" or "FALSE". /Supported operators/ : @EQUALS@      * REMOTE_DEBUG_ENABLED    * Whether the device is enabled for remote debugging. Valid values are "TRUE" or "FALSE". /Supported operators/ : @EQUALS@
--
-- * 'rOperator' - Specifies how Device Farm compares the rule's attribute to the value. For the operators that are supported by each attribute, see the attribute descriptions.
--
-- * 'rValue' - The rule's value.
rule
    :: Rule
rule = Rule' {_rAttribute = Nothing, _rOperator = Nothing, _rValue = Nothing}


-- | The rule's stringified attribute. For example, specify the value as @"\"abc\""@ . The supported operators for each attribute are provided in the following list.     * APPIUM_VERSION    * The Appium version for the test. /Supported operators/ : @CONTAINS@      * ARN    * The Amazon Resource Name (ARN) of the device. For example, "arn:aws:devicefarm:us-west-2::device:12345Example". /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * AVAILABILITY    * The current availability of the device. Valid values are "AVAILABLE", "HIGHLY_AVAILABLE", "BUSY", or "TEMPORARY_NOT_AVAILABLE". /Supported operators/ : @EQUALS@      * FLEET_TYPE    * The fleet type. Valid values are "PUBLIC" or "PRIVATE". /Supported operators/ : @EQUALS@      * FORM_FACTOR    * The device form factor. Valid values are "PHONE" or "TABLET". /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * INSTANCE_ARN    * The Amazon Resource Name (ARN) of the device instance. /Supported operators/ : @IN@ , @NOT_IN@      * INSTANCE_LABELS    * The label of the device instance. /Supported operators/ : @CONTAINS@      * MANUFACTURER    * The device manufacturer. For example, "Apple". /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * MODEL    * The device model, such as "Apple iPad Air 2" or "Google Pixel". /Supported operators/ : @CONTAINS@ , @EQUALS@ , @IN@ , @NOT_IN@      * OS_VERSION    * The operating system version. For example, "10.3.2". /Supported operators/ : @EQUALS@ , @GREATER_THAN@ , @GREATER_THAN_OR_EQUALS@ , @IN@ , @LESS_THAN@ , @LESS_THAN_OR_EQUALS@ , @NOT_IN@      * PLATFORM    * The device platform. Valid values are "ANDROID" or "IOS". /Supported operators/ : @EQUALS@ , @IN@ , @NOT_IN@      * REMOTE_ACCESS_ENABLED    * Whether the device is enabled for remote access. Valid values are "TRUE" or "FALSE". /Supported operators/ : @EQUALS@      * REMOTE_DEBUG_ENABLED    * Whether the device is enabled for remote debugging. Valid values are "TRUE" or "FALSE". /Supported operators/ : @EQUALS@
rAttribute :: Lens' Rule (Maybe DeviceAttribute)
rAttribute = lens _rAttribute (\ s a -> s{_rAttribute = a})

-- | Specifies how Device Farm compares the rule's attribute to the value. For the operators that are supported by each attribute, see the attribute descriptions.
rOperator :: Lens' Rule (Maybe RuleOperator)
rOperator = lens _rOperator (\ s a -> s{_rOperator = a})

-- | The rule's value.
rValue :: Lens' Rule (Maybe Text)
rValue = lens _rValue (\ s a -> s{_rValue = a})

instance FromJSON Rule where
        parseJSON
          = withObject "Rule"
              (\ x ->
                 Rule' <$>
                   (x .:? "attribute") <*> (x .:? "operator") <*>
                     (x .:? "value"))

instance Hashable Rule where

instance NFData Rule where

instance ToJSON Rule where
        toJSON Rule'{..}
          = object
              (catMaybes
                 [("attribute" .=) <$> _rAttribute,
                  ("operator" .=) <$> _rOperator,
                  ("value" .=) <$> _rValue])

-- | Represents a test run on a set of devices with a given app package, test parameters, etc.
--
--
--
-- /See:/ 'run' smart constructor.
data Run = Run'
  { _runBillingMethod         :: !(Maybe BillingMethod)
  , _runSkipAppResign         :: !(Maybe Bool)
  , _runStatus                :: !(Maybe ExecutionStatus)
  , _runCustomerArtifactPaths :: !(Maybe CustomerArtifactPaths)
  , _runEventCount            :: !(Maybe Int)
  , _runCounters              :: !(Maybe Counters)
  , _runPlatform              :: !(Maybe DevicePlatform)
  , _runSeed                  :: !(Maybe Int)
  , _runRadios                :: !(Maybe Radios)
  , _runArn                   :: !(Maybe Text)
  , _runLocation              :: !(Maybe Location)
  , _runCreated               :: !(Maybe POSIX)
  , _runLocale                :: !(Maybe Text)
  , _runTestSpecARN           :: !(Maybe Text)
  , _runStopped               :: !(Maybe POSIX)
  , _runResult                :: !(Maybe ExecutionResult)
  , _runJobTimeoutMinutes     :: !(Maybe Int)
  , _runCompletedJobs         :: !(Maybe Int)
  , _runResultCode            :: !(Maybe ExecutionResultCode)
  , _runName                  :: !(Maybe Text)
  , _runAppUpload             :: !(Maybe Text)
  , _runParsingResultURL      :: !(Maybe Text)
  , _runNetworkProfile        :: !(Maybe NetworkProfile)
  , _runDeviceMinutes         :: !(Maybe DeviceMinutes)
  , _runType                  :: !(Maybe TestType)
  , _runMessage               :: !(Maybe Text)
  , _runWebURL                :: !(Maybe Text)
  , _runTotalJobs             :: !(Maybe Int)
  , _runDevicePoolARN         :: !(Maybe Text)
  , _runStarted               :: !(Maybe POSIX)
  , _runDeviceSelectionResult :: !(Maybe DeviceSelectionResult)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Run' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'runBillingMethod' - Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
--
-- * 'runSkipAppResign' - When set to @true@ , for private devices, Device Farm will not sign your app again. For public devices, Device Farm always signs your apps again and this parameter has no effect. For more information about how Device Farm re-signs your app(s), see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- * 'runStatus' - The run's status. Allowed values include:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
--
-- * 'runCustomerArtifactPaths' - Output @CustomerArtifactPaths@ object for the test run.
--
-- * 'runEventCount' - For fuzz tests, this is the number of events, between 1 and 10000, that the UI fuzz test should perform.
--
-- * 'runCounters' - The run's result counters.
--
-- * 'runPlatform' - The run's platform. Allowed values include:     * ANDROID: The Android platform.     * IOS: The iOS platform.
--
-- * 'runSeed' - For fuzz tests, this is a seed to use for randomizing the UI fuzz test. Using the same seed value between tests ensures identical event sequences.
--
-- * 'runRadios' - Information about the radio states for the run.
--
-- * 'runArn' - The run's ARN.
--
-- * 'runLocation' - Information about the location that is used for the run.
--
-- * 'runCreated' - When the run was created.
--
-- * 'runLocale' - Information about the locale that is used for the run.
--
-- * 'runTestSpecARN' - The ARN of the YAML-formatted test specification for the run.
--
-- * 'runStopped' - The run's stop time.
--
-- * 'runResult' - The run's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
--
-- * 'runJobTimeoutMinutes' - The number of minutes the job will execute before it times out.
--
-- * 'runCompletedJobs' - The total number of completed jobs.
--
-- * 'runResultCode' - Supporting field for the result field. Set only if @result@ is @SKIPPED@ . @PARSING_FAILED@ if the result is skipped because of test package parsing failure.
--
-- * 'runName' - The run's name.
--
-- * 'runAppUpload' - An app to upload or that has been uploaded.
--
-- * 'runParsingResultURL' - Read-only URL for an object in S3 bucket where you can get the parsing results of the test package. If the test package doesn't parse, the reason why it doesn't parse appears in the file that this URL points to.
--
-- * 'runNetworkProfile' - The network profile being used for a test run.
--
-- * 'runDeviceMinutes' - Represents the total (metered or unmetered) minutes used by the test run.
--
-- * 'runType' - The run's type. Must be one of the following values:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_NODE: The Appium Node.js type.     * APPIUM_RUBY: The Appium Ruby type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for web apps.     * APPIUM_WEB_NODE: The Appium Node.js type for web apps.     * APPIUM_WEB_RUBY: The Appium Ruby type for web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
--
-- * 'runMessage' - A message about the run's result.
--
-- * 'runWebURL' - The Device Farm console URL for the recording of the run.
--
-- * 'runTotalJobs' - The total number of jobs for the run.
--
-- * 'runDevicePoolARN' - The ARN of the device pool for the run.
--
-- * 'runStarted' - The run's start time.
--
-- * 'runDeviceSelectionResult' - The results of a device filter used to select the devices for a test run.
run
    :: Run
run =
  Run'
    { _runBillingMethod = Nothing
    , _runSkipAppResign = Nothing
    , _runStatus = Nothing
    , _runCustomerArtifactPaths = Nothing
    , _runEventCount = Nothing
    , _runCounters = Nothing
    , _runPlatform = Nothing
    , _runSeed = Nothing
    , _runRadios = Nothing
    , _runArn = Nothing
    , _runLocation = Nothing
    , _runCreated = Nothing
    , _runLocale = Nothing
    , _runTestSpecARN = Nothing
    , _runStopped = Nothing
    , _runResult = Nothing
    , _runJobTimeoutMinutes = Nothing
    , _runCompletedJobs = Nothing
    , _runResultCode = Nothing
    , _runName = Nothing
    , _runAppUpload = Nothing
    , _runParsingResultURL = Nothing
    , _runNetworkProfile = Nothing
    , _runDeviceMinutes = Nothing
    , _runType = Nothing
    , _runMessage = Nothing
    , _runWebURL = Nothing
    , _runTotalJobs = Nothing
    , _runDevicePoolARN = Nothing
    , _runStarted = Nothing
    , _runDeviceSelectionResult = Nothing
    }


-- | Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
runBillingMethod :: Lens' Run (Maybe BillingMethod)
runBillingMethod = lens _runBillingMethod (\ s a -> s{_runBillingMethod = a})

-- | When set to @true@ , for private devices, Device Farm will not sign your app again. For public devices, Device Farm always signs your apps again and this parameter has no effect. For more information about how Device Farm re-signs your app(s), see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
runSkipAppResign :: Lens' Run (Maybe Bool)
runSkipAppResign = lens _runSkipAppResign (\ s a -> s{_runSkipAppResign = a})

-- | The run's status. Allowed values include:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
runStatus :: Lens' Run (Maybe ExecutionStatus)
runStatus = lens _runStatus (\ s a -> s{_runStatus = a})

-- | Output @CustomerArtifactPaths@ object for the test run.
runCustomerArtifactPaths :: Lens' Run (Maybe CustomerArtifactPaths)
runCustomerArtifactPaths = lens _runCustomerArtifactPaths (\ s a -> s{_runCustomerArtifactPaths = a})

-- | For fuzz tests, this is the number of events, between 1 and 10000, that the UI fuzz test should perform.
runEventCount :: Lens' Run (Maybe Int)
runEventCount = lens _runEventCount (\ s a -> s{_runEventCount = a})

-- | The run's result counters.
runCounters :: Lens' Run (Maybe Counters)
runCounters = lens _runCounters (\ s a -> s{_runCounters = a})

-- | The run's platform. Allowed values include:     * ANDROID: The Android platform.     * IOS: The iOS platform.
runPlatform :: Lens' Run (Maybe DevicePlatform)
runPlatform = lens _runPlatform (\ s a -> s{_runPlatform = a})

-- | For fuzz tests, this is a seed to use for randomizing the UI fuzz test. Using the same seed value between tests ensures identical event sequences.
runSeed :: Lens' Run (Maybe Int)
runSeed = lens _runSeed (\ s a -> s{_runSeed = a})

-- | Information about the radio states for the run.
runRadios :: Lens' Run (Maybe Radios)
runRadios = lens _runRadios (\ s a -> s{_runRadios = a})

-- | The run's ARN.
runArn :: Lens' Run (Maybe Text)
runArn = lens _runArn (\ s a -> s{_runArn = a})

-- | Information about the location that is used for the run.
runLocation :: Lens' Run (Maybe Location)
runLocation = lens _runLocation (\ s a -> s{_runLocation = a})

-- | When the run was created.
runCreated :: Lens' Run (Maybe UTCTime)
runCreated = lens _runCreated (\ s a -> s{_runCreated = a}) . mapping _Time

-- | Information about the locale that is used for the run.
runLocale :: Lens' Run (Maybe Text)
runLocale = lens _runLocale (\ s a -> s{_runLocale = a})

-- | The ARN of the YAML-formatted test specification for the run.
runTestSpecARN :: Lens' Run (Maybe Text)
runTestSpecARN = lens _runTestSpecARN (\ s a -> s{_runTestSpecARN = a})

-- | The run's stop time.
runStopped :: Lens' Run (Maybe UTCTime)
runStopped = lens _runStopped (\ s a -> s{_runStopped = a}) . mapping _Time

-- | The run's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
runResult :: Lens' Run (Maybe ExecutionResult)
runResult = lens _runResult (\ s a -> s{_runResult = a})

-- | The number of minutes the job will execute before it times out.
runJobTimeoutMinutes :: Lens' Run (Maybe Int)
runJobTimeoutMinutes = lens _runJobTimeoutMinutes (\ s a -> s{_runJobTimeoutMinutes = a})

-- | The total number of completed jobs.
runCompletedJobs :: Lens' Run (Maybe Int)
runCompletedJobs = lens _runCompletedJobs (\ s a -> s{_runCompletedJobs = a})

-- | Supporting field for the result field. Set only if @result@ is @SKIPPED@ . @PARSING_FAILED@ if the result is skipped because of test package parsing failure.
runResultCode :: Lens' Run (Maybe ExecutionResultCode)
runResultCode = lens _runResultCode (\ s a -> s{_runResultCode = a})

-- | The run's name.
runName :: Lens' Run (Maybe Text)
runName = lens _runName (\ s a -> s{_runName = a})

-- | An app to upload or that has been uploaded.
runAppUpload :: Lens' Run (Maybe Text)
runAppUpload = lens _runAppUpload (\ s a -> s{_runAppUpload = a})

-- | Read-only URL for an object in S3 bucket where you can get the parsing results of the test package. If the test package doesn't parse, the reason why it doesn't parse appears in the file that this URL points to.
runParsingResultURL :: Lens' Run (Maybe Text)
runParsingResultURL = lens _runParsingResultURL (\ s a -> s{_runParsingResultURL = a})

-- | The network profile being used for a test run.
runNetworkProfile :: Lens' Run (Maybe NetworkProfile)
runNetworkProfile = lens _runNetworkProfile (\ s a -> s{_runNetworkProfile = a})

-- | Represents the total (metered or unmetered) minutes used by the test run.
runDeviceMinutes :: Lens' Run (Maybe DeviceMinutes)
runDeviceMinutes = lens _runDeviceMinutes (\ s a -> s{_runDeviceMinutes = a})

-- | The run's type. Must be one of the following values:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_NODE: The Appium Node.js type.     * APPIUM_RUBY: The Appium Ruby type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for web apps.     * APPIUM_WEB_NODE: The Appium Node.js type for web apps.     * APPIUM_WEB_RUBY: The Appium Ruby type for web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
runType :: Lens' Run (Maybe TestType)
runType = lens _runType (\ s a -> s{_runType = a})

-- | A message about the run's result.
runMessage :: Lens' Run (Maybe Text)
runMessage = lens _runMessage (\ s a -> s{_runMessage = a})

-- | The Device Farm console URL for the recording of the run.
runWebURL :: Lens' Run (Maybe Text)
runWebURL = lens _runWebURL (\ s a -> s{_runWebURL = a})

-- | The total number of jobs for the run.
runTotalJobs :: Lens' Run (Maybe Int)
runTotalJobs = lens _runTotalJobs (\ s a -> s{_runTotalJobs = a})

-- | The ARN of the device pool for the run.
runDevicePoolARN :: Lens' Run (Maybe Text)
runDevicePoolARN = lens _runDevicePoolARN (\ s a -> s{_runDevicePoolARN = a})

-- | The run's start time.
runStarted :: Lens' Run (Maybe UTCTime)
runStarted = lens _runStarted (\ s a -> s{_runStarted = a}) . mapping _Time

-- | The results of a device filter used to select the devices for a test run.
runDeviceSelectionResult :: Lens' Run (Maybe DeviceSelectionResult)
runDeviceSelectionResult = lens _runDeviceSelectionResult (\ s a -> s{_runDeviceSelectionResult = a})

instance FromJSON Run where
        parseJSON
          = withObject "Run"
              (\ x ->
                 Run' <$>
                   (x .:? "billingMethod") <*> (x .:? "skipAppResign")
                     <*> (x .:? "status")
                     <*> (x .:? "customerArtifactPaths")
                     <*> (x .:? "eventCount")
                     <*> (x .:? "counters")
                     <*> (x .:? "platform")
                     <*> (x .:? "seed")
                     <*> (x .:? "radios")
                     <*> (x .:? "arn")
                     <*> (x .:? "location")
                     <*> (x .:? "created")
                     <*> (x .:? "locale")
                     <*> (x .:? "testSpecArn")
                     <*> (x .:? "stopped")
                     <*> (x .:? "result")
                     <*> (x .:? "jobTimeoutMinutes")
                     <*> (x .:? "completedJobs")
                     <*> (x .:? "resultCode")
                     <*> (x .:? "name")
                     <*> (x .:? "appUpload")
                     <*> (x .:? "parsingResultUrl")
                     <*> (x .:? "networkProfile")
                     <*> (x .:? "deviceMinutes")
                     <*> (x .:? "type")
                     <*> (x .:? "message")
                     <*> (x .:? "webUrl")
                     <*> (x .:? "totalJobs")
                     <*> (x .:? "devicePoolArn")
                     <*> (x .:? "started")
                     <*> (x .:? "deviceSelectionResult"))

instance Hashable Run where

instance NFData Run where

-- | Represents a sample of performance data.
--
--
--
-- /See:/ 'sample' smart constructor.
data Sample = Sample'
  { _samArn  :: !(Maybe Text)
  , _samUrl  :: !(Maybe Text)
  , _samType :: !(Maybe SampleType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Sample' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'samArn' - The sample's ARN.
--
-- * 'samUrl' - The pre-signed Amazon S3 URL that can be used with a corresponding GET request to download the sample's file.
--
-- * 'samType' - The sample's type. Must be one of the following values:     * CPU: A CPU sample type. This is expressed as the app processing CPU time (including child processes) as reported by process, as a percentage.     * MEMORY: A memory usage sample type. This is expressed as the total proportional set size of an app process, in kilobytes.     * NATIVE_AVG_DRAWTIME     * NATIVE_FPS     * NATIVE_FRAMES     * NATIVE_MAX_DRAWTIME     * NATIVE_MIN_DRAWTIME     * OPENGL_AVG_DRAWTIME     * OPENGL_FPS     * OPENGL_FRAMES     * OPENGL_MAX_DRAWTIME     * OPENGL_MIN_DRAWTIME     * RX     * RX_RATE: The total number of bytes per second (TCP and UDP) that are sent, by app process.     * THREADS: A threads sample type. This is expressed as the total number of threads per app process.     * TX     * TX_RATE: The total number of bytes per second (TCP and UDP) that are received, by app process.
sample
    :: Sample
sample = Sample' {_samArn = Nothing, _samUrl = Nothing, _samType = Nothing}


-- | The sample's ARN.
samArn :: Lens' Sample (Maybe Text)
samArn = lens _samArn (\ s a -> s{_samArn = a})

-- | The pre-signed Amazon S3 URL that can be used with a corresponding GET request to download the sample's file.
samUrl :: Lens' Sample (Maybe Text)
samUrl = lens _samUrl (\ s a -> s{_samUrl = a})

-- | The sample's type. Must be one of the following values:     * CPU: A CPU sample type. This is expressed as the app processing CPU time (including child processes) as reported by process, as a percentage.     * MEMORY: A memory usage sample type. This is expressed as the total proportional set size of an app process, in kilobytes.     * NATIVE_AVG_DRAWTIME     * NATIVE_FPS     * NATIVE_FRAMES     * NATIVE_MAX_DRAWTIME     * NATIVE_MIN_DRAWTIME     * OPENGL_AVG_DRAWTIME     * OPENGL_FPS     * OPENGL_FRAMES     * OPENGL_MAX_DRAWTIME     * OPENGL_MIN_DRAWTIME     * RX     * RX_RATE: The total number of bytes per second (TCP and UDP) that are sent, by app process.     * THREADS: A threads sample type. This is expressed as the total number of threads per app process.     * TX     * TX_RATE: The total number of bytes per second (TCP and UDP) that are received, by app process.
samType :: Lens' Sample (Maybe SampleType)
samType = lens _samType (\ s a -> s{_samType = a})

instance FromJSON Sample where
        parseJSON
          = withObject "Sample"
              (\ x ->
                 Sample' <$>
                   (x .:? "arn") <*> (x .:? "url") <*> (x .:? "type"))

instance Hashable Sample where

instance NFData Sample where

-- | Represents the settings for a run. Includes things like location, radio states, auxiliary apps, and network profiles.
--
--
--
-- /See:/ 'scheduleRunConfiguration' smart constructor.
data ScheduleRunConfiguration = ScheduleRunConfiguration'
  { _srcBillingMethod         :: !(Maybe BillingMethod)
  , _srcCustomerArtifactPaths :: !(Maybe CustomerArtifactPaths)
  , _srcRadios                :: !(Maybe Radios)
  , _srcLocation              :: !(Maybe Location)
  , _srcLocale                :: !(Maybe Text)
  , _srcNetworkProfileARN     :: !(Maybe Text)
  , _srcExtraDataPackageARN   :: !(Maybe Text)
  , _srcAuxiliaryApps         :: !(Maybe [Text])
  , _srcVpceConfigurationARNs :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduleRunConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srcBillingMethod' - Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
--
-- * 'srcCustomerArtifactPaths' - Input @CustomerArtifactPaths@ object for the scheduled run configuration.
--
-- * 'srcRadios' - Information about the radio states for the run.
--
-- * 'srcLocation' - Information about the location that is used for the run.
--
-- * 'srcLocale' - Information about the locale that is used for the run.
--
-- * 'srcNetworkProfileARN' - Reserved for internal use.
--
-- * 'srcExtraDataPackageARN' - The ARN of the extra data for the run. The extra data is a .zip file that AWS Device Farm will extract to external data for Android or the app's sandbox for iOS.
--
-- * 'srcAuxiliaryApps' - A list of auxiliary apps for the run.
--
-- * 'srcVpceConfigurationARNs' - An array of Amazon Resource Names (ARNs) for your VPC endpoint configurations.
scheduleRunConfiguration
    :: ScheduleRunConfiguration
scheduleRunConfiguration =
  ScheduleRunConfiguration'
    { _srcBillingMethod = Nothing
    , _srcCustomerArtifactPaths = Nothing
    , _srcRadios = Nothing
    , _srcLocation = Nothing
    , _srcLocale = Nothing
    , _srcNetworkProfileARN = Nothing
    , _srcExtraDataPackageARN = Nothing
    , _srcAuxiliaryApps = Nothing
    , _srcVpceConfigurationARNs = Nothing
    }


-- | Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
srcBillingMethod :: Lens' ScheduleRunConfiguration (Maybe BillingMethod)
srcBillingMethod = lens _srcBillingMethod (\ s a -> s{_srcBillingMethod = a})

-- | Input @CustomerArtifactPaths@ object for the scheduled run configuration.
srcCustomerArtifactPaths :: Lens' ScheduleRunConfiguration (Maybe CustomerArtifactPaths)
srcCustomerArtifactPaths = lens _srcCustomerArtifactPaths (\ s a -> s{_srcCustomerArtifactPaths = a})

-- | Information about the radio states for the run.
srcRadios :: Lens' ScheduleRunConfiguration (Maybe Radios)
srcRadios = lens _srcRadios (\ s a -> s{_srcRadios = a})

-- | Information about the location that is used for the run.
srcLocation :: Lens' ScheduleRunConfiguration (Maybe Location)
srcLocation = lens _srcLocation (\ s a -> s{_srcLocation = a})

-- | Information about the locale that is used for the run.
srcLocale :: Lens' ScheduleRunConfiguration (Maybe Text)
srcLocale = lens _srcLocale (\ s a -> s{_srcLocale = a})

-- | Reserved for internal use.
srcNetworkProfileARN :: Lens' ScheduleRunConfiguration (Maybe Text)
srcNetworkProfileARN = lens _srcNetworkProfileARN (\ s a -> s{_srcNetworkProfileARN = a})

-- | The ARN of the extra data for the run. The extra data is a .zip file that AWS Device Farm will extract to external data for Android or the app's sandbox for iOS.
srcExtraDataPackageARN :: Lens' ScheduleRunConfiguration (Maybe Text)
srcExtraDataPackageARN = lens _srcExtraDataPackageARN (\ s a -> s{_srcExtraDataPackageARN = a})

-- | A list of auxiliary apps for the run.
srcAuxiliaryApps :: Lens' ScheduleRunConfiguration [Text]
srcAuxiliaryApps = lens _srcAuxiliaryApps (\ s a -> s{_srcAuxiliaryApps = a}) . _Default . _Coerce

-- | An array of Amazon Resource Names (ARNs) for your VPC endpoint configurations.
srcVpceConfigurationARNs :: Lens' ScheduleRunConfiguration [Text]
srcVpceConfigurationARNs = lens _srcVpceConfigurationARNs (\ s a -> s{_srcVpceConfigurationARNs = a}) . _Default . _Coerce

instance Hashable ScheduleRunConfiguration where

instance NFData ScheduleRunConfiguration where

instance ToJSON ScheduleRunConfiguration where
        toJSON ScheduleRunConfiguration'{..}
          = object
              (catMaybes
                 [("billingMethod" .=) <$> _srcBillingMethod,
                  ("customerArtifactPaths" .=) <$>
                    _srcCustomerArtifactPaths,
                  ("radios" .=) <$> _srcRadios,
                  ("location" .=) <$> _srcLocation,
                  ("locale" .=) <$> _srcLocale,
                  ("networkProfileArn" .=) <$> _srcNetworkProfileARN,
                  ("extraDataPackageArn" .=) <$>
                    _srcExtraDataPackageARN,
                  ("auxiliaryApps" .=) <$> _srcAuxiliaryApps,
                  ("vpceConfigurationArns" .=) <$>
                    _srcVpceConfigurationARNs])

-- | Represents test settings. This data structure is passed in as the "test" parameter to ScheduleRun. For an example of the JSON request syntax, see 'ScheduleRun' .
--
--
--
-- /See:/ 'scheduleRunTest' smart constructor.
data ScheduleRunTest = ScheduleRunTest'
  { _srtTestSpecARN    :: !(Maybe Text)
  , _srtTestPackageARN :: !(Maybe Text)
  , _srtParameters     :: !(Maybe (Map Text Text))
  , _srtFilter         :: !(Maybe Text)
  , _srtType           :: !TestType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduleRunTest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srtTestSpecARN' - The ARN of the YAML-formatted test specification.
--
-- * 'srtTestPackageARN' - The ARN of the uploaded test that will be run.
--
-- * 'srtParameters' - The test's parameters, such as test framework parameters and fixture settings. Parameters are represented by name-value pairs of strings. For all tests:     * app_performance_monitoring: Performance monitoring is enabled by default. Set this parameter to "false" to disable it. For Calabash tests:     * profile: A cucumber profile, for example, "my_profile_name".     * tags: You can limit execution to features or scenarios that have (or don't have) certain tags, for example, "@smoke" or "@smoke,~@wip". For Appium tests (all types):     * appium_version: The Appium version. Currently supported values are "1.6.5" (and higher), "latest", and "default".     *
