{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Product where

import           Network.AWS.DeviceFarm.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | A container for account-level settings within AWS Device Farm.
--
--
--
-- /See:/ 'accountSettings' smart constructor.
data AccountSettings = AccountSettings'
    { _asAwsAccountNumber             :: !(Maybe Text)
    , _asMaxJobTimeoutMinutes         :: !(Maybe Int)
    , _asUnmeteredDevices             :: !(Maybe (Map DevicePlatform Int))
    , _asUnmeteredRemoteAccessDevices :: !(Maybe (Map DevicePlatform Int))
    , _asDefaultJobTimeoutMinutes     :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asAwsAccountNumber' - The AWS account number specified in the @AccountSettings@ container.
--
-- * 'asMaxJobTimeoutMinutes' - The maximum number of minutes a test run will execute before it times out.
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
    { _asAwsAccountNumber = Nothing
    , _asMaxJobTimeoutMinutes = Nothing
    , _asUnmeteredDevices = Nothing
    , _asUnmeteredRemoteAccessDevices = Nothing
    , _asDefaultJobTimeoutMinutes = Nothing
    }

-- | The AWS account number specified in the @AccountSettings@ container.
asAwsAccountNumber :: Lens' AccountSettings (Maybe Text)
asAwsAccountNumber = lens _asAwsAccountNumber (\ s a -> s{_asAwsAccountNumber = a});

-- | The maximum number of minutes a test run will execute before it times out.
asMaxJobTimeoutMinutes :: Lens' AccountSettings (Maybe Int)
asMaxJobTimeoutMinutes = lens _asMaxJobTimeoutMinutes (\ s a -> s{_asMaxJobTimeoutMinutes = a});

-- | Returns the unmetered devices you have purchased or want to purchase.
asUnmeteredDevices :: Lens' AccountSettings (HashMap DevicePlatform Int)
asUnmeteredDevices = lens _asUnmeteredDevices (\ s a -> s{_asUnmeteredDevices = a}) . _Default . _Map;

-- | Returns the unmetered remote access devices you have purchased or want to purchase.
asUnmeteredRemoteAccessDevices :: Lens' AccountSettings (HashMap DevicePlatform Int)
asUnmeteredRemoteAccessDevices = lens _asUnmeteredRemoteAccessDevices (\ s a -> s{_asUnmeteredRemoteAccessDevices = a}) . _Default . _Map;

-- | The default number of minutes (at the account level) a test run will execute before it times out. Default value is 60 minutes.
asDefaultJobTimeoutMinutes :: Lens' AccountSettings (Maybe Int)
asDefaultJobTimeoutMinutes = lens _asDefaultJobTimeoutMinutes (\ s a -> s{_asDefaultJobTimeoutMinutes = a});

instance FromJSON AccountSettings where
        parseJSON
          = withObject "AccountSettings"
              (\ x ->
                 AccountSettings' <$>
                   (x .:? "awsAccountNumber") <*>
                     (x .:? "maxJobTimeoutMinutes")
                     <*> (x .:? "unmeteredDevices" .!= mempty)
                     <*> (x .:? "unmeteredRemoteAccessDevices" .!= mempty)
                     <*> (x .:? "defaultJobTimeoutMinutes"))

instance Hashable AccountSettings

instance NFData AccountSettings

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
-- * 'aType' - The artifact's type. Allowed values include the following:     * UNKNOWN: An unknown type.     * SCREENSHOT: The screenshot type.     * DEVICE_LOG: The device log type.     * MESSAGE_LOG: The message log type.     * RESULT_LOG: The result log type.     * SERVICE_LOG: The service log type.     * WEBKIT_LOG: The web kit log type.     * INSTRUMENTATION_OUTPUT: The instrumentation type.     * EXERCISER_MONKEY_OUTPUT: For Android, the artifact (log) generated by an Android fuzz test.     * CALABASH_JSON_OUTPUT: The Calabash JSON output type.     * CALABASH_PRETTY_OUTPUT: The Calabash pretty output type.     * CALABASH_STANDARD_OUTPUT: The Calabash standard output type.     * CALABASH_JAVA_XML_OUTPUT: The Calabash Java XML output type.     * AUTOMATION_OUTPUT: The automation output type.     * APPIUM_SERVER_OUTPUT: The Appium server output type.     * APPIUM_JAVA_OUTPUT: The Appium Java output type.     * APPIUM_JAVA_XML_OUTPUT: The Appium Java XML output type.     * APPIUM_PYTHON_OUTPUT: The Appium Python output type.     * APPIUM_PYTHON_XML_OUTPUT: The Appium Python XML output type.     * EXPLORER_EVENT_LOG: The Explorer event log output type.     * EXPLORER_SUMMARY_LOG: The Explorer summary log output type.     * APPLICATION_CRASH_REPORT: The application crash report output type.     * XCTEST_LOG: The XCode test output type.
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
aArn = lens _aArn (\ s a -> s{_aArn = a});

-- | The pre-signed Amazon S3 URL that can be used with a corresponding GET request to download the artifact's file.
aUrl :: Lens' Artifact (Maybe Text)
aUrl = lens _aUrl (\ s a -> s{_aUrl = a});

-- | The artifact's file extension.
aExtension :: Lens' Artifact (Maybe Text)
aExtension = lens _aExtension (\ s a -> s{_aExtension = a});

-- | The artifact's name.
aName :: Lens' Artifact (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

-- | The artifact's type. Allowed values include the following:     * UNKNOWN: An unknown type.     * SCREENSHOT: The screenshot type.     * DEVICE_LOG: The device log type.     * MESSAGE_LOG: The message log type.     * RESULT_LOG: The result log type.     * SERVICE_LOG: The service log type.     * WEBKIT_LOG: The web kit log type.     * INSTRUMENTATION_OUTPUT: The instrumentation type.     * EXERCISER_MONKEY_OUTPUT: For Android, the artifact (log) generated by an Android fuzz test.     * CALABASH_JSON_OUTPUT: The Calabash JSON output type.     * CALABASH_PRETTY_OUTPUT: The Calabash pretty output type.     * CALABASH_STANDARD_OUTPUT: The Calabash standard output type.     * CALABASH_JAVA_XML_OUTPUT: The Calabash Java XML output type.     * AUTOMATION_OUTPUT: The automation output type.     * APPIUM_SERVER_OUTPUT: The Appium server output type.     * APPIUM_JAVA_OUTPUT: The Appium Java output type.     * APPIUM_JAVA_XML_OUTPUT: The Appium Java XML output type.     * APPIUM_PYTHON_OUTPUT: The Appium Python output type.     * APPIUM_PYTHON_XML_OUTPUT: The Appium Python XML output type.     * EXPLORER_EVENT_LOG: The Explorer event log output type.     * EXPLORER_SUMMARY_LOG: The Explorer summary log output type.     * APPLICATION_CRASH_REPORT: The application crash report output type.     * XCTEST_LOG: The XCode test output type.
aType :: Lens' Artifact (Maybe ArtifactType)
aType = lens _aType (\ s a -> s{_aType = a});

instance FromJSON Artifact where
        parseJSON
          = withObject "Artifact"
              (\ x ->
                 Artifact' <$>
                   (x .:? "arn") <*> (x .:? "url") <*>
                     (x .:? "extension")
                     <*> (x .:? "name")
                     <*> (x .:? "type"))

instance Hashable Artifact

instance NFData Artifact

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _cpuFrequency = Nothing
    , _cpuClock = Nothing
    , _cpuArchitecture = Nothing
    }

-- | The CPU's frequency.
cpuFrequency :: Lens' CPU (Maybe Text)
cpuFrequency = lens _cpuFrequency (\ s a -> s{_cpuFrequency = a});

-- | The clock speed of the device's CPU, expressed in hertz (Hz). For example, a 1.2 GHz CPU is expressed as 1200000000.
cpuClock :: Lens' CPU (Maybe Double)
cpuClock = lens _cpuClock (\ s a -> s{_cpuClock = a});

-- | The CPU's architecture, for example x86 or ARM.
cpuArchitecture :: Lens' CPU (Maybe Text)
cpuArchitecture = lens _cpuArchitecture (\ s a -> s{_cpuArchitecture = a});

instance FromJSON CPU where
        parseJSON
          = withObject "CPU"
              (\ x ->
                 CPU' <$>
                   (x .:? "frequency") <*> (x .:? "clock") <*>
                     (x .:? "architecture"))

instance Hashable CPU

instance NFData CPU

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
cPassed = lens _cPassed (\ s a -> s{_cPassed = a});

-- | The number of skipped entities.
cSkipped :: Lens' Counters (Maybe Int)
cSkipped = lens _cSkipped (\ s a -> s{_cSkipped = a});

-- | The number of warned entities.
cWarned :: Lens' Counters (Maybe Int)
cWarned = lens _cWarned (\ s a -> s{_cWarned = a});

-- | The number of stopped entities.
cStopped :: Lens' Counters (Maybe Int)
cStopped = lens _cStopped (\ s a -> s{_cStopped = a});

-- | The total number of entities.
cTotal :: Lens' Counters (Maybe Int)
cTotal = lens _cTotal (\ s a -> s{_cTotal = a});

-- | The number of failed entities.
cFailed :: Lens' Counters (Maybe Int)
cFailed = lens _cFailed (\ s a -> s{_cFailed = a});

-- | The number of errored entities.
cErrored :: Lens' Counters (Maybe Int)
cErrored = lens _cErrored (\ s a -> s{_cErrored = a});

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

instance Hashable Counters

instance NFData Counters

-- | Creates the configuration settings for a remote access session, including the device model and type.
--
--
--
-- /See:/ 'createRemoteAccessSessionConfiguration' smart constructor.
newtype CreateRemoteAccessSessionConfiguration = CreateRemoteAccessSessionConfiguration'
    { _crascBillingMethod :: Maybe BillingMethod
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRemoteAccessSessionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crascBillingMethod' - Returns the billing method for purposes of configuring a remote access session.
createRemoteAccessSessionConfiguration
    :: CreateRemoteAccessSessionConfiguration
createRemoteAccessSessionConfiguration =
    CreateRemoteAccessSessionConfiguration'
    { _crascBillingMethod = Nothing
    }

-- | Returns the billing method for purposes of configuring a remote access session.
crascBillingMethod :: Lens' CreateRemoteAccessSessionConfiguration (Maybe BillingMethod)
crascBillingMethod = lens _crascBillingMethod (\ s a -> s{_crascBillingMethod = a});

instance Hashable
         CreateRemoteAccessSessionConfiguration

instance NFData
         CreateRemoteAccessSessionConfiguration

instance ToJSON
         CreateRemoteAccessSessionConfiguration where
        toJSON CreateRemoteAccessSessionConfiguration'{..}
          = object
              (catMaybes
                 [("billingMethod" .=) <$> _crascBillingMethod])

-- | Represents a device type that an app is tested against.
--
--
--
-- /See:/ 'device' smart constructor.
data Device = Device'
    { _devCarrier             :: !(Maybe Text)
    , _devImage               :: !(Maybe Text)
    , _devManufacturer        :: !(Maybe Text)
    , _devPlatform            :: !(Maybe DevicePlatform)
    , _devRemoteAccessEnabled :: !(Maybe Bool)
    , _devArn                 :: !(Maybe Text)
    , _devFormFactor          :: !(Maybe DeviceFormFactor)
    , _devFleetType           :: !(Maybe Text)
    , _devResolution          :: !(Maybe Resolution)
    , _devMemory              :: !(Maybe Integer)
    , _devRadio               :: !(Maybe Text)
    , _devOs                  :: !(Maybe Text)
    , _devName                :: !(Maybe Text)
    , _devModel               :: !(Maybe Text)
    , _devCpu                 :: !(Maybe CPU)
    , _devHeapSize            :: !(Maybe Integer)
    , _devFleetName           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'devCarrier' - The device's carrier.
--
-- * 'devImage' - The device's image name.
--
-- * 'devManufacturer' - The device's manufacturer name.
--
-- * 'devPlatform' - The device's platform. Allowed values include:     * ANDROID: The Android platform.     * IOS: The iOS platform.
--
-- * 'devRemoteAccessEnabled' - Specifies whether remote access has been enabled for the specified device.
--
-- * 'devArn' - The device's ARN.
--
-- * 'devFormFactor' - The device's form factor. Allowed values include:     * PHONE: The phone form factor.     * TABLET: The tablet form factor.
--
-- * 'devFleetType' - The type of fleet to which this device belongs. Possible values for fleet type are PRIVATE and PUBLIC.
--
-- * 'devResolution' - The resolution of the device.
--
-- * 'devMemory' - The device's total memory size, expressed in bytes.
--
-- * 'devRadio' - The device's radio.
--
-- * 'devOs' - The device's operating system type.
--
-- * 'devName' - The device's display name.
--
-- * 'devModel' - The device's model name.
--
-- * 'devCpu' - Information about the device's CPU.
--
-- * 'devHeapSize' - The device's heap size, expressed in bytes.
--
-- * 'devFleetName' - The name of the fleet to which this device belongs.
device
    :: Device
device =
    Device'
    { _devCarrier = Nothing
    , _devImage = Nothing
    , _devManufacturer = Nothing
    , _devPlatform = Nothing
    , _devRemoteAccessEnabled = Nothing
    , _devArn = Nothing
    , _devFormFactor = Nothing
    , _devFleetType = Nothing
    , _devResolution = Nothing
    , _devMemory = Nothing
    , _devRadio = Nothing
    , _devOs = Nothing
    , _devName = Nothing
    , _devModel = Nothing
    , _devCpu = Nothing
    , _devHeapSize = Nothing
    , _devFleetName = Nothing
    }

-- | The device's carrier.
devCarrier :: Lens' Device (Maybe Text)
devCarrier = lens _devCarrier (\ s a -> s{_devCarrier = a});

-- | The device's image name.
devImage :: Lens' Device (Maybe Text)
devImage = lens _devImage (\ s a -> s{_devImage = a});

-- | The device's manufacturer name.
devManufacturer :: Lens' Device (Maybe Text)
devManufacturer = lens _devManufacturer (\ s a -> s{_devManufacturer = a});

-- | The device's platform. Allowed values include:     * ANDROID: The Android platform.     * IOS: The iOS platform.
devPlatform :: Lens' Device (Maybe DevicePlatform)
devPlatform = lens _devPlatform (\ s a -> s{_devPlatform = a});

-- | Specifies whether remote access has been enabled for the specified device.
devRemoteAccessEnabled :: Lens' Device (Maybe Bool)
devRemoteAccessEnabled = lens _devRemoteAccessEnabled (\ s a -> s{_devRemoteAccessEnabled = a});

-- | The device's ARN.
devArn :: Lens' Device (Maybe Text)
devArn = lens _devArn (\ s a -> s{_devArn = a});

-- | The device's form factor. Allowed values include:     * PHONE: The phone form factor.     * TABLET: The tablet form factor.
devFormFactor :: Lens' Device (Maybe DeviceFormFactor)
devFormFactor = lens _devFormFactor (\ s a -> s{_devFormFactor = a});

-- | The type of fleet to which this device belongs. Possible values for fleet type are PRIVATE and PUBLIC.
devFleetType :: Lens' Device (Maybe Text)
devFleetType = lens _devFleetType (\ s a -> s{_devFleetType = a});

-- | The resolution of the device.
devResolution :: Lens' Device (Maybe Resolution)
devResolution = lens _devResolution (\ s a -> s{_devResolution = a});

-- | The device's total memory size, expressed in bytes.
devMemory :: Lens' Device (Maybe Integer)
devMemory = lens _devMemory (\ s a -> s{_devMemory = a});

-- | The device's radio.
devRadio :: Lens' Device (Maybe Text)
devRadio = lens _devRadio (\ s a -> s{_devRadio = a});

-- | The device's operating system type.
devOs :: Lens' Device (Maybe Text)
devOs = lens _devOs (\ s a -> s{_devOs = a});

-- | The device's display name.
devName :: Lens' Device (Maybe Text)
devName = lens _devName (\ s a -> s{_devName = a});

-- | The device's model name.
devModel :: Lens' Device (Maybe Text)
devModel = lens _devModel (\ s a -> s{_devModel = a});

-- | Information about the device's CPU.
devCpu :: Lens' Device (Maybe CPU)
devCpu = lens _devCpu (\ s a -> s{_devCpu = a});

-- | The device's heap size, expressed in bytes.
devHeapSize :: Lens' Device (Maybe Integer)
devHeapSize = lens _devHeapSize (\ s a -> s{_devHeapSize = a});

-- | The name of the fleet to which this device belongs.
devFleetName :: Lens' Device (Maybe Text)
devFleetName = lens _devFleetName (\ s a -> s{_devFleetName = a});

instance FromJSON Device where
        parseJSON
          = withObject "Device"
              (\ x ->
                 Device' <$>
                   (x .:? "carrier") <*> (x .:? "image") <*>
                     (x .:? "manufacturer")
                     <*> (x .:? "platform")
                     <*> (x .:? "remoteAccessEnabled")
                     <*> (x .:? "arn")
                     <*> (x .:? "formFactor")
                     <*> (x .:? "fleetType")
                     <*> (x .:? "resolution")
                     <*> (x .:? "memory")
                     <*> (x .:? "radio")
                     <*> (x .:? "os")
                     <*> (x .:? "name")
                     <*> (x .:? "model")
                     <*> (x .:? "cpu")
                     <*> (x .:? "heapSize")
                     <*> (x .:? "fleetName"))

instance Hashable Device

instance NFData Device

-- | Represents the total (metered or unmetered) minutes used by the resource to run tests. Contains the sum of minutes consumed by all children.
--
--
--
-- /See:/ 'deviceMinutes' smart constructor.
data DeviceMinutes = DeviceMinutes'
    { _dmMetered   :: !(Maybe Double)
    , _dmTotal     :: !(Maybe Double)
    , _dmUnmetered :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _dmMetered = Nothing
    , _dmTotal = Nothing
    , _dmUnmetered = Nothing
    }

-- | When specified, represents only the sum of metered minutes used by the resource to run tests.
dmMetered :: Lens' DeviceMinutes (Maybe Double)
dmMetered = lens _dmMetered (\ s a -> s{_dmMetered = a});

-- | When specified, represents the total minutes used by the resource to run tests.
dmTotal :: Lens' DeviceMinutes (Maybe Double)
dmTotal = lens _dmTotal (\ s a -> s{_dmTotal = a});

-- | When specified, represents only the sum of unmetered minutes used by the resource to run tests.
dmUnmetered :: Lens' DeviceMinutes (Maybe Double)
dmUnmetered = lens _dmUnmetered (\ s a -> s{_dmUnmetered = a});

instance FromJSON DeviceMinutes where
        parseJSON
          = withObject "DeviceMinutes"
              (\ x ->
                 DeviceMinutes' <$>
                   (x .:? "metered") <*> (x .:? "total") <*>
                     (x .:? "unmetered"))

instance Hashable DeviceMinutes

instance NFData DeviceMinutes

-- | Represents a collection of device types.
--
--
--
-- /See:/ 'devicePool' smart constructor.
data DevicePool = DevicePool'
    { _dArn         :: !(Maybe Text)
    , _dRules       :: !(Maybe [Rule])
    , _dName        :: !(Maybe Text)
    , _dType        :: !(Maybe DevicePoolType)
    , _dDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DevicePool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dArn' - The device pool's ARN.
--
-- * 'dRules' - Information about the device pool's rules.
--
-- * 'dName' - The device pool's name.
--
-- * 'dType' - The device pool's type. Allowed values include:     * CURATED: A device pool that is created and managed by AWS Device Farm.     * PRIVATE: A device pool that is created and managed by the device pool developer.
--
-- * 'dDescription' - The device pool's description.
devicePool
    :: DevicePool
devicePool =
    DevicePool'
    { _dArn = Nothing
    , _dRules = Nothing
    , _dName = Nothing
    , _dType = Nothing
    , _dDescription = Nothing
    }

-- | The device pool's ARN.
dArn :: Lens' DevicePool (Maybe Text)
dArn = lens _dArn (\ s a -> s{_dArn = a});

-- | Information about the device pool's rules.
dRules :: Lens' DevicePool [Rule]
dRules = lens _dRules (\ s a -> s{_dRules = a}) . _Default . _Coerce;

-- | The device pool's name.
dName :: Lens' DevicePool (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a});

-- | The device pool's type. Allowed values include:     * CURATED: A device pool that is created and managed by AWS Device Farm.     * PRIVATE: A device pool that is created and managed by the device pool developer.
dType :: Lens' DevicePool (Maybe DevicePoolType)
dType = lens _dType (\ s a -> s{_dType = a});

-- | The device pool's description.
dDescription :: Lens' DevicePool (Maybe Text)
dDescription = lens _dDescription (\ s a -> s{_dDescription = a});

instance FromJSON DevicePool where
        parseJSON
          = withObject "DevicePool"
              (\ x ->
                 DevicePool' <$>
                   (x .:? "arn") <*> (x .:? "rules" .!= mempty) <*>
                     (x .:? "name")
                     <*> (x .:? "type")
                     <*> (x .:? "description"))

instance Hashable DevicePool

instance NFData DevicePool

-- | Represents a device pool compatibility result.
--
--
--
-- /See:/ 'devicePoolCompatibilityResult' smart constructor.
data DevicePoolCompatibilityResult = DevicePoolCompatibilityResult'
    { _dpcrDevice                  :: !(Maybe Device)
    , _dpcrCompatible              :: !(Maybe Bool)
    , _dpcrIncompatibilityMessages :: !(Maybe [IncompatibilityMessage])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
dpcrDevice = lens _dpcrDevice (\ s a -> s{_dpcrDevice = a});

-- | Whether the result was compatible with the device pool.
dpcrCompatible :: Lens' DevicePoolCompatibilityResult (Maybe Bool)
dpcrCompatible = lens _dpcrCompatible (\ s a -> s{_dpcrCompatible = a});

-- | Information about the compatibility.
dpcrIncompatibilityMessages :: Lens' DevicePoolCompatibilityResult [IncompatibilityMessage]
dpcrIncompatibilityMessages = lens _dpcrIncompatibilityMessages (\ s a -> s{_dpcrIncompatibilityMessages = a}) . _Default . _Coerce;

instance FromJSON DevicePoolCompatibilityResult where
        parseJSON
          = withObject "DevicePoolCompatibilityResult"
              (\ x ->
                 DevicePoolCompatibilityResult' <$>
                   (x .:? "device") <*> (x .:? "compatible") <*>
                     (x .:? "incompatibilityMessages" .!= mempty))

instance Hashable DevicePoolCompatibilityResult

instance NFData DevicePoolCompatibilityResult

-- | Represents configuration information about a test run, such as the execution timeout (in minutes).
--
--
--
-- /See:/ 'executionConfiguration' smart constructor.
newtype ExecutionConfiguration = ExecutionConfiguration'
    { _ecJobTimeoutMinutes :: Maybe Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExecutionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecJobTimeoutMinutes' - The number of minutes a test run will execute before it times out.
executionConfiguration
    :: ExecutionConfiguration
executionConfiguration =
    ExecutionConfiguration'
    { _ecJobTimeoutMinutes = Nothing
    }

-- | The number of minutes a test run will execute before it times out.
ecJobTimeoutMinutes :: Lens' ExecutionConfiguration (Maybe Int)
ecJobTimeoutMinutes = lens _ecJobTimeoutMinutes (\ s a -> s{_ecJobTimeoutMinutes = a});

instance Hashable ExecutionConfiguration

instance NFData ExecutionConfiguration

instance ToJSON ExecutionConfiguration where
        toJSON ExecutionConfiguration'{..}
          = object
              (catMaybes
                 [("jobTimeoutMinutes" .=) <$> _ecJobTimeoutMinutes])

-- | Represents information about incompatibility.
--
--
--
-- /See:/ 'incompatibilityMessage' smart constructor.
data IncompatibilityMessage = IncompatibilityMessage'
    { _imType    :: !(Maybe DeviceAttribute)
    , _imMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IncompatibilityMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imType' - The type of incompatibility. Allowed values include:     * ARN: The ARN.     * FORM_FACTOR: The form factor (for example, phone or tablet).     * MANUFACTURER: The manufacturer.     * PLATFORM: The platform (for example, Android or iOS).
--
-- * 'imMessage' - A message about the incompatibility.
incompatibilityMessage
    :: IncompatibilityMessage
incompatibilityMessage =
    IncompatibilityMessage'
    { _imType = Nothing
    , _imMessage = Nothing
    }

-- | The type of incompatibility. Allowed values include:     * ARN: The ARN.     * FORM_FACTOR: The form factor (for example, phone or tablet).     * MANUFACTURER: The manufacturer.     * PLATFORM: The platform (for example, Android or iOS).
imType :: Lens' IncompatibilityMessage (Maybe DeviceAttribute)
imType = lens _imType (\ s a -> s{_imType = a});

-- | A message about the incompatibility.
imMessage :: Lens' IncompatibilityMessage (Maybe Text)
imMessage = lens _imMessage (\ s a -> s{_imMessage = a});

instance FromJSON IncompatibilityMessage where
        parseJSON
          = withObject "IncompatibilityMessage"
              (\ x ->
                 IncompatibilityMessage' <$>
                   (x .:? "type") <*> (x .:? "message"))

instance Hashable IncompatibilityMessage

instance NFData IncompatibilityMessage

-- | Represents a device.
--
--
--
-- /See:/ 'job' smart constructor.
data Job = Job'
    { _jobStatus        :: !(Maybe ExecutionStatus)
    , _jobCounters      :: !(Maybe Counters)
    , _jobArn           :: !(Maybe Text)
    , _jobCreated       :: !(Maybe POSIX)
    , _jobDevice        :: !(Maybe Device)
    , _jobStopped       :: !(Maybe POSIX)
    , _jobResult        :: !(Maybe ExecutionResult)
    , _jobName          :: !(Maybe Text)
    , _jobDeviceMinutes :: !(Maybe DeviceMinutes)
    , _jobType          :: !(Maybe TestType)
    , _jobMessage       :: !(Maybe Text)
    , _jobStarted       :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
-- * 'jobDeviceMinutes' - Represents the total (metered or unmetered) minutes used by the job.
--
-- * 'jobType' - The job's type. Allowed values include the following:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for Web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for Web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for Web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
--
-- * 'jobMessage' - A message about the job's result.
--
-- * 'jobStarted' - The job's start time.
job
    :: Job
job =
    Job'
    { _jobStatus = Nothing
    , _jobCounters = Nothing
    , _jobArn = Nothing
    , _jobCreated = Nothing
    , _jobDevice = Nothing
    , _jobStopped = Nothing
    , _jobResult = Nothing
    , _jobName = Nothing
    , _jobDeviceMinutes = Nothing
    , _jobType = Nothing
    , _jobMessage = Nothing
    , _jobStarted = Nothing
    }

-- | The job's status. Allowed values include:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
jobStatus :: Lens' Job (Maybe ExecutionStatus)
jobStatus = lens _jobStatus (\ s a -> s{_jobStatus = a});

-- | The job's result counters.
jobCounters :: Lens' Job (Maybe Counters)
jobCounters = lens _jobCounters (\ s a -> s{_jobCounters = a});

-- | The job's ARN.
jobArn :: Lens' Job (Maybe Text)
jobArn = lens _jobArn (\ s a -> s{_jobArn = a});

-- | When the job was created.
jobCreated :: Lens' Job (Maybe UTCTime)
jobCreated = lens _jobCreated (\ s a -> s{_jobCreated = a}) . mapping _Time;

-- | The device (phone or tablet).
jobDevice :: Lens' Job (Maybe Device)
jobDevice = lens _jobDevice (\ s a -> s{_jobDevice = a});

-- | The job's stop time.
jobStopped :: Lens' Job (Maybe UTCTime)
jobStopped = lens _jobStopped (\ s a -> s{_jobStopped = a}) . mapping _Time;

-- | The job's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
jobResult :: Lens' Job (Maybe ExecutionResult)
jobResult = lens _jobResult (\ s a -> s{_jobResult = a});

-- | The job's name.
jobName :: Lens' Job (Maybe Text)
jobName = lens _jobName (\ s a -> s{_jobName = a});

-- | Represents the total (metered or unmetered) minutes used by the job.
jobDeviceMinutes :: Lens' Job (Maybe DeviceMinutes)
jobDeviceMinutes = lens _jobDeviceMinutes (\ s a -> s{_jobDeviceMinutes = a});

-- | The job's type. Allowed values include the following:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for Web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for Web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for Web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
jobType :: Lens' Job (Maybe TestType)
jobType = lens _jobType (\ s a -> s{_jobType = a});

-- | A message about the job's result.
jobMessage :: Lens' Job (Maybe Text)
jobMessage = lens _jobMessage (\ s a -> s{_jobMessage = a});

-- | The job's start time.
jobStarted :: Lens' Job (Maybe UTCTime)
jobStarted = lens _jobStarted (\ s a -> s{_jobStarted = a}) . mapping _Time;

instance FromJSON Job where
        parseJSON
          = withObject "Job"
              (\ x ->
                 Job' <$>
                   (x .:? "status") <*> (x .:? "counters") <*>
                     (x .:? "arn")
                     <*> (x .:? "created")
                     <*> (x .:? "device")
                     <*> (x .:? "stopped")
                     <*> (x .:? "result")
                     <*> (x .:? "name")
                     <*> (x .:? "deviceMinutes")
                     <*> (x .:? "type")
                     <*> (x .:? "message")
                     <*> (x .:? "started"))

instance Hashable Job

instance NFData Job

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    Location'
    { _lLatitude = pLatitude_
    , _lLongitude = pLongitude_
    }

-- | The latitude.
lLatitude :: Lens' Location Double
lLatitude = lens _lLatitude (\ s a -> s{_lLatitude = a});

-- | The longitude.
lLongitude :: Lens' Location Double
lLongitude = lens _lLongitude (\ s a -> s{_lLongitude = a});

instance Hashable Location

instance NFData Location

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    MonetaryAmount'
    { _maAmount = Nothing
    , _maCurrencyCode = Nothing
    }

-- | The numerical amount of an offering or transaction.
maAmount :: Lens' MonetaryAmount (Maybe Double)
maAmount = lens _maAmount (\ s a -> s{_maAmount = a});

-- | The currency code of a monetary amount. For example, @USD@ means "U.S. dollars."
maCurrencyCode :: Lens' MonetaryAmount (Maybe CurrencyCode)
maCurrencyCode = lens _maCurrencyCode (\ s a -> s{_maCurrencyCode = a});

instance FromJSON MonetaryAmount where
        parseJSON
          = withObject "MonetaryAmount"
              (\ x ->
                 MonetaryAmount' <$>
                   (x .:? "amount") <*> (x .:? "currencyCode"))

instance Hashable MonetaryAmount

instance NFData MonetaryAmount

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
npUplinkJitterMs = lens _npUplinkJitterMs (\ s a -> s{_npUplinkJitterMs = a});

-- | The Amazon Resource Name (ARN) of the network profile.
npArn :: Lens' NetworkProfile (Maybe Text)
npArn = lens _npArn (\ s a -> s{_npArn = a});

-- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
npUplinkLossPercent :: Lens' NetworkProfile (Maybe Natural)
npUplinkLossPercent = lens _npUplinkLossPercent (\ s a -> s{_npUplinkLossPercent = a}) . mapping _Nat;

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
npDownlinkJitterMs :: Lens' NetworkProfile (Maybe Integer)
npDownlinkJitterMs = lens _npDownlinkJitterMs (\ s a -> s{_npDownlinkJitterMs = a});

-- | The name of the network profile.
npName :: Lens' NetworkProfile (Maybe Text)
npName = lens _npName (\ s a -> s{_npName = a});

-- | Proportion of received packets that fail to arrive from 0 to 100 percent.
npDownlinkLossPercent :: Lens' NetworkProfile (Maybe Natural)
npDownlinkLossPercent = lens _npDownlinkLossPercent (\ s a -> s{_npDownlinkLossPercent = a}) . mapping _Nat;

-- | The type of network profile. Valid values are listed below.
npType :: Lens' NetworkProfile (Maybe NetworkProfileType)
npType = lens _npType (\ s a -> s{_npType = a});

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
npUplinkDelayMs :: Lens' NetworkProfile (Maybe Integer)
npUplinkDelayMs = lens _npUplinkDelayMs (\ s a -> s{_npUplinkDelayMs = a});

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
npUplinkBandwidthBits :: Lens' NetworkProfile (Maybe Integer)
npUplinkBandwidthBits = lens _npUplinkBandwidthBits (\ s a -> s{_npUplinkBandwidthBits = a});

-- | The description of the network profile.
npDescription :: Lens' NetworkProfile (Maybe Text)
npDescription = lens _npDescription (\ s a -> s{_npDescription = a});

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
npDownlinkDelayMs :: Lens' NetworkProfile (Maybe Integer)
npDownlinkDelayMs = lens _npDownlinkDelayMs (\ s a -> s{_npDownlinkDelayMs = a});

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
npDownlinkBandwidthBits :: Lens' NetworkProfile (Maybe Integer)
npDownlinkBandwidthBits = lens _npDownlinkBandwidthBits (\ s a -> s{_npDownlinkBandwidthBits = a});

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

instance Hashable NetworkProfile

instance NFData NetworkProfile

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
oPlatform = lens _oPlatform (\ s a -> s{_oPlatform = a});

-- | The ID that corresponds to a device offering.
oId :: Lens' Offering (Maybe Text)
oId = lens _oId (\ s a -> s{_oId = a});

-- | Specifies whether there are recurring charges for the offering.
oRecurringCharges :: Lens' Offering [RecurringCharge]
oRecurringCharges = lens _oRecurringCharges (\ s a -> s{_oRecurringCharges = a}) . _Default . _Coerce;

-- | The type of offering (e.g., "RECURRING") for a device.
oType :: Lens' Offering (Maybe OfferingType)
oType = lens _oType (\ s a -> s{_oType = a});

-- | A string describing the offering.
oDescription :: Lens' Offering (Maybe Text)
oDescription = lens _oDescription (\ s a -> s{_oDescription = a});

instance FromJSON Offering where
        parseJSON
          = withObject "Offering"
              (\ x ->
                 Offering' <$>
                   (x .:? "platform") <*> (x .:? "id") <*>
                     (x .:? "recurringCharges" .!= mempty)
                     <*> (x .:? "type")
                     <*> (x .:? "description"))

instance Hashable Offering

instance NFData Offering

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
osEffectiveOn = lens _osEffectiveOn (\ s a -> s{_osEffectiveOn = a}) . mapping _Time;

-- | Represents the metadata of an offering status.
osOffering :: Lens' OfferingStatus (Maybe Offering)
osOffering = lens _osOffering (\ s a -> s{_osOffering = a});

-- | The number of available devices in the offering.
osQuantity :: Lens' OfferingStatus (Maybe Int)
osQuantity = lens _osQuantity (\ s a -> s{_osQuantity = a});

-- | The type specified for the offering status.
osType :: Lens' OfferingStatus (Maybe OfferingTransactionType)
osType = lens _osType (\ s a -> s{_osType = a});

instance FromJSON OfferingStatus where
        parseJSON
          = withObject "OfferingStatus"
              (\ x ->
                 OfferingStatus' <$>
                   (x .:? "effectiveOn") <*> (x .:? "offering") <*>
                     (x .:? "quantity")
                     <*> (x .:? "type"))

instance Hashable OfferingStatus

instance NFData OfferingStatus

-- | Represents the metadata of an offering transaction.
--
--
--
-- /See:/ 'offeringTransaction' smart constructor.
data OfferingTransaction = OfferingTransaction'
    { _otOfferingStatus :: !(Maybe OfferingStatus)
    , _otCost           :: !(Maybe MonetaryAmount)
    , _otTransactionId  :: !(Maybe Text)
    , _otCreatedOn      :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
-- * 'otCreatedOn' - The date on which an offering transaction was created.
offeringTransaction
    :: OfferingTransaction
offeringTransaction =
    OfferingTransaction'
    { _otOfferingStatus = Nothing
    , _otCost = Nothing
    , _otTransactionId = Nothing
    , _otCreatedOn = Nothing
    }

-- | The status of an offering transaction.
otOfferingStatus :: Lens' OfferingTransaction (Maybe OfferingStatus)
otOfferingStatus = lens _otOfferingStatus (\ s a -> s{_otOfferingStatus = a});

-- | The cost of an offering transaction.
otCost :: Lens' OfferingTransaction (Maybe MonetaryAmount)
otCost = lens _otCost (\ s a -> s{_otCost = a});

-- | The transaction ID of the offering transaction.
otTransactionId :: Lens' OfferingTransaction (Maybe Text)
otTransactionId = lens _otTransactionId (\ s a -> s{_otTransactionId = a});

-- | The date on which an offering transaction was created.
otCreatedOn :: Lens' OfferingTransaction (Maybe UTCTime)
otCreatedOn = lens _otCreatedOn (\ s a -> s{_otCreatedOn = a}) . mapping _Time;

instance FromJSON OfferingTransaction where
        parseJSON
          = withObject "OfferingTransaction"
              (\ x ->
                 OfferingTransaction' <$>
                   (x .:? "offeringStatus") <*> (x .:? "cost") <*>
                     (x .:? "transactionId")
                     <*> (x .:? "createdOn"))

instance Hashable OfferingTransaction

instance NFData OfferingTransaction

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
pDevice = lens _pDevice (\ s a -> s{_pDevice = a});

-- | Information about the associated test.
pTest :: Lens' Problem (Maybe ProblemDetail)
pTest = lens _pTest (\ s a -> s{_pTest = a});

-- | The problem's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
pResult :: Lens' Problem (Maybe ExecutionResult)
pResult = lens _pResult (\ s a -> s{_pResult = a});

-- | Information about the associated run.
pRun :: Lens' Problem (Maybe ProblemDetail)
pRun = lens _pRun (\ s a -> s{_pRun = a});

-- | Information about the associated job.
pJob :: Lens' Problem (Maybe ProblemDetail)
pJob = lens _pJob (\ s a -> s{_pJob = a});

-- | A message about the problem's result.
pMessage :: Lens' Problem (Maybe Text)
pMessage = lens _pMessage (\ s a -> s{_pMessage = a});

-- | Information about the associated suite.
pSuite :: Lens' Problem (Maybe ProblemDetail)
pSuite = lens _pSuite (\ s a -> s{_pSuite = a});

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

instance Hashable Problem

instance NFData Problem

-- | Information about a problem detail.
--
--
--
-- /See:/ 'problemDetail' smart constructor.
data ProblemDetail = ProblemDetail'
    { _pdArn  :: !(Maybe Text)
    , _pdName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProblemDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdArn' - The problem detail's ARN.
--
-- * 'pdName' - The problem detail's name.
problemDetail
    :: ProblemDetail
problemDetail =
    ProblemDetail'
    { _pdArn = Nothing
    , _pdName = Nothing
    }

-- | The problem detail's ARN.
pdArn :: Lens' ProblemDetail (Maybe Text)
pdArn = lens _pdArn (\ s a -> s{_pdArn = a});

-- | The problem detail's name.
pdName :: Lens' ProblemDetail (Maybe Text)
pdName = lens _pdName (\ s a -> s{_pdName = a});

instance FromJSON ProblemDetail where
        parseJSON
          = withObject "ProblemDetail"
              (\ x ->
                 ProblemDetail' <$> (x .:? "arn") <*> (x .:? "name"))

instance Hashable ProblemDetail

instance NFData ProblemDetail

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
pArn = lens _pArn (\ s a -> s{_pArn = a});

-- | When the project was created.
pCreated :: Lens' Project (Maybe UTCTime)
pCreated = lens _pCreated (\ s a -> s{_pCreated = a}) . mapping _Time;

-- | The project's name.
pName :: Lens' Project (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a});

-- | The default number of minutes (at the project level) a test run will execute before it times out. Default value is 60 minutes.
pDefaultJobTimeoutMinutes :: Lens' Project (Maybe Int)
pDefaultJobTimeoutMinutes = lens _pDefaultJobTimeoutMinutes (\ s a -> s{_pDefaultJobTimeoutMinutes = a});

instance FromJSON Project where
        parseJSON
          = withObject "Project"
              (\ x ->
                 Project' <$>
                   (x .:? "arn") <*> (x .:? "created") <*>
                     (x .:? "name")
                     <*> (x .:? "defaultJobTimeoutMinutes"))

instance Hashable Project

instance NFData Project

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _rNfc = Nothing
    , _rGps = Nothing
    , _rBluetooth = Nothing
    , _rWifi = Nothing
    }

-- | True if NFC is enabled at the beginning of the test; otherwise, false.
rNfc :: Lens' Radios (Maybe Bool)
rNfc = lens _rNfc (\ s a -> s{_rNfc = a});

-- | True if GPS is enabled at the beginning of the test; otherwise, false.
rGps :: Lens' Radios (Maybe Bool)
rGps = lens _rGps (\ s a -> s{_rGps = a});

-- | True if Bluetooth is enabled at the beginning of the test; otherwise, false.
rBluetooth :: Lens' Radios (Maybe Bool)
rBluetooth = lens _rBluetooth (\ s a -> s{_rBluetooth = a});

-- | True if Wi-Fi is enabled at the beginning of the test; otherwise, false.
rWifi :: Lens' Radios (Maybe Bool)
rWifi = lens _rWifi (\ s a -> s{_rWifi = a});

instance Hashable Radios

instance NFData Radios

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcFrequency' - The frequency in which charges will recur.
--
-- * 'rcCost' - The cost of the recurring charge.
recurringCharge
    :: RecurringCharge
recurringCharge =
    RecurringCharge'
    { _rcFrequency = Nothing
    , _rcCost = Nothing
    }

-- | The frequency in which charges will recur.
rcFrequency :: Lens' RecurringCharge (Maybe RecurringChargeFrequency)
rcFrequency = lens _rcFrequency (\ s a -> s{_rcFrequency = a});

-- | The cost of the recurring charge.
rcCost :: Lens' RecurringCharge (Maybe MonetaryAmount)
rcCost = lens _rcCost (\ s a -> s{_rcCost = a});

instance FromJSON RecurringCharge where
        parseJSON
          = withObject "RecurringCharge"
              (\ x ->
                 RecurringCharge' <$>
                   (x .:? "frequency") <*> (x .:? "cost"))

instance Hashable RecurringCharge

instance NFData RecurringCharge

-- | Represents information about the remote access session.
--
--
--
-- /See:/ 'remoteAccessSession' smart constructor.
data RemoteAccessSession = RemoteAccessSession'
    { _rasBillingMethod :: !(Maybe BillingMethod)
    , _rasStatus        :: !(Maybe ExecutionStatus)
    , _rasArn           :: !(Maybe Text)
    , _rasCreated       :: !(Maybe POSIX)
    , _rasDevice        :: !(Maybe Device)
    , _rasStopped       :: !(Maybe POSIX)
    , _rasResult        :: !(Maybe ExecutionResult)
    , _rasName          :: !(Maybe Text)
    , _rasDeviceMinutes :: !(Maybe DeviceMinutes)
    , _rasEndpoint      :: !(Maybe Text)
    , _rasMessage       :: !(Maybe Text)
    , _rasStarted       :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemoteAccessSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rasBillingMethod' - The billing method of the remote access session. Possible values include @METERED@ or @UNMETERED@ . For more information about metered devices, see <http://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology> ."
--
-- * 'rasStatus' - The status of the remote access session. Can be any of the following:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
--
-- * 'rasArn' - The Amazon Resource Name (ARN) of the remote access session.
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
-- * 'rasEndpoint' - The endpoint for the remote access sesssion.
--
-- * 'rasMessage' - A message about the remote access session.
--
-- * 'rasStarted' - The date and time the remote access session was started.
remoteAccessSession
    :: RemoteAccessSession
remoteAccessSession =
    RemoteAccessSession'
    { _rasBillingMethod = Nothing
    , _rasStatus = Nothing
    , _rasArn = Nothing
    , _rasCreated = Nothing
    , _rasDevice = Nothing
    , _rasStopped = Nothing
    , _rasResult = Nothing
    , _rasName = Nothing
    , _rasDeviceMinutes = Nothing
    , _rasEndpoint = Nothing
    , _rasMessage = Nothing
    , _rasStarted = Nothing
    }

-- | The billing method of the remote access session. Possible values include @METERED@ or @UNMETERED@ . For more information about metered devices, see <http://docs.aws.amazon.com/devicefarm/latest/developerguide/welcome.html#welcome-terminology AWS Device Farm terminology> ."
rasBillingMethod :: Lens' RemoteAccessSession (Maybe BillingMethod)
rasBillingMethod = lens _rasBillingMethod (\ s a -> s{_rasBillingMethod = a});

-- | The status of the remote access session. Can be any of the following:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
rasStatus :: Lens' RemoteAccessSession (Maybe ExecutionStatus)
rasStatus = lens _rasStatus (\ s a -> s{_rasStatus = a});

-- | The Amazon Resource Name (ARN) of the remote access session.
rasArn :: Lens' RemoteAccessSession (Maybe Text)
rasArn = lens _rasArn (\ s a -> s{_rasArn = a});

-- | The date and time the remote access session was created.
rasCreated :: Lens' RemoteAccessSession (Maybe UTCTime)
rasCreated = lens _rasCreated (\ s a -> s{_rasCreated = a}) . mapping _Time;

-- | The device (phone or tablet) used in the remote access session.
rasDevice :: Lens' RemoteAccessSession (Maybe Device)
rasDevice = lens _rasDevice (\ s a -> s{_rasDevice = a});

-- | The date and time the remote access session was stopped.
rasStopped :: Lens' RemoteAccessSession (Maybe UTCTime)
rasStopped = lens _rasStopped (\ s a -> s{_rasStopped = a}) . mapping _Time;

-- | The result of the remote access session. Can be any of the following:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
rasResult :: Lens' RemoteAccessSession (Maybe ExecutionResult)
rasResult = lens _rasResult (\ s a -> s{_rasResult = a});

-- | The name of the remote access session.
rasName :: Lens' RemoteAccessSession (Maybe Text)
rasName = lens _rasName (\ s a -> s{_rasName = a});

-- | The number of minutes a device is used in a remote access sesssion (including setup and teardown minutes).
rasDeviceMinutes :: Lens' RemoteAccessSession (Maybe DeviceMinutes)
rasDeviceMinutes = lens _rasDeviceMinutes (\ s a -> s{_rasDeviceMinutes = a});

-- | The endpoint for the remote access sesssion.
rasEndpoint :: Lens' RemoteAccessSession (Maybe Text)
rasEndpoint = lens _rasEndpoint (\ s a -> s{_rasEndpoint = a});

-- | A message about the remote access session.
rasMessage :: Lens' RemoteAccessSession (Maybe Text)
rasMessage = lens _rasMessage (\ s a -> s{_rasMessage = a});

-- | The date and time the remote access session was started.
rasStarted :: Lens' RemoteAccessSession (Maybe UTCTime)
rasStarted = lens _rasStarted (\ s a -> s{_rasStarted = a}) . mapping _Time;

instance FromJSON RemoteAccessSession where
        parseJSON
          = withObject "RemoteAccessSession"
              (\ x ->
                 RemoteAccessSession' <$>
                   (x .:? "billingMethod") <*> (x .:? "status") <*>
                     (x .:? "arn")
                     <*> (x .:? "created")
                     <*> (x .:? "device")
                     <*> (x .:? "stopped")
                     <*> (x .:? "result")
                     <*> (x .:? "name")
                     <*> (x .:? "deviceMinutes")
                     <*> (x .:? "endpoint")
                     <*> (x .:? "message")
                     <*> (x .:? "started"))

instance Hashable RemoteAccessSession

instance NFData RemoteAccessSession

-- | Represents the screen resolution of a device in height and width, expressed in pixels.
--
--
--
-- /See:/ 'resolution' smart constructor.
data Resolution = Resolution'
    { _rHeight :: !(Maybe Int)
    , _rWidth  :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Resolution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rHeight' - The screen resolution's height, expressed in pixels.
--
-- * 'rWidth' - The screen resolution's width, expressed in pixels.
resolution
    :: Resolution
resolution =
    Resolution'
    { _rHeight = Nothing
    , _rWidth = Nothing
    }

-- | The screen resolution's height, expressed in pixels.
rHeight :: Lens' Resolution (Maybe Int)
rHeight = lens _rHeight (\ s a -> s{_rHeight = a});

-- | The screen resolution's width, expressed in pixels.
rWidth :: Lens' Resolution (Maybe Int)
rWidth = lens _rWidth (\ s a -> s{_rWidth = a});

instance FromJSON Resolution where
        parseJSON
          = withObject "Resolution"
              (\ x ->
                 Resolution' <$> (x .:? "height") <*> (x .:? "width"))

instance Hashable Resolution

instance NFData Resolution

-- | Represents a condition for a device pool.
--
--
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
    { _rAttribute :: !(Maybe DeviceAttribute)
    , _rOperator  :: !(Maybe RuleOperator)
    , _rValue     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rAttribute' - The rule's stringified attribute. For example, specify the value as @"\"abc\""@ . Allowed values include:     * ARN: The ARN.     * FORM_FACTOR: The form factor (for example, phone or tablet).     * MANUFACTURER: The manufacturer.     * PLATFORM: The platform (for example, Android or iOS).
--
-- * 'rOperator' - The rule's operator.     * EQUALS: The equals operator.     * GREATER_THAN: The greater-than operator.     * IN: The in operator.     * LESS_THAN: The less-than operator.     * NOT_IN: The not-in operator.
--
-- * 'rValue' - The rule's value.
rule
    :: Rule
rule =
    Rule'
    { _rAttribute = Nothing
    , _rOperator = Nothing
    , _rValue = Nothing
    }

-- | The rule's stringified attribute. For example, specify the value as @"\"abc\""@ . Allowed values include:     * ARN: The ARN.     * FORM_FACTOR: The form factor (for example, phone or tablet).     * MANUFACTURER: The manufacturer.     * PLATFORM: The platform (for example, Android or iOS).
rAttribute :: Lens' Rule (Maybe DeviceAttribute)
rAttribute = lens _rAttribute (\ s a -> s{_rAttribute = a});

-- | The rule's operator.     * EQUALS: The equals operator.     * GREATER_THAN: The greater-than operator.     * IN: The in operator.     * LESS_THAN: The less-than operator.     * NOT_IN: The not-in operator.
rOperator :: Lens' Rule (Maybe RuleOperator)
rOperator = lens _rOperator (\ s a -> s{_rOperator = a});

-- | The rule's value.
rValue :: Lens' Rule (Maybe Text)
rValue = lens _rValue (\ s a -> s{_rValue = a});

instance FromJSON Rule where
        parseJSON
          = withObject "Rule"
              (\ x ->
                 Rule' <$>
                   (x .:? "attribute") <*> (x .:? "operator") <*>
                     (x .:? "value"))

instance Hashable Rule

instance NFData Rule

instance ToJSON Rule where
        toJSON Rule'{..}
          = object
              (catMaybes
                 [("attribute" .=) <$> _rAttribute,
                  ("operator" .=) <$> _rOperator,
                  ("value" .=) <$> _rValue])

-- | Represents an app on a set of devices with a specific test and configuration.
--
--
--
-- /See:/ 'run' smart constructor.
data Run = Run'
    { _runBillingMethod  :: !(Maybe BillingMethod)
    , _runStatus         :: !(Maybe ExecutionStatus)
    , _runCounters       :: !(Maybe Counters)
    , _runPlatform       :: !(Maybe DevicePlatform)
    , _runArn            :: !(Maybe Text)
    , _runCreated        :: !(Maybe POSIX)
    , _runStopped        :: !(Maybe POSIX)
    , _runResult         :: !(Maybe ExecutionResult)
    , _runCompletedJobs  :: !(Maybe Int)
    , _runName           :: !(Maybe Text)
    , _runNetworkProfile :: !(Maybe NetworkProfile)
    , _runDeviceMinutes  :: !(Maybe DeviceMinutes)
    , _runType           :: !(Maybe TestType)
    , _runMessage        :: !(Maybe Text)
    , _runTotalJobs      :: !(Maybe Int)
    , _runStarted        :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Run' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'runBillingMethod' - Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
--
-- * 'runStatus' - The run's status. Allowed values include:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
--
-- * 'runCounters' - The run's result counters.
--
-- * 'runPlatform' - The run's platform. Allowed values include:     * ANDROID: The Android platform.     * IOS: The iOS platform.
--
-- * 'runArn' - The run's ARN.
--
-- * 'runCreated' - When the run was created.
--
-- * 'runStopped' - The run's stop time.
--
-- * 'runResult' - The run's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
--
-- * 'runCompletedJobs' - The total number of completed jobs.
--
-- * 'runName' - The run's name.
--
-- * 'runNetworkProfile' - The network profile being used for a test run.
--
-- * 'runDeviceMinutes' - Represents the total (metered or unmetered) minutes used by the test run.
--
-- * 'runType' - The run's type. Must be one of the following values:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for Web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for Web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for Web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
--
-- * 'runMessage' - A message about the run's result.
--
-- * 'runTotalJobs' - The total number of jobs for the run.
--
-- * 'runStarted' - The run's start time.
run
    :: Run
run =
    Run'
    { _runBillingMethod = Nothing
    , _runStatus = Nothing
    , _runCounters = Nothing
    , _runPlatform = Nothing
    , _runArn = Nothing
    , _runCreated = Nothing
    , _runStopped = Nothing
    , _runResult = Nothing
    , _runCompletedJobs = Nothing
    , _runName = Nothing
    , _runNetworkProfile = Nothing
    , _runDeviceMinutes = Nothing
    , _runType = Nothing
    , _runMessage = Nothing
    , _runTotalJobs = Nothing
    , _runStarted = Nothing
    }

-- | Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
runBillingMethod :: Lens' Run (Maybe BillingMethod)
runBillingMethod = lens _runBillingMethod (\ s a -> s{_runBillingMethod = a});

-- | The run's status. Allowed values include:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
runStatus :: Lens' Run (Maybe ExecutionStatus)
runStatus = lens _runStatus (\ s a -> s{_runStatus = a});

-- | The run's result counters.
runCounters :: Lens' Run (Maybe Counters)
runCounters = lens _runCounters (\ s a -> s{_runCounters = a});

-- | The run's platform. Allowed values include:     * ANDROID: The Android platform.     * IOS: The iOS platform.
runPlatform :: Lens' Run (Maybe DevicePlatform)
runPlatform = lens _runPlatform (\ s a -> s{_runPlatform = a});

-- | The run's ARN.
runArn :: Lens' Run (Maybe Text)
runArn = lens _runArn (\ s a -> s{_runArn = a});

-- | When the run was created.
runCreated :: Lens' Run (Maybe UTCTime)
runCreated = lens _runCreated (\ s a -> s{_runCreated = a}) . mapping _Time;

-- | The run's stop time.
runStopped :: Lens' Run (Maybe UTCTime)
runStopped = lens _runStopped (\ s a -> s{_runStopped = a}) . mapping _Time;

-- | The run's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
runResult :: Lens' Run (Maybe ExecutionResult)
runResult = lens _runResult (\ s a -> s{_runResult = a});

-- | The total number of completed jobs.
runCompletedJobs :: Lens' Run (Maybe Int)
runCompletedJobs = lens _runCompletedJobs (\ s a -> s{_runCompletedJobs = a});

-- | The run's name.
runName :: Lens' Run (Maybe Text)
runName = lens _runName (\ s a -> s{_runName = a});

-- | The network profile being used for a test run.
runNetworkProfile :: Lens' Run (Maybe NetworkProfile)
runNetworkProfile = lens _runNetworkProfile (\ s a -> s{_runNetworkProfile = a});

-- | Represents the total (metered or unmetered) minutes used by the test run.
runDeviceMinutes :: Lens' Run (Maybe DeviceMinutes)
runDeviceMinutes = lens _runDeviceMinutes (\ s a -> s{_runDeviceMinutes = a});

-- | The run's type. Must be one of the following values:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for Web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for Web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for Web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
runType :: Lens' Run (Maybe TestType)
runType = lens _runType (\ s a -> s{_runType = a});

-- | A message about the run's result.
runMessage :: Lens' Run (Maybe Text)
runMessage = lens _runMessage (\ s a -> s{_runMessage = a});

-- | The total number of jobs for the run.
runTotalJobs :: Lens' Run (Maybe Int)
runTotalJobs = lens _runTotalJobs (\ s a -> s{_runTotalJobs = a});

-- | The run's start time.
runStarted :: Lens' Run (Maybe UTCTime)
runStarted = lens _runStarted (\ s a -> s{_runStarted = a}) . mapping _Time;

instance FromJSON Run where
        parseJSON
          = withObject "Run"
              (\ x ->
                 Run' <$>
                   (x .:? "billingMethod") <*> (x .:? "status") <*>
                     (x .:? "counters")
                     <*> (x .:? "platform")
                     <*> (x .:? "arn")
                     <*> (x .:? "created")
                     <*> (x .:? "stopped")
                     <*> (x .:? "result")
                     <*> (x .:? "completedJobs")
                     <*> (x .:? "name")
                     <*> (x .:? "networkProfile")
                     <*> (x .:? "deviceMinutes")
                     <*> (x .:? "type")
                     <*> (x .:? "message")
                     <*> (x .:? "totalJobs")
                     <*> (x .:? "started"))

instance Hashable Run

instance NFData Run

-- | Represents a sample of performance data.
--
--
--
-- /See:/ 'sample' smart constructor.
data Sample = Sample'
    { _samArn  :: !(Maybe Text)
    , _samUrl  :: !(Maybe Text)
    , _samType :: !(Maybe SampleType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
sample =
    Sample'
    { _samArn = Nothing
    , _samUrl = Nothing
    , _samType = Nothing
    }

-- | The sample's ARN.
samArn :: Lens' Sample (Maybe Text)
samArn = lens _samArn (\ s a -> s{_samArn = a});

-- | The pre-signed Amazon S3 URL that can be used with a corresponding GET request to download the sample's file.
samUrl :: Lens' Sample (Maybe Text)
samUrl = lens _samUrl (\ s a -> s{_samUrl = a});

-- | The sample's type. Must be one of the following values:     * CPU: A CPU sample type. This is expressed as the app processing CPU time (including child processes) as reported by process, as a percentage.     * MEMORY: A memory usage sample type. This is expressed as the total proportional set size of an app process, in kilobytes.     * NATIVE_AVG_DRAWTIME     * NATIVE_FPS     * NATIVE_FRAMES     * NATIVE_MAX_DRAWTIME     * NATIVE_MIN_DRAWTIME     * OPENGL_AVG_DRAWTIME     * OPENGL_FPS     * OPENGL_FRAMES     * OPENGL_MAX_DRAWTIME     * OPENGL_MIN_DRAWTIME     * RX     * RX_RATE: The total number of bytes per second (TCP and UDP) that are sent, by app process.     * THREADS: A threads sample type. This is expressed as the total number of threads per app process.     * TX     * TX_RATE: The total number of bytes per second (TCP and UDP) that are received, by app process.
samType :: Lens' Sample (Maybe SampleType)
samType = lens _samType (\ s a -> s{_samType = a});

instance FromJSON Sample where
        parseJSON
          = withObject "Sample"
              (\ x ->
                 Sample' <$>
                   (x .:? "arn") <*> (x .:? "url") <*> (x .:? "type"))

instance Hashable Sample

instance NFData Sample

-- | Represents the settings for a run. Includes things like location, radio states, auxiliary apps, and network profiles.
--
--
--
-- /See:/ 'scheduleRunConfiguration' smart constructor.
data ScheduleRunConfiguration = ScheduleRunConfiguration'
    { _srcBillingMethod       :: !(Maybe BillingMethod)
    , _srcRadios              :: !(Maybe Radios)
    , _srcLocation            :: !(Maybe Location)
    , _srcLocale              :: !(Maybe Text)
    , _srcNetworkProfileARN   :: !(Maybe Text)
    , _srcExtraDataPackageARN :: !(Maybe Text)
    , _srcAuxiliaryApps       :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScheduleRunConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srcBillingMethod' - Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
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
scheduleRunConfiguration
    :: ScheduleRunConfiguration
scheduleRunConfiguration =
    ScheduleRunConfiguration'
    { _srcBillingMethod = Nothing
    , _srcRadios = Nothing
    , _srcLocation = Nothing
    , _srcLocale = Nothing
    , _srcNetworkProfileARN = Nothing
    , _srcExtraDataPackageARN = Nothing
    , _srcAuxiliaryApps = Nothing
    }

-- | Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
srcBillingMethod :: Lens' ScheduleRunConfiguration (Maybe BillingMethod)
srcBillingMethod = lens _srcBillingMethod (\ s a -> s{_srcBillingMethod = a});

-- | Information about the radio states for the run.
srcRadios :: Lens' ScheduleRunConfiguration (Maybe Radios)
srcRadios = lens _srcRadios (\ s a -> s{_srcRadios = a});

-- | Information about the location that is used for the run.
srcLocation :: Lens' ScheduleRunConfiguration (Maybe Location)
srcLocation = lens _srcLocation (\ s a -> s{_srcLocation = a});

-- | Information about the locale that is used for the run.
srcLocale :: Lens' ScheduleRunConfiguration (Maybe Text)
srcLocale = lens _srcLocale (\ s a -> s{_srcLocale = a});

-- | Reserved for internal use.
srcNetworkProfileARN :: Lens' ScheduleRunConfiguration (Maybe Text)
srcNetworkProfileARN = lens _srcNetworkProfileARN (\ s a -> s{_srcNetworkProfileARN = a});

-- | The ARN of the extra data for the run. The extra data is a .zip file that AWS Device Farm will extract to external data for Android or the app's sandbox for iOS.
srcExtraDataPackageARN :: Lens' ScheduleRunConfiguration (Maybe Text)
srcExtraDataPackageARN = lens _srcExtraDataPackageARN (\ s a -> s{_srcExtraDataPackageARN = a});

-- | A list of auxiliary apps for the run.
srcAuxiliaryApps :: Lens' ScheduleRunConfiguration [Text]
srcAuxiliaryApps = lens _srcAuxiliaryApps (\ s a -> s{_srcAuxiliaryApps = a}) . _Default . _Coerce;

instance Hashable ScheduleRunConfiguration

instance NFData ScheduleRunConfiguration

instance ToJSON ScheduleRunConfiguration where
        toJSON ScheduleRunConfiguration'{..}
          = object
              (catMaybes
                 [("billingMethod" .=) <$> _srcBillingMethod,
                  ("radios" .=) <$> _srcRadios,
                  ("location" .=) <$> _srcLocation,
                  ("locale" .=) <$> _srcLocale,
                  ("networkProfileArn" .=) <$> _srcNetworkProfileARN,
                  ("extraDataPackageArn" .=) <$>
                    _srcExtraDataPackageARN,
                  ("auxiliaryApps" .=) <$> _srcAuxiliaryApps])

-- | Represents additional test settings.
--
--
--
-- /See:/ 'scheduleRunTest' smart constructor.
data ScheduleRunTest = ScheduleRunTest'
    { _srtTestPackageARN :: !(Maybe Text)
    , _srtParameters     :: !(Maybe (Map Text Text))
    , _srtFilter         :: !(Maybe Text)
    , _srtType           :: !TestType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScheduleRunTest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srtTestPackageARN' - The ARN of the uploaded test that will be run.
--
-- * 'srtParameters' - The test's parameters, such as test framework parameters and fixture settings.
--
-- * 'srtFilter' - The test's filter.
--
-- * 'srtType' - The test's type. Must be one of the following values:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for Web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for Web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for Web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
scheduleRunTest
    :: TestType -- ^ 'srtType'
    -> ScheduleRunTest
scheduleRunTest pType_ =
    ScheduleRunTest'
    { _srtTestPackageARN = Nothing
    , _srtParameters = Nothing
    , _srtFilter = Nothing
    , _srtType = pType_
    }

-- | The ARN of the uploaded test that will be run.
srtTestPackageARN :: Lens' ScheduleRunTest (Maybe Text)
srtTestPackageARN = lens _srtTestPackageARN (\ s a -> s{_srtTestPackageARN = a});

-- | The test's parameters, such as test framework parameters and fixture settings.
srtParameters :: Lens' ScheduleRunTest (HashMap Text Text)
srtParameters = lens _srtParameters (\ s a -> s{_srtParameters = a}) . _Default . _Map;

-- | The test's filter.
srtFilter :: Lens' ScheduleRunTest (Maybe Text)
srtFilter = lens _srtFilter (\ s a -> s{_srtFilter = a});

-- | The test's type. Must be one of the following values:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for Web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for Web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for Web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
srtType :: Lens' ScheduleRunTest TestType
srtType = lens _srtType (\ s a -> s{_srtType = a});

instance Hashable ScheduleRunTest

instance NFData ScheduleRunTest

instance ToJSON ScheduleRunTest where
        toJSON ScheduleRunTest'{..}
          = object
              (catMaybes
                 [("testPackageArn" .=) <$> _srtTestPackageARN,
                  ("parameters" .=) <$> _srtParameters,
                  ("filter" .=) <$> _srtFilter,
                  Just ("type" .= _srtType)])

-- | Represents a collection of one or more tests.
--
--
--
-- /See:/ 'suite' smart constructor.
data Suite = Suite'
    { _sStatus        :: !(Maybe ExecutionStatus)
    , _sCounters      :: !(Maybe Counters)
    , _sArn           :: !(Maybe Text)
    , _sCreated       :: !(Maybe POSIX)
    , _sStopped       :: !(Maybe POSIX)
    , _sResult        :: !(Maybe ExecutionResult)
    , _sName          :: !(Maybe Text)
    , _sDeviceMinutes :: !(Maybe DeviceMinutes)
    , _sType          :: !(Maybe TestType)
    , _sMessage       :: !(Maybe Text)
    , _sStarted       :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Suite' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStatus' - The suite's status. Allowed values include:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
--
-- * 'sCounters' - The suite's result counters.
--
-- * 'sArn' - The suite's ARN.
--
-- * 'sCreated' - When the suite was created.
--
-- * 'sStopped' - The suite's stop time.
--
-- * 'sResult' - The suite's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
--
-- * 'sName' - The suite's name.
--
-- * 'sDeviceMinutes' - Represents the total (metered or unmetered) minutes used by the test suite.
--
-- * 'sType' - The suite's type. Must be one of the following values:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for Web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for Web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for Web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
--
-- * 'sMessage' - A message about the suite's result.
--
-- * 'sStarted' - The suite's start time.
suite
    :: Suite
suite =
    Suite'
    { _sStatus = Nothing
    , _sCounters = Nothing
    , _sArn = Nothing
    , _sCreated = Nothing
    , _sStopped = Nothing
    , _sResult = Nothing
    , _sName = Nothing
    , _sDeviceMinutes = Nothing
    , _sType = Nothing
    , _sMessage = Nothing
    , _sStarted = Nothing
    }

-- | The suite's status. Allowed values include:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
sStatus :: Lens' Suite (Maybe ExecutionStatus)
sStatus = lens _sStatus (\ s a -> s{_sStatus = a});

-- | The suite's result counters.
sCounters :: Lens' Suite (Maybe Counters)
sCounters = lens _sCounters (\ s a -> s{_sCounters = a});

-- | The suite's ARN.
sArn :: Lens' Suite (Maybe Text)
sArn = lens _sArn (\ s a -> s{_sArn = a});

-- | When the suite was created.
sCreated :: Lens' Suite (Maybe UTCTime)
sCreated = lens _sCreated (\ s a -> s{_sCreated = a}) . mapping _Time;

-- | The suite's stop time.
sStopped :: Lens' Suite (Maybe UTCTime)
sStopped = lens _sStopped (\ s a -> s{_sStopped = a}) . mapping _Time;

-- | The suite's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
sResult :: Lens' Suite (Maybe ExecutionResult)
sResult = lens _sResult (\ s a -> s{_sResult = a});

-- | The suite's name.
sName :: Lens' Suite (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a});

-- | Represents the total (metered or unmetered) minutes used by the test suite.
sDeviceMinutes :: Lens' Suite (Maybe DeviceMinutes)
sDeviceMinutes = lens _sDeviceMinutes (\ s a -> s{_sDeviceMinutes = a});

-- | The suite's type. Must be one of the following values:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for Web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for Web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for Web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
sType :: Lens' Suite (Maybe TestType)
sType = lens _sType (\ s a -> s{_sType = a});

-- | A message about the suite's result.
sMessage :: Lens' Suite (Maybe Text)
sMessage = lens _sMessage (\ s a -> s{_sMessage = a});

-- | The suite's start time.
sStarted :: Lens' Suite (Maybe UTCTime)
sStarted = lens _sStarted (\ s a -> s{_sStarted = a}) . mapping _Time;

instance FromJSON Suite where
        parseJSON
          = withObject "Suite"
              (\ x ->
                 Suite' <$>
                   (x .:? "status") <*> (x .:? "counters") <*>
                     (x .:? "arn")
                     <*> (x .:? "created")
                     <*> (x .:? "stopped")
                     <*> (x .:? "result")
                     <*> (x .:? "name")
                     <*> (x .:? "deviceMinutes")
                     <*> (x .:? "type")
                     <*> (x .:? "message")
                     <*> (x .:? "started"))

instance Hashable Suite

instance NFData Suite

-- | Represents a condition that is evaluated.
--
--
--
-- /See:/ 'test' smart constructor.
data Test = Test'
    { _tStatus        :: !(Maybe ExecutionStatus)
    , _tCounters      :: !(Maybe Counters)
    , _tArn           :: !(Maybe Text)
    , _tCreated       :: !(Maybe POSIX)
    , _tStopped       :: !(Maybe POSIX)
    , _tResult        :: !(Maybe ExecutionResult)
    , _tName          :: !(Maybe Text)
    , _tDeviceMinutes :: !(Maybe DeviceMinutes)
    , _tType          :: !(Maybe TestType)
    , _tMessage       :: !(Maybe Text)
    , _tStarted       :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Test' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tStatus' - The test's status. Allowed values include:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
--
-- * 'tCounters' - The test's result counters.
--
-- * 'tArn' - The test's ARN.
--
-- * 'tCreated' - When the test was created.
--
-- * 'tStopped' - The test's stop time.
--
-- * 'tResult' - The test's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
--
-- * 'tName' - The test's name.
--
-- * 'tDeviceMinutes' - Represents the total (metered or unmetered) minutes used by the test.
--
-- * 'tType' - The test's type. Must be one of the following values:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for Web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for Web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for Web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
--
-- * 'tMessage' - A message about the test's result.
--
-- * 'tStarted' - The test's start time.
test
    :: Test
test =
    Test'
    { _tStatus = Nothing
    , _tCounters = Nothing
    , _tArn = Nothing
    , _tCreated = Nothing
    , _tStopped = Nothing
    , _tResult = Nothing
    , _tName = Nothing
    , _tDeviceMinutes = Nothing
    , _tType = Nothing
    , _tMessage = Nothing
    , _tStarted = Nothing
    }

-- | The test's status. Allowed values include:     * PENDING: A pending status.     * PENDING_CONCURRENCY: A pending concurrency status.     * PENDING_DEVICE: A pending device status.     * PROCESSING: A processing status.     * SCHEDULING: A scheduling status.     * PREPARING: A preparing status.     * RUNNING: A running status.     * COMPLETED: A completed status.     * STOPPING: A stopping status.
tStatus :: Lens' Test (Maybe ExecutionStatus)
tStatus = lens _tStatus (\ s a -> s{_tStatus = a});

-- | The test's result counters.
tCounters :: Lens' Test (Maybe Counters)
tCounters = lens _tCounters (\ s a -> s{_tCounters = a});

-- | The test's ARN.
tArn :: Lens' Test (Maybe Text)
tArn = lens _tArn (\ s a -> s{_tArn = a});

-- | When the test was created.
tCreated :: Lens' Test (Maybe UTCTime)
tCreated = lens _tCreated (\ s a -> s{_tCreated = a}) . mapping _Time;

-- | The test's stop time.
tStopped :: Lens' Test (Maybe UTCTime)
tStopped = lens _tStopped (\ s a -> s{_tStopped = a}) . mapping _Time;

-- | The test's result. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
tResult :: Lens' Test (Maybe ExecutionResult)
tResult = lens _tResult (\ s a -> s{_tResult = a});

-- | The test's name.
tName :: Lens' Test (Maybe Text)
tName = lens _tName (\ s a -> s{_tName = a});

-- | Represents the total (metered or unmetered) minutes used by the test.
tDeviceMinutes :: Lens' Test (Maybe DeviceMinutes)
tDeviceMinutes = lens _tDeviceMinutes (\ s a -> s{_tDeviceMinutes = a});

-- | The test's type. Must be one of the following values:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for Web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for Web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for Web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
tType :: Lens' Test (Maybe TestType)
tType = lens _tType (\ s a -> s{_tType = a});

-- | A message about the test's result.
tMessage :: Lens' Test (Maybe Text)
tMessage = lens _tMessage (\ s a -> s{_tMessage = a});

-- | The test's start time.
tStarted :: Lens' Test (Maybe UTCTime)
tStarted = lens _tStarted (\ s a -> s{_tStarted = a}) . mapping _Time;

instance FromJSON Test where
        parseJSON
          = withObject "Test"
              (\ x ->
                 Test' <$>
                   (x .:? "status") <*> (x .:? "counters") <*>
                     (x .:? "arn")
                     <*> (x .:? "created")
                     <*> (x .:? "stopped")
                     <*> (x .:? "result")
                     <*> (x .:? "name")
                     <*> (x .:? "deviceMinutes")
                     <*> (x .:? "type")
                     <*> (x .:? "message")
                     <*> (x .:? "started"))

instance Hashable Test

instance NFData Test

-- | A collection of one or more problems, grouped by their result.
--
--
--
-- /See:/ 'uniqueProblem' smart constructor.
data UniqueProblem = UniqueProblem'
    { _upProblems :: !(Maybe [Problem])
    , _upMessage  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UniqueProblem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upProblems' - Information about the problems.
--
-- * 'upMessage' - A message about the unique problems' result.
uniqueProblem
    :: UniqueProblem
uniqueProblem =
    UniqueProblem'
    { _upProblems = Nothing
    , _upMessage = Nothing
    }

-- | Information about the problems.
upProblems :: Lens' UniqueProblem [Problem]
upProblems = lens _upProblems (\ s a -> s{_upProblems = a}) . _Default . _Coerce;

-- | A message about the unique problems' result.
upMessage :: Lens' UniqueProblem (Maybe Text)
upMessage = lens _upMessage (\ s a -> s{_upMessage = a});

instance FromJSON UniqueProblem where
        parseJSON
          = withObject "UniqueProblem"
              (\ x ->
                 UniqueProblem' <$>
                   (x .:? "problems" .!= mempty) <*> (x .:? "message"))

instance Hashable UniqueProblem

instance NFData UniqueProblem

-- | An app or a set of one or more tests to upload or that have been uploaded.
--
--
--
-- /See:/ 'upload' smart constructor.
data Upload = Upload'
    { _uStatus      :: !(Maybe UploadStatus)
    , _uArn         :: !(Maybe Text)
    , _uCreated     :: !(Maybe POSIX)
    , _uUrl         :: !(Maybe Text)
    , _uName        :: !(Maybe Text)
    , _uMetadata    :: !(Maybe Text)
    , _uType        :: !(Maybe UploadType)
    , _uMessage     :: !(Maybe Text)
    , _uContentType :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Upload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uStatus' - The upload's status. Must be one of the following values:     * FAILED: A failed status.     * INITIALIZED: An initialized status.     * PROCESSING: A processing status.     * SUCCEEDED: A succeeded status.
--
-- * 'uArn' - The upload's ARN.
--
-- * 'uCreated' - When the upload was created.
--
-- * 'uUrl' - The pre-signed Amazon S3 URL that was used to store a file through a corresponding PUT request.
--
-- * 'uName' - The upload's file name.
--
-- * 'uMetadata' - The upload's metadata. For example, for Android, this contains information that is parsed from the manifest and is displayed in the AWS Device Farm console after the associated app is uploaded.
--
-- * 'uType' - The upload's type. Must be one of the following values:     * ANDROID_APP: An Android upload.     * IOS_APP: An iOS upload.     * WEB_APP: A web appliction upload.     * EXTERNAL_DATA: An external data upload.     * APPIUM_JAVA_JUNIT_TEST_PACKAGE: An Appium Java JUnit test package upload.     * APPIUM_JAVA_TESTNG_TEST_PACKAGE: An Appium Java TestNG test package upload.     * APPIUM_PYTHON_TEST_PACKAGE: An Appium Python test package upload.     * APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE: An Appium Java JUnit test package upload.     * APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE: An Appium Java TestNG test package upload.     * APPIUM_WEB_PYTHON_TEST_PACKAGE: An Appium Python test package upload.     * CALABASH_TEST_PACKAGE: A Calabash test package upload.     * INSTRUMENTATION_TEST_PACKAGE: An instrumentation upload.     * UIAUTOMATION_TEST_PACKAGE: A uiautomation test package upload.     * UIAUTOMATOR_TEST_PACKAGE: A uiautomator test package upload.     * XCTEST_TEST_PACKAGE: An XCode test package upload.     * XCTEST_UI_TEST_PACKAGE: An XCode UI test package upload.
--
-- * 'uMessage' - A message about the upload's result.
--
-- * 'uContentType' - The upload's content type (for example, "application/octet-stream").
upload
    :: Upload
upload =
    Upload'
    { _uStatus = Nothing
    , _uArn = Nothing
    , _uCreated = Nothing
    , _uUrl = Nothing
    , _uName = Nothing
    , _uMetadata = Nothing
    , _uType = Nothing
    , _uMessage = Nothing
    , _uContentType = Nothing
    }

-- | The upload's status. Must be one of the following values:     * FAILED: A failed status.     * INITIALIZED: An initialized status.     * PROCESSING: A processing status.     * SUCCEEDED: A succeeded status.
uStatus :: Lens' Upload (Maybe UploadStatus)
uStatus = lens _uStatus (\ s a -> s{_uStatus = a});

-- | The upload's ARN.
uArn :: Lens' Upload (Maybe Text)
uArn = lens _uArn (\ s a -> s{_uArn = a});

-- | When the upload was created.
uCreated :: Lens' Upload (Maybe UTCTime)
uCreated = lens _uCreated (\ s a -> s{_uCreated = a}) . mapping _Time;

-- | The pre-signed Amazon S3 URL that was used to store a file through a corresponding PUT request.
uUrl :: Lens' Upload (Maybe Text)
uUrl = lens _uUrl (\ s a -> s{_uUrl = a});

-- | The upload's file name.
uName :: Lens' Upload (Maybe Text)
uName = lens _uName (\ s a -> s{_uName = a});

-- | The upload's metadata. For example, for Android, this contains information that is parsed from the manifest and is displayed in the AWS Device Farm console after the associated app is uploaded.
uMetadata :: Lens' Upload (Maybe Text)
uMetadata = lens _uMetadata (\ s a -> s{_uMetadata = a});

-- | The upload's type. Must be one of the following values:     * ANDROID_APP: An Android upload.     * IOS_APP: An iOS upload.     * WEB_APP: A web appliction upload.     * EXTERNAL_DATA: An external data upload.     * APPIUM_JAVA_JUNIT_TEST_PACKAGE: An Appium Java JUnit test package upload.     * APPIUM_JAVA_TESTNG_TEST_PACKAGE: An Appium Java TestNG test package upload.     * APPIUM_PYTHON_TEST_PACKAGE: An Appium Python test package upload.     * APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE: An Appium Java JUnit test package upload.     * APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE: An Appium Java TestNG test package upload.     * APPIUM_WEB_PYTHON_TEST_PACKAGE: An Appium Python test package upload.     * CALABASH_TEST_PACKAGE: A Calabash test package upload.     * INSTRUMENTATION_TEST_PACKAGE: An instrumentation upload.     * UIAUTOMATION_TEST_PACKAGE: A uiautomation test package upload.     * UIAUTOMATOR_TEST_PACKAGE: A uiautomator test package upload.     * XCTEST_TEST_PACKAGE: An XCode test package upload.     * XCTEST_UI_TEST_PACKAGE: An XCode UI test package upload.
uType :: Lens' Upload (Maybe UploadType)
uType = lens _uType (\ s a -> s{_uType = a});

-- | A message about the upload's result.
uMessage :: Lens' Upload (Maybe Text)
uMessage = lens _uMessage (\ s a -> s{_uMessage = a});

-- | The upload's content type (for example, "application/octet-stream").
uContentType :: Lens' Upload (Maybe Text)
uContentType = lens _uContentType (\ s a -> s{_uContentType = a});

instance FromJSON Upload where
        parseJSON
          = withObject "Upload"
              (\ x ->
                 Upload' <$>
                   (x .:? "status") <*> (x .:? "arn") <*>
                     (x .:? "created")
                     <*> (x .:? "url")
                     <*> (x .:? "name")
                     <*> (x .:? "metadata")
                     <*> (x .:? "type")
                     <*> (x .:? "message")
                     <*> (x .:? "contentType"))

instance Hashable Upload

instance NFData Upload
