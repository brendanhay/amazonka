{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Product where

import           Network.AWS.DeviceFarm.Types.Sum
import           Network.AWS.Prelude

-- | A container for account-level settings within AWS Device Farm.
--
-- /See:/ 'accountSettings' smart constructor.
data AccountSettings = AccountSettings'
    { _asAwsAccountNumber :: !(Maybe Text)
    , _asUnmeteredDevices :: !(Maybe (Map DevicePlatform Int))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asAwsAccountNumber'
--
-- * 'asUnmeteredDevices'
accountSettings
    :: AccountSettings
accountSettings =
    AccountSettings'
    { _asAwsAccountNumber = Nothing
    , _asUnmeteredDevices = Nothing
    }

-- | The AWS account number specified in the 'AccountSettings' container.
asAwsAccountNumber :: Lens' AccountSettings (Maybe Text)
asAwsAccountNumber = lens _asAwsAccountNumber (\ s a -> s{_asAwsAccountNumber = a});

-- | Returns the unmetered devices you have purchased.
asUnmeteredDevices :: Lens' AccountSettings (HashMap DevicePlatform Int)
asUnmeteredDevices = lens _asUnmeteredDevices (\ s a -> s{_asUnmeteredDevices = a}) . _Default . _Map;

instance FromJSON AccountSettings where
        parseJSON
          = withObject "AccountSettings"
              (\ x ->
                 AccountSettings' <$>
                   (x .:? "awsAccountNumber") <*>
                     (x .:? "unmeteredDevices" .!= mempty))

-- | Represents the output of a test. Examples of artifacts include logs and
-- screenshots.
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
-- * 'aArn'
--
-- * 'aUrl'
--
-- * 'aExtension'
--
-- * 'aName'
--
-- * 'aType'
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

-- | The artifact\'s ARN.
aArn :: Lens' Artifact (Maybe Text)
aArn = lens _aArn (\ s a -> s{_aArn = a});

-- | The pre-signed Amazon S3 URL that can be used with a corresponding GET
-- request to download the artifact\'s file.
aUrl :: Lens' Artifact (Maybe Text)
aUrl = lens _aUrl (\ s a -> s{_aUrl = a});

-- | The artifact\'s file extension.
aExtension :: Lens' Artifact (Maybe Text)
aExtension = lens _aExtension (\ s a -> s{_aExtension = a});

-- | The artifact\'s name.
aName :: Lens' Artifact (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

-- | The artifact\'s type.
--
-- Allowed values include the following:
--
-- -   APPIUM_JAVA_OUTPUT: The Appium Java output type.
--
-- -   APPIUM_JAVA_XML_OUTPUT: The Appium Java XML output type.
--
-- -   APPIUM_SERVER_OUTPUT: The Appium server output type.
--
-- -   AUTOMATION_OUTPUT: The automation output type.
--
-- -   CALABASH_JSON_OUTPUT: The Calabash JSON output type.
--
-- -   CALABASH_JAVA_XML_OUTPUT: The Calabash Java XML output type.
--
-- -   CALABASH_PRETTY_OUTPUT: The Calabash pretty output type.
--
-- -   CALABASH_STANDARD_OUTPUT: The Calabash standard output type.
--
-- -   DEVICE_LOG: The device log type.
--
-- -   EXERCISER_MONKEY_OUTPUT: For Android, the artifact (log) generated
--     by an Android fuzz test.
--
-- -   INSTRUMENTATION_OUTPUT: The instrumentation type.
--
-- -   MESSAGE_LOG: The message log type.
--
-- -   RESULT_LOG: The result log type.
--
-- -   SCREENSHOT: The screenshot type.
--
-- -   SERVICE_LOG: The service log type.
--
-- -   UNKNOWN: An unknown type.
--
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

-- | Represents the amount of CPU that an app is using on a physical device.
--
-- Note that this does not represent system-wide CPU usage.
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
-- * 'cpuFrequency'
--
-- * 'cpuClock'
--
-- * 'cpuArchitecture'
cpu
    :: CPU
cpu =
    CPU'
    { _cpuFrequency = Nothing
    , _cpuClock = Nothing
    , _cpuArchitecture = Nothing
    }

-- | The CPU\'s frequency.
cpuFrequency :: Lens' CPU (Maybe Text)
cpuFrequency = lens _cpuFrequency (\ s a -> s{_cpuFrequency = a});

-- | The clock speed of the device\'s CPU, expressed in hertz (Hz). For
-- example, a 1.2 GHz CPU is expressed as 1200000000.
cpuClock :: Lens' CPU (Maybe Double)
cpuClock = lens _cpuClock (\ s a -> s{_cpuClock = a});

-- | The CPU\'s architecture, for example x86 or ARM.
cpuArchitecture :: Lens' CPU (Maybe Text)
cpuArchitecture = lens _cpuArchitecture (\ s a -> s{_cpuArchitecture = a});

instance FromJSON CPU where
        parseJSON
          = withObject "CPU"
              (\ x ->
                 CPU' <$>
                   (x .:? "frequency") <*> (x .:? "clock") <*>
                     (x .:? "architecture"))

-- | Represents entity counters.
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
-- * 'cPassed'
--
-- * 'cSkipped'
--
-- * 'cWarned'
--
-- * 'cStopped'
--
-- * 'cTotal'
--
-- * 'cFailed'
--
-- * 'cErrored'
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

-- | Represents a device type that an app is tested against.
--
-- /See:/ 'device' smart constructor.
data Device = Device'
    { _dCarrier      :: !(Maybe Text)
    , _dImage        :: !(Maybe Text)
    , _dManufacturer :: !(Maybe Text)
    , _dPlatform     :: !(Maybe DevicePlatform)
    , _dArn          :: !(Maybe Text)
    , _dFormFactor   :: !(Maybe DeviceFormFactor)
    , _dResolution   :: !(Maybe Resolution)
    , _dMemory       :: !(Maybe Integer)
    , _dRadio        :: !(Maybe Text)
    , _dOs           :: !(Maybe Text)
    , _dName         :: !(Maybe Text)
    , _dModel        :: !(Maybe Text)
    , _dCpu          :: !(Maybe CPU)
    , _dHeapSize     :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCarrier'
--
-- * 'dImage'
--
-- * 'dManufacturer'
--
-- * 'dPlatform'
--
-- * 'dArn'
--
-- * 'dFormFactor'
--
-- * 'dResolution'
--
-- * 'dMemory'
--
-- * 'dRadio'
--
-- * 'dOs'
--
-- * 'dName'
--
-- * 'dModel'
--
-- * 'dCpu'
--
-- * 'dHeapSize'
device
    :: Device
device =
    Device'
    { _dCarrier = Nothing
    , _dImage = Nothing
    , _dManufacturer = Nothing
    , _dPlatform = Nothing
    , _dArn = Nothing
    , _dFormFactor = Nothing
    , _dResolution = Nothing
    , _dMemory = Nothing
    , _dRadio = Nothing
    , _dOs = Nothing
    , _dName = Nothing
    , _dModel = Nothing
    , _dCpu = Nothing
    , _dHeapSize = Nothing
    }

-- | The device\'s carrier.
dCarrier :: Lens' Device (Maybe Text)
dCarrier = lens _dCarrier (\ s a -> s{_dCarrier = a});

-- | The device\'s image name.
dImage :: Lens' Device (Maybe Text)
dImage = lens _dImage (\ s a -> s{_dImage = a});

-- | The device\'s manufacturer name.
dManufacturer :: Lens' Device (Maybe Text)
dManufacturer = lens _dManufacturer (\ s a -> s{_dManufacturer = a});

-- | The device\'s platform.
--
-- Allowed values include:
--
-- -   ANDROID: The Android platform.
--
-- -   IOS: The iOS platform.
--
dPlatform :: Lens' Device (Maybe DevicePlatform)
dPlatform = lens _dPlatform (\ s a -> s{_dPlatform = a});

-- | The device\'s ARN.
dArn :: Lens' Device (Maybe Text)
dArn = lens _dArn (\ s a -> s{_dArn = a});

-- | The device\'s form factor.
--
-- Allowed values include:
--
-- -   PHONE: The phone form factor.
--
-- -   TABLET: The tablet form factor.
--
dFormFactor :: Lens' Device (Maybe DeviceFormFactor)
dFormFactor = lens _dFormFactor (\ s a -> s{_dFormFactor = a});

-- | Undocumented member.
dResolution :: Lens' Device (Maybe Resolution)
dResolution = lens _dResolution (\ s a -> s{_dResolution = a});

-- | The device\'s total memory size, expressed in bytes.
dMemory :: Lens' Device (Maybe Integer)
dMemory = lens _dMemory (\ s a -> s{_dMemory = a});

-- | The device\'s radio.
dRadio :: Lens' Device (Maybe Text)
dRadio = lens _dRadio (\ s a -> s{_dRadio = a});

-- | The device\'s operating system type.
dOs :: Lens' Device (Maybe Text)
dOs = lens _dOs (\ s a -> s{_dOs = a});

-- | The device\'s display name.
dName :: Lens' Device (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a});

-- | The device\'s model name.
dModel :: Lens' Device (Maybe Text)
dModel = lens _dModel (\ s a -> s{_dModel = a});

-- | Information about the device\'s CPU.
dCpu :: Lens' Device (Maybe CPU)
dCpu = lens _dCpu (\ s a -> s{_dCpu = a});

-- | The device\'s heap size, expressed in bytes.
dHeapSize :: Lens' Device (Maybe Integer)
dHeapSize = lens _dHeapSize (\ s a -> s{_dHeapSize = a});

instance FromJSON Device where
        parseJSON
          = withObject "Device"
              (\ x ->
                 Device' <$>
                   (x .:? "carrier") <*> (x .:? "image") <*>
                     (x .:? "manufacturer")
                     <*> (x .:? "platform")
                     <*> (x .:? "arn")
                     <*> (x .:? "formFactor")
                     <*> (x .:? "resolution")
                     <*> (x .:? "memory")
                     <*> (x .:? "radio")
                     <*> (x .:? "os")
                     <*> (x .:? "name")
                     <*> (x .:? "model")
                     <*> (x .:? "cpu")
                     <*> (x .:? "heapSize"))

-- | Represents a collection of device types.
--
-- /See:/ 'devicePool' smart constructor.
data DevicePool = DevicePool'
    { _dpArn         :: !(Maybe Text)
    , _dpRules       :: !(Maybe [Rule])
    , _dpName        :: !(Maybe Text)
    , _dpType        :: !(Maybe DevicePoolType)
    , _dpDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DevicePool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpArn'
--
-- * 'dpRules'
--
-- * 'dpName'
--
-- * 'dpType'
--
-- * 'dpDescription'
devicePool
    :: DevicePool
devicePool =
    DevicePool'
    { _dpArn = Nothing
    , _dpRules = Nothing
    , _dpName = Nothing
    , _dpType = Nothing
    , _dpDescription = Nothing
    }

-- | The device pool\'s ARN.
dpArn :: Lens' DevicePool (Maybe Text)
dpArn = lens _dpArn (\ s a -> s{_dpArn = a});

-- | Information about the device pool\'s rules.
dpRules :: Lens' DevicePool [Rule]
dpRules = lens _dpRules (\ s a -> s{_dpRules = a}) . _Default . _Coerce;

-- | The device pool\'s name.
dpName :: Lens' DevicePool (Maybe Text)
dpName = lens _dpName (\ s a -> s{_dpName = a});

-- | The device pool\'s type.
--
-- Allowed values include:
--
-- -   CURATED: A device pool that is created and managed by AWS Device
--     Farm.
--
-- -   PRIVATE: A device pool that is created and managed by the device
--     pool developer.
--
dpType :: Lens' DevicePool (Maybe DevicePoolType)
dpType = lens _dpType (\ s a -> s{_dpType = a});

-- | The device pool\'s description.
dpDescription :: Lens' DevicePool (Maybe Text)
dpDescription = lens _dpDescription (\ s a -> s{_dpDescription = a});

instance FromJSON DevicePool where
        parseJSON
          = withObject "DevicePool"
              (\ x ->
                 DevicePool' <$>
                   (x .:? "arn") <*> (x .:? "rules" .!= mempty) <*>
                     (x .:? "name")
                     <*> (x .:? "type")
                     <*> (x .:? "description"))

-- | Represents a device pool compatibility result.
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
-- * 'dpcrDevice'
--
-- * 'dpcrCompatible'
--
-- * 'dpcrIncompatibilityMessages'
devicePoolCompatibilityResult
    :: DevicePoolCompatibilityResult
devicePoolCompatibilityResult =
    DevicePoolCompatibilityResult'
    { _dpcrDevice = Nothing
    , _dpcrCompatible = Nothing
    , _dpcrIncompatibilityMessages = Nothing
    }

-- | Undocumented member.
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

-- | Represents information about incompatibility.
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
-- * 'imType'
--
-- * 'imMessage'
incompatibilityMessage
    :: IncompatibilityMessage
incompatibilityMessage =
    IncompatibilityMessage'
    { _imType = Nothing
    , _imMessage = Nothing
    }

-- | The type of incompatibility.
--
-- Allowed values include:
--
-- -   ARN: The ARN.
--
-- -   FORM_FACTOR: The form factor (for example, phone or tablet).
--
-- -   MANUFACTURER: The manufacturer.
--
-- -   PLATFORM: The platform (for example, Android or iOS).
--
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

-- | Represents a device.
--
-- /See:/ 'job' smart constructor.
data Job = Job'
    { _jobStatus   :: !(Maybe ExecutionStatus)
    , _jobCounters :: !(Maybe Counters)
    , _jobArn      :: !(Maybe Text)
    , _jobCreated  :: !(Maybe POSIX)
    , _jobDevice   :: !(Maybe Device)
    , _jobStopped  :: !(Maybe POSIX)
    , _jobResult   :: !(Maybe ExecutionResult)
    , _jobName     :: !(Maybe Text)
    , _jobType     :: !(Maybe TestType)
    , _jobMessage  :: !(Maybe Text)
    , _jobStarted  :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jobStatus'
--
-- * 'jobCounters'
--
-- * 'jobArn'
--
-- * 'jobCreated'
--
-- * 'jobDevice'
--
-- * 'jobStopped'
--
-- * 'jobResult'
--
-- * 'jobName'
--
-- * 'jobType'
--
-- * 'jobMessage'
--
-- * 'jobStarted'
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
    , _jobType = Nothing
    , _jobMessage = Nothing
    , _jobStarted = Nothing
    }

-- | The job\'s status.
--
-- Allowed values include:
--
-- -   COMPLETED: A completed status.
--
-- -   PENDING: A pending status.
--
-- -   PROCESSING: A processing status.
--
-- -   RUNNING: A running status.
--
-- -   SCHEDULING: A scheduling status.
--
jobStatus :: Lens' Job (Maybe ExecutionStatus)
jobStatus = lens _jobStatus (\ s a -> s{_jobStatus = a});

-- | The job\'s result counters.
jobCounters :: Lens' Job (Maybe Counters)
jobCounters = lens _jobCounters (\ s a -> s{_jobCounters = a});

-- | The job\'s ARN.
jobArn :: Lens' Job (Maybe Text)
jobArn = lens _jobArn (\ s a -> s{_jobArn = a});

-- | When the job was created.
jobCreated :: Lens' Job (Maybe UTCTime)
jobCreated = lens _jobCreated (\ s a -> s{_jobCreated = a}) . mapping _Time;

-- | Undocumented member.
jobDevice :: Lens' Job (Maybe Device)
jobDevice = lens _jobDevice (\ s a -> s{_jobDevice = a});

-- | The job\'s stop time.
jobStopped :: Lens' Job (Maybe UTCTime)
jobStopped = lens _jobStopped (\ s a -> s{_jobStopped = a}) . mapping _Time;

-- | The job\'s result.
--
-- Allowed values include:
--
-- -   ERRORED: An error condition.
--
-- -   FAILED: A failed condition.
--
-- -   SKIPPED: A skipped condition.
--
-- -   STOPPED: A stopped condition.
--
-- -   PASSED: A passing condition.
--
-- -   PENDING: A pending condition.
--
-- -   WARNED: A warning condition.
--
jobResult :: Lens' Job (Maybe ExecutionResult)
jobResult = lens _jobResult (\ s a -> s{_jobResult = a});

-- | The job\'s name.
jobName :: Lens' Job (Maybe Text)
jobName = lens _jobName (\ s a -> s{_jobName = a});

-- | The job\'s type.
--
-- Allowed values include the following:
--
-- -   BUILTIN_FUZZ: The built-in fuzz type.
--
-- -   BUILTIN_EXPLORER: For Android, an app explorer that will traverse an
--     Android app, interacting with it and capturing screenshots at the
--     same time.
--
-- -   APPIUM_JAVA_JUNIT: The Appium Java JUnit type.
--
-- -   APPIUM_JAVA_TESTNG: The Appium Java TestNG type.
--
-- -   CALABASH: The Calabash type.
--
-- -   INSTRUMENTATION: The Instrumentation type.
--
-- -   UIAUTOMATION: The uiautomation type.
--
-- -   UIAUTOMATOR: The uiautomator type.
--
-- -   XCTEST: The XCode test type.
--
jobType :: Lens' Job (Maybe TestType)
jobType = lens _jobType (\ s a -> s{_jobType = a});

-- | A message about the job\'s result.
jobMessage :: Lens' Job (Maybe Text)
jobMessage = lens _jobMessage (\ s a -> s{_jobMessage = a});

-- | The job\'s start time.
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
                     <*> (x .:? "type")
                     <*> (x .:? "message")
                     <*> (x .:? "started"))

-- | Represents a latitude and longitude pair, expressed in geographic
-- coordinate system degrees (for example 47.6204, -122.3491).
--
-- Elevation is currently not supported.
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
-- * 'lLatitude'
--
-- * 'lLongitude'
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

instance ToJSON Location where
        toJSON Location'{..}
          = object
              (catMaybes
                 [Just ("latitude" .= _lLatitude),
                  Just ("longitude" .= _lLongitude)])

-- | Represents a specific warning or failure.
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
-- * 'pDevice'
--
-- * 'pTest'
--
-- * 'pResult'
--
-- * 'pRun'
--
-- * 'pJob'
--
-- * 'pMessage'
--
-- * 'pSuite'
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

-- | The problem\'s result.
--
-- Allowed values include:
--
-- -   ERRORED: An error condition.
--
-- -   FAILED: A failed condition.
--
-- -   SKIPPED: A skipped condition.
--
-- -   STOPPED: A stopped condition.
--
-- -   PASSED: A passing condition.
--
-- -   PENDING: A pending condition.
--
-- -   WARNED: A warning condition.
--
pResult :: Lens' Problem (Maybe ExecutionResult)
pResult = lens _pResult (\ s a -> s{_pResult = a});

-- | Information about the associated run.
pRun :: Lens' Problem (Maybe ProblemDetail)
pRun = lens _pRun (\ s a -> s{_pRun = a});

-- | Information about the associated job.
pJob :: Lens' Problem (Maybe ProblemDetail)
pJob = lens _pJob (\ s a -> s{_pJob = a});

-- | A message about the problem\'s result.
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

-- | Information about a problem detail.
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
-- * 'pdArn'
--
-- * 'pdName'
problemDetail
    :: ProblemDetail
problemDetail =
    ProblemDetail'
    { _pdArn = Nothing
    , _pdName = Nothing
    }

-- | The problem detail\'s ARN.
pdArn :: Lens' ProblemDetail (Maybe Text)
pdArn = lens _pdArn (\ s a -> s{_pdArn = a});

-- | The problem detail\'s name.
pdName :: Lens' ProblemDetail (Maybe Text)
pdName = lens _pdName (\ s a -> s{_pdName = a});

instance FromJSON ProblemDetail where
        parseJSON
          = withObject "ProblemDetail"
              (\ x ->
                 ProblemDetail' <$> (x .:? "arn") <*> (x .:? "name"))

-- | Represents an operating-system neutral workspace for running and
-- managing tests.
--
-- /See:/ 'project' smart constructor.
data Project = Project'
    { _pArn     :: !(Maybe Text)
    , _pCreated :: !(Maybe POSIX)
    , _pName    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Project' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pArn'
--
-- * 'pCreated'
--
-- * 'pName'
project
    :: Project
project =
    Project'
    { _pArn = Nothing
    , _pCreated = Nothing
    , _pName = Nothing
    }

-- | The project\'s ARN.
pArn :: Lens' Project (Maybe Text)
pArn = lens _pArn (\ s a -> s{_pArn = a});

-- | When the project was created.
pCreated :: Lens' Project (Maybe UTCTime)
pCreated = lens _pCreated (\ s a -> s{_pCreated = a}) . mapping _Time;

-- | The project\'s name.
pName :: Lens' Project (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a});

instance FromJSON Project where
        parseJSON
          = withObject "Project"
              (\ x ->
                 Project' <$>
                   (x .:? "arn") <*> (x .:? "created") <*>
                     (x .:? "name"))

-- | Represents the set of radios and their states on a device. Examples of
-- radios include Wi-Fi, GPS, Bluetooth, and NFC.
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
-- * 'rNfc'
--
-- * 'rGps'
--
-- * 'rBluetooth'
--
-- * 'rWifi'
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

-- | True if Bluetooth is enabled at the beginning of the test; otherwise,
-- false.
rBluetooth :: Lens' Radios (Maybe Bool)
rBluetooth = lens _rBluetooth (\ s a -> s{_rBluetooth = a});

-- | True if Wi-Fi is enabled at the beginning of the test; otherwise, false.
rWifi :: Lens' Radios (Maybe Bool)
rWifi = lens _rWifi (\ s a -> s{_rWifi = a});

instance ToJSON Radios where
        toJSON Radios'{..}
          = object
              (catMaybes
                 [("nfc" .=) <$> _rNfc, ("gps" .=) <$> _rGps,
                  ("bluetooth" .=) <$> _rBluetooth,
                  ("wifi" .=) <$> _rWifi])

-- | Represents the screen resolution of a device in height and width,
-- expressed in pixels.
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
-- * 'rHeight'
--
-- * 'rWidth'
resolution
    :: Resolution
resolution =
    Resolution'
    { _rHeight = Nothing
    , _rWidth = Nothing
    }

-- | The screen resolution\'s height, expressed in pixels.
rHeight :: Lens' Resolution (Maybe Int)
rHeight = lens _rHeight (\ s a -> s{_rHeight = a});

-- | The screen resolution\'s width, expressed in pixels.
rWidth :: Lens' Resolution (Maybe Int)
rWidth = lens _rWidth (\ s a -> s{_rWidth = a});

instance FromJSON Resolution where
        parseJSON
          = withObject "Resolution"
              (\ x ->
                 Resolution' <$> (x .:? "height") <*> (x .:? "width"))

-- | Represents a condition for a device pool.
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
-- * 'rAttribute'
--
-- * 'rOperator'
--
-- * 'rValue'
rule
    :: Rule
rule =
    Rule'
    { _rAttribute = Nothing
    , _rOperator = Nothing
    , _rValue = Nothing
    }

-- | The rule\'s attribute.
--
-- Allowed values include:
--
-- -   ARN: The ARN.
--
-- -   FORM_FACTOR: The form factor (for example, phone or tablet).
--
-- -   MANUFACTURER: The manufacturer.
--
-- -   PLATFORM: The platform (for example, Android or iOS).
--
rAttribute :: Lens' Rule (Maybe DeviceAttribute)
rAttribute = lens _rAttribute (\ s a -> s{_rAttribute = a});

-- | The rule\'s operator.
--
-- -   EQUALS: The equals operator.
--
-- -   GREATER_THAN: The greater-than operator.
--
-- -   IN: The in operator.
--
-- -   LESS_THAN: The less-than operator.
--
-- -   NOT_IN: The not-in operator.
--
rOperator :: Lens' Rule (Maybe RuleOperator)
rOperator = lens _rOperator (\ s a -> s{_rOperator = a});

-- | The rule\'s value.
rValue :: Lens' Rule (Maybe Text)
rValue = lens _rValue (\ s a -> s{_rValue = a});

instance FromJSON Rule where
        parseJSON
          = withObject "Rule"
              (\ x ->
                 Rule' <$>
                   (x .:? "attribute") <*> (x .:? "operator") <*>
                     (x .:? "value"))

instance ToJSON Rule where
        toJSON Rule'{..}
          = object
              (catMaybes
                 [("attribute" .=) <$> _rAttribute,
                  ("operator" .=) <$> _rOperator,
                  ("value" .=) <$> _rValue])

-- | Represents an app on a set of devices with a specific test and
-- configuration.
--
-- /See:/ 'run' smart constructor.
data Run = Run'
    { _runBillingMethod :: !(Maybe BillingMethod)
    , _runStatus        :: !(Maybe ExecutionStatus)
    , _runCounters      :: !(Maybe Counters)
    , _runPlatform      :: !(Maybe DevicePlatform)
    , _runArn           :: !(Maybe Text)
    , _runCreated       :: !(Maybe POSIX)
    , _runStopped       :: !(Maybe POSIX)
    , _runResult        :: !(Maybe ExecutionResult)
    , _runCompletedJobs :: !(Maybe Int)
    , _runName          :: !(Maybe Text)
    , _runType          :: !(Maybe TestType)
    , _runMessage       :: !(Maybe Text)
    , _runTotalJobs     :: !(Maybe Int)
    , _runStarted       :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Run' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'runBillingMethod'
--
-- * 'runStatus'
--
-- * 'runCounters'
--
-- * 'runPlatform'
--
-- * 'runArn'
--
-- * 'runCreated'
--
-- * 'runStopped'
--
-- * 'runResult'
--
-- * 'runCompletedJobs'
--
-- * 'runName'
--
-- * 'runType'
--
-- * 'runMessage'
--
-- * 'runTotalJobs'
--
-- * 'runStarted'
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
    , _runType = Nothing
    , _runMessage = Nothing
    , _runTotalJobs = Nothing
    , _runStarted = Nothing
    }

-- | Specifies the billing method for a test run: 'metered' or 'unmetered'.
-- If the parameter is not specified, the default value is 'unmetered'.
runBillingMethod :: Lens' Run (Maybe BillingMethod)
runBillingMethod = lens _runBillingMethod (\ s a -> s{_runBillingMethod = a});

-- | The run\'s status.
--
-- Allowed values include:
--
-- -   COMPLETED: A completed status.
--
-- -   PENDING: A pending status.
--
-- -   PROCESSING: A processing status.
--
-- -   RUNNING: A running status.
--
-- -   SCHEDULING: A scheduling status.
--
runStatus :: Lens' Run (Maybe ExecutionStatus)
runStatus = lens _runStatus (\ s a -> s{_runStatus = a});

-- | The run\'s result counters.
runCounters :: Lens' Run (Maybe Counters)
runCounters = lens _runCounters (\ s a -> s{_runCounters = a});

-- | The run\'s platform.
--
-- Allowed values include:
--
-- -   ANDROID: The Android platform.
--
-- -   IOS: The iOS platform.
--
runPlatform :: Lens' Run (Maybe DevicePlatform)
runPlatform = lens _runPlatform (\ s a -> s{_runPlatform = a});

-- | The run\'s ARN.
runArn :: Lens' Run (Maybe Text)
runArn = lens _runArn (\ s a -> s{_runArn = a});

-- | When the run was created.
runCreated :: Lens' Run (Maybe UTCTime)
runCreated = lens _runCreated (\ s a -> s{_runCreated = a}) . mapping _Time;

-- | The run\'s stop time.
runStopped :: Lens' Run (Maybe UTCTime)
runStopped = lens _runStopped (\ s a -> s{_runStopped = a}) . mapping _Time;

-- | The run\'s result.
--
-- Allowed values include:
--
-- -   ERRORED: An error condition.
--
-- -   FAILED: A failed condition.
--
-- -   SKIPPED: A skipped condition.
--
-- -   STOPPED: A stopped condition.
--
-- -   PASSED: A passing condition.
--
-- -   PENDING: A pending condition.
--
-- -   WARNED: A warning condition.
--
runResult :: Lens' Run (Maybe ExecutionResult)
runResult = lens _runResult (\ s a -> s{_runResult = a});

-- | The total number of completed jobs.
runCompletedJobs :: Lens' Run (Maybe Int)
runCompletedJobs = lens _runCompletedJobs (\ s a -> s{_runCompletedJobs = a});

-- | The run\'s name.
runName :: Lens' Run (Maybe Text)
runName = lens _runName (\ s a -> s{_runName = a});

-- | The run\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ: The built-in fuzz type.
--
-- -   BUILTIN_EXPLORER: For Android, an app explorer that will traverse an
--     Android app, interacting with it and capturing screenshots at the
--     same time.
--
-- -   APPIUM_JAVA_JUNIT: The Appium Java JUnit type.
--
-- -   APPIUM_JAVA_TESTNG: The Appium Java TestNG type.
--
-- -   CALABASH: The Calabash type.
--
-- -   INSTRUMENTATION: The Instrumentation type.
--
-- -   UIAUTOMATION: The uiautomation type.
--
-- -   UIAUTOMATOR: The uiautomator type.
--
-- -   XCTEST: The XCode test type.
--
runType :: Lens' Run (Maybe TestType)
runType = lens _runType (\ s a -> s{_runType = a});

-- | A message about the run\'s result.
runMessage :: Lens' Run (Maybe Text)
runMessage = lens _runMessage (\ s a -> s{_runMessage = a});

-- | The total number of jobs for the run.
runTotalJobs :: Lens' Run (Maybe Int)
runTotalJobs = lens _runTotalJobs (\ s a -> s{_runTotalJobs = a});

-- | The run\'s start time.
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
                     <*> (x .:? "type")
                     <*> (x .:? "message")
                     <*> (x .:? "totalJobs")
                     <*> (x .:? "started"))

-- | Represents a sample of performance data.
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
-- * 'samArn'
--
-- * 'samUrl'
--
-- * 'samType'
sample
    :: Sample
sample =
    Sample'
    { _samArn = Nothing
    , _samUrl = Nothing
    , _samType = Nothing
    }

-- | The sample\'s ARN.
samArn :: Lens' Sample (Maybe Text)
samArn = lens _samArn (\ s a -> s{_samArn = a});

-- | The pre-signed Amazon S3 URL that can be used with a corresponding GET
-- request to download the sample\'s file.
samUrl :: Lens' Sample (Maybe Text)
samUrl = lens _samUrl (\ s a -> s{_samUrl = a});

-- | The sample\'s type.
--
-- Must be one of the following values:
--
-- -   CPU: A CPU sample type. This is expressed as the app processing CPU
--     time (including child processes) as reported by process, as a
--     percentage.
--
-- -   MEMORY: A memory usage sample type. This is expressed as the total
--     proportional set size of an app process, in kilobytes.
--
-- -   NATIVE_AVG_DRAWTIME
--
-- -   NATIVE_FPS
--
-- -   NATIVE_FRAMES
--
-- -   NATIVE_MAX_DRAWTIME
--
-- -   NATIVE_MIN_DRAWTIME
--
-- -   OPENGL_AVG_DRAWTIME
--
-- -   OPENGL_FPS
--
-- -   OPENGL_FRAMES
--
-- -   OPENGL_MAX_DRAWTIME
--
-- -   OPENGL_MIN_DRAWTIME
--
-- -   RX
--
-- -   RX_RATE: The total number of bytes per second (TCP and UDP) that are
--     sent, by app process.
--
-- -   THREADS: A threads sample type. This is expressed as the total
--     number of threads per app process.
--
-- -   TX
--
-- -   TX_RATE: The total number of bytes per second (TCP and UDP) that are
--     received, by app process.
--
samType :: Lens' Sample (Maybe SampleType)
samType = lens _samType (\ s a -> s{_samType = a});

instance FromJSON Sample where
        parseJSON
          = withObject "Sample"
              (\ x ->
                 Sample' <$>
                   (x .:? "arn") <*> (x .:? "url") <*> (x .:? "type"))

-- | Represents the settings for a run. Includes things like location, radio
-- states, auxiliary apps, and network profiles.
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
-- * 'srcBillingMethod'
--
-- * 'srcRadios'
--
-- * 'srcLocation'
--
-- * 'srcLocale'
--
-- * 'srcNetworkProfileARN'
--
-- * 'srcExtraDataPackageARN'
--
-- * 'srcAuxiliaryApps'
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

-- | Specifies the billing method for a test run: 'metered' or 'unmetered'.
-- If the parameter is not specified, the default value is 'unmetered'.
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

-- | The ARN of the extra data for the run. The extra data is a .zip file
-- that AWS Device Farm will extract to external data for Android or the
-- app\'s sandbox for iOS.
srcExtraDataPackageARN :: Lens' ScheduleRunConfiguration (Maybe Text)
srcExtraDataPackageARN = lens _srcExtraDataPackageARN (\ s a -> s{_srcExtraDataPackageARN = a});

-- | A list of auxiliary apps for the run.
srcAuxiliaryApps :: Lens' ScheduleRunConfiguration [Text]
srcAuxiliaryApps = lens _srcAuxiliaryApps (\ s a -> s{_srcAuxiliaryApps = a}) . _Default . _Coerce;

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
-- * 'srtTestPackageARN'
--
-- * 'srtParameters'
--
-- * 'srtFilter'
--
-- * 'srtType'
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

-- | The test\'s parameters, such as test framework parameters and fixture
-- settings.
srtParameters :: Lens' ScheduleRunTest (HashMap Text Text)
srtParameters = lens _srtParameters (\ s a -> s{_srtParameters = a}) . _Default . _Map;

-- | The test\'s filter.
srtFilter :: Lens' ScheduleRunTest (Maybe Text)
srtFilter = lens _srtFilter (\ s a -> s{_srtFilter = a});

-- | The test\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ: The built-in fuzz type.
--
-- -   BUILTIN_EXPLORER: For Android, an app explorer that will traverse an
--     Android app, interacting with it and capturing screenshots at the
--     same time.
--
-- -   APPIUM_JAVA_JUNIT: The Appium Java JUnit type.
--
-- -   APPIUM_JAVA_TESTNG: The Appium Java TestNG type.
--
-- -   CALABASH: The Calabash type.
--
-- -   INSTRUMENTATION: The Instrumentation type.
--
-- -   UIAUTOMATION: The uiautomation type.
--
-- -   UIAUTOMATOR: The uiautomator type.
--
-- -   XCTEST: The XCode test type.
--
srtType :: Lens' ScheduleRunTest TestType
srtType = lens _srtType (\ s a -> s{_srtType = a});

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
-- /See:/ 'suite' smart constructor.
data Suite = Suite'
    { _sStatus   :: !(Maybe ExecutionStatus)
    , _sCounters :: !(Maybe Counters)
    , _sArn      :: !(Maybe Text)
    , _sCreated  :: !(Maybe POSIX)
    , _sStopped  :: !(Maybe POSIX)
    , _sResult   :: !(Maybe ExecutionResult)
    , _sName     :: !(Maybe Text)
    , _sType     :: !(Maybe TestType)
    , _sMessage  :: !(Maybe Text)
    , _sStarted  :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Suite' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStatus'
--
-- * 'sCounters'
--
-- * 'sArn'
--
-- * 'sCreated'
--
-- * 'sStopped'
--
-- * 'sResult'
--
-- * 'sName'
--
-- * 'sType'
--
-- * 'sMessage'
--
-- * 'sStarted'
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
    , _sType = Nothing
    , _sMessage = Nothing
    , _sStarted = Nothing
    }

-- | The suite\'s status.
--
-- Allowed values include:
--
-- -   COMPLETED: A completed status.
--
-- -   PENDING: A pending status.
--
-- -   PROCESSING: A processing status.
--
-- -   RUNNING: A running status.
--
-- -   SCHEDULING: A scheduling status.
--
sStatus :: Lens' Suite (Maybe ExecutionStatus)
sStatus = lens _sStatus (\ s a -> s{_sStatus = a});

-- | The suite\'s result counters.
sCounters :: Lens' Suite (Maybe Counters)
sCounters = lens _sCounters (\ s a -> s{_sCounters = a});

-- | The suite\'s ARN.
sArn :: Lens' Suite (Maybe Text)
sArn = lens _sArn (\ s a -> s{_sArn = a});

-- | When the suite was created.
sCreated :: Lens' Suite (Maybe UTCTime)
sCreated = lens _sCreated (\ s a -> s{_sCreated = a}) . mapping _Time;

-- | The suite\'s stop time.
sStopped :: Lens' Suite (Maybe UTCTime)
sStopped = lens _sStopped (\ s a -> s{_sStopped = a}) . mapping _Time;

-- | The suite\'s result.
--
-- Allowed values include:
--
-- -   ERRORED: An error condition.
--
-- -   FAILED: A failed condition.
--
-- -   SKIPPED: A skipped condition.
--
-- -   STOPPED: A stopped condition.
--
-- -   PASSED: A passing condition.
--
-- -   PENDING: A pending condition.
--
-- -   WARNED: A warning condition.
--
sResult :: Lens' Suite (Maybe ExecutionResult)
sResult = lens _sResult (\ s a -> s{_sResult = a});

-- | The suite\'s name.
sName :: Lens' Suite (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a});

-- | The suite\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ: The built-in fuzz type.
--
-- -   BUILTIN_EXPLORER: For Android, an app explorer that will traverse an
--     Android app, interacting with it and capturing screenshots at the
--     same time.
--
-- -   APPIUM_JAVA_JUNIT: The Appium Java JUnit type.
--
-- -   APPIUM_JAVA_TESTNG: The Appium Java TestNG type.
--
-- -   CALABASH: The Calabash type.
--
-- -   INSTRUMENTATION: The Instrumentation type.
--
-- -   UIAUTOMATION: The uiautomation type.
--
-- -   UIAUTOMATOR: The uiautomator type.
--
-- -   XCTEST: The XCode test type.
--
sType :: Lens' Suite (Maybe TestType)
sType = lens _sType (\ s a -> s{_sType = a});

-- | A message about the suite\'s result.
sMessage :: Lens' Suite (Maybe Text)
sMessage = lens _sMessage (\ s a -> s{_sMessage = a});

-- | The suite\'s start time.
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
                     <*> (x .:? "type")
                     <*> (x .:? "message")
                     <*> (x .:? "started"))

-- | Represents a condition that is evaluated.
--
-- /See:/ 'test' smart constructor.
data Test = Test'
    { _tStatus   :: !(Maybe ExecutionStatus)
    , _tCounters :: !(Maybe Counters)
    , _tArn      :: !(Maybe Text)
    , _tCreated  :: !(Maybe POSIX)
    , _tStopped  :: !(Maybe POSIX)
    , _tResult   :: !(Maybe ExecutionResult)
    , _tName     :: !(Maybe Text)
    , _tType     :: !(Maybe TestType)
    , _tMessage  :: !(Maybe Text)
    , _tStarted  :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Test' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tStatus'
--
-- * 'tCounters'
--
-- * 'tArn'
--
-- * 'tCreated'
--
-- * 'tStopped'
--
-- * 'tResult'
--
-- * 'tName'
--
-- * 'tType'
--
-- * 'tMessage'
--
-- * 'tStarted'
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
    , _tType = Nothing
    , _tMessage = Nothing
    , _tStarted = Nothing
    }

-- | The test\'s status.
--
-- Allowed values include:
--
-- -   COMPLETED: A completed status.
--
-- -   PENDING: A pending status.
--
-- -   PROCESSING: A processing status.
--
-- -   RUNNING: A running status.
--
-- -   SCHEDULING: A scheduling status.
--
tStatus :: Lens' Test (Maybe ExecutionStatus)
tStatus = lens _tStatus (\ s a -> s{_tStatus = a});

-- | The test\'s result counters.
tCounters :: Lens' Test (Maybe Counters)
tCounters = lens _tCounters (\ s a -> s{_tCounters = a});

-- | The test\'s ARN.
tArn :: Lens' Test (Maybe Text)
tArn = lens _tArn (\ s a -> s{_tArn = a});

-- | When the test was created.
tCreated :: Lens' Test (Maybe UTCTime)
tCreated = lens _tCreated (\ s a -> s{_tCreated = a}) . mapping _Time;

-- | The test\'s stop time.
tStopped :: Lens' Test (Maybe UTCTime)
tStopped = lens _tStopped (\ s a -> s{_tStopped = a}) . mapping _Time;

-- | The test\'s result.
--
-- Allowed values include:
--
-- -   ERRORED: An error condition.
--
-- -   FAILED: A failed condition.
--
-- -   SKIPPED: A skipped condition.
--
-- -   STOPPED: A stopped condition.
--
-- -   PASSED: A passing condition.
--
-- -   PENDING: A pending condition.
--
-- -   WARNED: A warning condition.
--
tResult :: Lens' Test (Maybe ExecutionResult)
tResult = lens _tResult (\ s a -> s{_tResult = a});

-- | The test\'s name.
tName :: Lens' Test (Maybe Text)
tName = lens _tName (\ s a -> s{_tName = a});

-- | The test\'s type.
--
-- Must be one of the following values:
--
-- -   BUILTIN_FUZZ: The built-in fuzz type.
--
-- -   BUILTIN_EXPLORER: For Android, an app explorer that will traverse an
--     Android app, interacting with it and capturing screenshots at the
--     same time.
--
-- -   APPIUM_JAVA_JUNIT: The Appium Java JUnit type.
--
-- -   APPIUM_JAVA_TESTNG: The Appium Java TestNG type.
--
-- -   CALABASH: The Calabash type.
--
-- -   INSTRUMENTATION: The Instrumentation type.
--
-- -   UIAUTOMATION: The uiautomation type.
--
-- -   UIAUTOMATOR: The uiautomator type.
--
-- -   XCTEST: The XCode test type.
--
tType :: Lens' Test (Maybe TestType)
tType = lens _tType (\ s a -> s{_tType = a});

-- | A message about the test\'s result.
tMessage :: Lens' Test (Maybe Text)
tMessage = lens _tMessage (\ s a -> s{_tMessage = a});

-- | The test\'s start time.
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
                     <*> (x .:? "type")
                     <*> (x .:? "message")
                     <*> (x .:? "started"))

-- | A collection of one or more problems, grouped by their result.
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
-- * 'upProblems'
--
-- * 'upMessage'
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

-- | A message about the unique problems\' result.
upMessage :: Lens' UniqueProblem (Maybe Text)
upMessage = lens _upMessage (\ s a -> s{_upMessage = a});

instance FromJSON UniqueProblem where
        parseJSON
          = withObject "UniqueProblem"
              (\ x ->
                 UniqueProblem' <$>
                   (x .:? "problems" .!= mempty) <*> (x .:? "message"))

-- | An app or a set of one or more tests to upload or that have been
-- uploaded.
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
-- * 'uStatus'
--
-- * 'uArn'
--
-- * 'uCreated'
--
-- * 'uUrl'
--
-- * 'uName'
--
-- * 'uMetadata'
--
-- * 'uType'
--
-- * 'uMessage'
--
-- * 'uContentType'
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

-- | The upload\'s status.
--
-- Must be one of the following values:
--
-- -   FAILED: A failed status.
--
-- -   INITIALIZED: An initialized status.
--
-- -   PROCESSING: A processing status.
--
-- -   SUCCEEDED: A succeeded status.
--
uStatus :: Lens' Upload (Maybe UploadStatus)
uStatus = lens _uStatus (\ s a -> s{_uStatus = a});

-- | The upload\'s ARN.
uArn :: Lens' Upload (Maybe Text)
uArn = lens _uArn (\ s a -> s{_uArn = a});

-- | When the upload was created.
uCreated :: Lens' Upload (Maybe UTCTime)
uCreated = lens _uCreated (\ s a -> s{_uCreated = a}) . mapping _Time;

-- | The pre-signed Amazon S3 URL that was used to store a file through a
-- corresponding PUT request.
uUrl :: Lens' Upload (Maybe Text)
uUrl = lens _uUrl (\ s a -> s{_uUrl = a});

-- | The upload\'s file name.
uName :: Lens' Upload (Maybe Text)
uName = lens _uName (\ s a -> s{_uName = a});

-- | The upload\'s metadata. For example, for Android, this contains
-- information that is parsed from the manifest and is displayed in the AWS
-- Device Farm console after the associated app is uploaded.
uMetadata :: Lens' Upload (Maybe Text)
uMetadata = lens _uMetadata (\ s a -> s{_uMetadata = a});

-- | The upload\'s type.
--
-- Must be one of the following values:
--
-- -   ANDROID_APP: An Android upload.
--
-- -   IOS_APP: An iOS upload.
--
-- -   EXTERNAL_DATA: An external data upload.
--
-- -   APPIUM_JAVA_JUNIT_TEST_PACKAGE: An Appium Java JUnit test package
--     upload.
--
-- -   APPIUM_JAVA_TESTNG_TEST_PACKAGE: An Appium Java TestNG test package
--     upload.
--
-- -   CALABASH_TEST_PACKAGE: A Calabash test package upload.
--
-- -   INSTRUMENTATION_TEST_PACKAGE: An instrumentation upload.
--
-- -   UIAUTOMATOR_TEST_PACKAGE: A uiautomator test package upload.
--
-- -   XCTEST_TEST_PACKAGE: An XCode test package upload.
--
uType :: Lens' Upload (Maybe UploadType)
uType = lens _uType (\ s a -> s{_uType = a});

-- | A message about the upload\'s result.
uMessage :: Lens' Upload (Maybe Text)
uMessage = lens _uMessage (\ s a -> s{_uMessage = a});

-- | The upload\'s content type (for example, \"application\/octet-stream\").
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
