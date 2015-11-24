{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Sum where

import           Network.AWS.Prelude

data ArtifactCategory
    = ACFile
    | ACLog
    | ACScreenshot
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ArtifactCategory where
    parser = takeLowerText >>= \case
        "file" -> pure ACFile
        "log" -> pure ACLog
        "screenshot" -> pure ACScreenshot
        e -> fromTextError $ "Failure parsing ArtifactCategory from value: '" <> e
           <> "'. Accepted values: FILE, LOG, SCREENSHOT"

instance ToText ArtifactCategory where
    toText = \case
        ACFile -> "FILE"
        ACLog -> "LOG"
        ACScreenshot -> "SCREENSHOT"

instance Hashable     ArtifactCategory
instance ToByteString ArtifactCategory
instance ToQuery      ArtifactCategory
instance ToHeader     ArtifactCategory

instance ToJSON ArtifactCategory where
    toJSON = toJSONText

data ArtifactType
    = AppiumJavaOutput
    | AppiumJavaXMLOutput
    | AppiumServerOutput
    | ApplicationCrashReport
    | AutomationOutput
    | CalabashJSONOutput
    | CalabashJavaXMLOutput
    | CalabashPrettyOutput
    | CalabashStandardOutput
    | DeviceLog
    | ExerciserMonkeyOutput
    | ExplorerEventLog
    | ExplorerSummaryLog
    | InstrumentationOutput
    | MessageLog
    | ResultLog
    | Screenshot
    | ServiceLog
    | Unknown
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ArtifactType where
    parser = takeLowerText >>= \case
        "appium_java_output" -> pure AppiumJavaOutput
        "appium_java_xml_output" -> pure AppiumJavaXMLOutput
        "appium_server_output" -> pure AppiumServerOutput
        "application_crash_report" -> pure ApplicationCrashReport
        "automation_output" -> pure AutomationOutput
        "calabash_json_output" -> pure CalabashJSONOutput
        "calabash_java_xml_output" -> pure CalabashJavaXMLOutput
        "calabash_pretty_output" -> pure CalabashPrettyOutput
        "calabash_standard_output" -> pure CalabashStandardOutput
        "device_log" -> pure DeviceLog
        "exerciser_monkey_output" -> pure ExerciserMonkeyOutput
        "explorer_event_log" -> pure ExplorerEventLog
        "explorer_summary_log" -> pure ExplorerSummaryLog
        "instrumentation_output" -> pure InstrumentationOutput
        "message_log" -> pure MessageLog
        "result_log" -> pure ResultLog
        "screenshot" -> pure Screenshot
        "service_log" -> pure ServiceLog
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing ArtifactType from value: '" <> e
           <> "'. Accepted values: APPIUM_JAVA_OUTPUT, APPIUM_JAVA_XML_OUTPUT, APPIUM_SERVER_OUTPUT, APPLICATION_CRASH_REPORT, AUTOMATION_OUTPUT, CALABASH_JSON_OUTPUT, CALABASH_JAVA_XML_OUTPUT, CALABASH_PRETTY_OUTPUT, CALABASH_STANDARD_OUTPUT, DEVICE_LOG, EXERCISER_MONKEY_OUTPUT, EXPLORER_EVENT_LOG, EXPLORER_SUMMARY_LOG, INSTRUMENTATION_OUTPUT, MESSAGE_LOG, RESULT_LOG, SCREENSHOT, SERVICE_LOG, UNKNOWN"

instance ToText ArtifactType where
    toText = \case
        AppiumJavaOutput -> "APPIUM_JAVA_OUTPUT"
        AppiumJavaXMLOutput -> "APPIUM_JAVA_XML_OUTPUT"
        AppiumServerOutput -> "APPIUM_SERVER_OUTPUT"
        ApplicationCrashReport -> "APPLICATION_CRASH_REPORT"
        AutomationOutput -> "AUTOMATION_OUTPUT"
        CalabashJSONOutput -> "CALABASH_JSON_OUTPUT"
        CalabashJavaXMLOutput -> "CALABASH_JAVA_XML_OUTPUT"
        CalabashPrettyOutput -> "CALABASH_PRETTY_OUTPUT"
        CalabashStandardOutput -> "CALABASH_STANDARD_OUTPUT"
        DeviceLog -> "DEVICE_LOG"
        ExerciserMonkeyOutput -> "EXERCISER_MONKEY_OUTPUT"
        ExplorerEventLog -> "EXPLORER_EVENT_LOG"
        ExplorerSummaryLog -> "EXPLORER_SUMMARY_LOG"
        InstrumentationOutput -> "INSTRUMENTATION_OUTPUT"
        MessageLog -> "MESSAGE_LOG"
        ResultLog -> "RESULT_LOG"
        Screenshot -> "SCREENSHOT"
        ServiceLog -> "SERVICE_LOG"
        Unknown -> "UNKNOWN"

instance Hashable     ArtifactType
instance ToByteString ArtifactType
instance ToQuery      ArtifactType
instance ToHeader     ArtifactType

instance FromJSON ArtifactType where
    parseJSON = parseJSONText "ArtifactType"

data BillingMethod
    = Metered
    | Unmetered
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText BillingMethod where
    parser = takeLowerText >>= \case
        "metered" -> pure Metered
        "unmetered" -> pure Unmetered
        e -> fromTextError $ "Failure parsing BillingMethod from value: '" <> e
           <> "'. Accepted values: METERED, UNMETERED"

instance ToText BillingMethod where
    toText = \case
        Metered -> "METERED"
        Unmetered -> "UNMETERED"

instance Hashable     BillingMethod
instance ToByteString BillingMethod
instance ToQuery      BillingMethod
instance ToHeader     BillingMethod

instance ToJSON BillingMethod where
    toJSON = toJSONText

instance FromJSON BillingMethod where
    parseJSON = parseJSONText "BillingMethod"

data DeviceAttribute
    = ARN
    | FormFactor
    | Manufacturer
    | Platform
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DeviceAttribute where
    parser = takeLowerText >>= \case
        "arn" -> pure ARN
        "form_factor" -> pure FormFactor
        "manufacturer" -> pure Manufacturer
        "platform" -> pure Platform
        e -> fromTextError $ "Failure parsing DeviceAttribute from value: '" <> e
           <> "'. Accepted values: ARN, FORM_FACTOR, MANUFACTURER, PLATFORM"

instance ToText DeviceAttribute where
    toText = \case
        ARN -> "ARN"
        FormFactor -> "FORM_FACTOR"
        Manufacturer -> "MANUFACTURER"
        Platform -> "PLATFORM"

instance Hashable     DeviceAttribute
instance ToByteString DeviceAttribute
instance ToQuery      DeviceAttribute
instance ToHeader     DeviceAttribute

instance ToJSON DeviceAttribute where
    toJSON = toJSONText

instance FromJSON DeviceAttribute where
    parseJSON = parseJSONText "DeviceAttribute"

data DeviceFormFactor
    = Phone
    | Tablet
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DeviceFormFactor where
    parser = takeLowerText >>= \case
        "phone" -> pure Phone
        "tablet" -> pure Tablet
        e -> fromTextError $ "Failure parsing DeviceFormFactor from value: '" <> e
           <> "'. Accepted values: PHONE, TABLET"

instance ToText DeviceFormFactor where
    toText = \case
        Phone -> "PHONE"
        Tablet -> "TABLET"

instance Hashable     DeviceFormFactor
instance ToByteString DeviceFormFactor
instance ToQuery      DeviceFormFactor
instance ToHeader     DeviceFormFactor

instance FromJSON DeviceFormFactor where
    parseJSON = parseJSONText "DeviceFormFactor"

data DevicePlatform
    = Android
    | Ios
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DevicePlatform where
    parser = takeLowerText >>= \case
        "android" -> pure Android
        "ios" -> pure Ios
        e -> fromTextError $ "Failure parsing DevicePlatform from value: '" <> e
           <> "'. Accepted values: ANDROID, IOS"

instance ToText DevicePlatform where
    toText = \case
        Android -> "ANDROID"
        Ios -> "IOS"

instance Hashable     DevicePlatform
instance ToByteString DevicePlatform
instance ToQuery      DevicePlatform
instance ToHeader     DevicePlatform

instance FromJSON DevicePlatform where
    parseJSON = parseJSONText "DevicePlatform"

data DevicePoolType
    = Curated
    | Private
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DevicePoolType where
    parser = takeLowerText >>= \case
        "curated" -> pure Curated
        "private" -> pure Private
        e -> fromTextError $ "Failure parsing DevicePoolType from value: '" <> e
           <> "'. Accepted values: CURATED, PRIVATE"

instance ToText DevicePoolType where
    toText = \case
        Curated -> "CURATED"
        Private -> "PRIVATE"

instance Hashable     DevicePoolType
instance ToByteString DevicePoolType
instance ToQuery      DevicePoolType
instance ToHeader     DevicePoolType

instance ToJSON DevicePoolType where
    toJSON = toJSONText

instance FromJSON DevicePoolType where
    parseJSON = parseJSONText "DevicePoolType"

data ExecutionResult
    = ERErrored
    | ERFailed
    | ERPassed
    | ERPending
    | ERSkipped
    | ERStopped
    | ERWarned
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ExecutionResult where
    parser = takeLowerText >>= \case
        "errored" -> pure ERErrored
        "failed" -> pure ERFailed
        "passed" -> pure ERPassed
        "pending" -> pure ERPending
        "skipped" -> pure ERSkipped
        "stopped" -> pure ERStopped
        "warned" -> pure ERWarned
        e -> fromTextError $ "Failure parsing ExecutionResult from value: '" <> e
           <> "'. Accepted values: ERRORED, FAILED, PASSED, PENDING, SKIPPED, STOPPED, WARNED"

instance ToText ExecutionResult where
    toText = \case
        ERErrored -> "ERRORED"
        ERFailed -> "FAILED"
        ERPassed -> "PASSED"
        ERPending -> "PENDING"
        ERSkipped -> "SKIPPED"
        ERStopped -> "STOPPED"
        ERWarned -> "WARNED"

instance Hashable     ExecutionResult
instance ToByteString ExecutionResult
instance ToQuery      ExecutionResult
instance ToHeader     ExecutionResult

instance FromJSON ExecutionResult where
    parseJSON = parseJSONText "ExecutionResult"

data ExecutionStatus
    = Completed
    | Pending
    | Processing
    | Running
    | Scheduling
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ExecutionStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "pending" -> pure Pending
        "processing" -> pure Processing
        "running" -> pure Running
        "scheduling" -> pure Scheduling
        e -> fromTextError $ "Failure parsing ExecutionStatus from value: '" <> e
           <> "'. Accepted values: COMPLETED, PENDING, PROCESSING, RUNNING, SCHEDULING"

instance ToText ExecutionStatus where
    toText = \case
        Completed -> "COMPLETED"
        Pending -> "PENDING"
        Processing -> "PROCESSING"
        Running -> "RUNNING"
        Scheduling -> "SCHEDULING"

instance Hashable     ExecutionStatus
instance ToByteString ExecutionStatus
instance ToQuery      ExecutionStatus
instance ToHeader     ExecutionStatus

instance FromJSON ExecutionStatus where
    parseJSON = parseJSONText "ExecutionStatus"

data RuleOperator
    = Equals
    | GreaterThan
    | IN
    | LessThan
    | NotIn
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText RuleOperator where
    parser = takeLowerText >>= \case
        "equals" -> pure Equals
        "greater_than" -> pure GreaterThan
        "in" -> pure IN
        "less_than" -> pure LessThan
        "not_in" -> pure NotIn
        e -> fromTextError $ "Failure parsing RuleOperator from value: '" <> e
           <> "'. Accepted values: EQUALS, GREATER_THAN, IN, LESS_THAN, NOT_IN"

instance ToText RuleOperator where
    toText = \case
        Equals -> "EQUALS"
        GreaterThan -> "GREATER_THAN"
        IN -> "IN"
        LessThan -> "LESS_THAN"
        NotIn -> "NOT_IN"

instance Hashable     RuleOperator
instance ToByteString RuleOperator
instance ToQuery      RuleOperator
instance ToHeader     RuleOperator

instance ToJSON RuleOperator where
    toJSON = toJSONText

instance FromJSON RuleOperator where
    parseJSON = parseJSONText "RuleOperator"

data SampleType
    = CPU
    | Memory
    | NativeAvgDrawtime
    | NativeFps
    | NativeFrames
    | NativeMaxDrawtime
    | NativeMinDrawtime
    | OpenglAvgDrawtime
    | OpenglFps
    | OpenglFrames
    | OpenglMaxDrawtime
    | OpenglMinDrawtime
    | RX
    | RxRate
    | TX
    | Threads
    | TxRate
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText SampleType where
    parser = takeLowerText >>= \case
        "cpu" -> pure CPU
        "memory" -> pure Memory
        "native_avg_drawtime" -> pure NativeAvgDrawtime
        "native_fps" -> pure NativeFps
        "native_frames" -> pure NativeFrames
        "native_max_drawtime" -> pure NativeMaxDrawtime
        "native_min_drawtime" -> pure NativeMinDrawtime
        "opengl_avg_drawtime" -> pure OpenglAvgDrawtime
        "opengl_fps" -> pure OpenglFps
        "opengl_frames" -> pure OpenglFrames
        "opengl_max_drawtime" -> pure OpenglMaxDrawtime
        "opengl_min_drawtime" -> pure OpenglMinDrawtime
        "rx" -> pure RX
        "rx_rate" -> pure RxRate
        "tx" -> pure TX
        "threads" -> pure Threads
        "tx_rate" -> pure TxRate
        e -> fromTextError $ "Failure parsing SampleType from value: '" <> e
           <> "'. Accepted values: CPU, MEMORY, NATIVE_AVG_DRAWTIME, NATIVE_FPS, NATIVE_FRAMES, NATIVE_MAX_DRAWTIME, NATIVE_MIN_DRAWTIME, OPENGL_AVG_DRAWTIME, OPENGL_FPS, OPENGL_FRAMES, OPENGL_MAX_DRAWTIME, OPENGL_MIN_DRAWTIME, RX, RX_RATE, TX, THREADS, TX_RATE"

instance ToText SampleType where
    toText = \case
        CPU -> "CPU"
        Memory -> "MEMORY"
        NativeAvgDrawtime -> "NATIVE_AVG_DRAWTIME"
        NativeFps -> "NATIVE_FPS"
        NativeFrames -> "NATIVE_FRAMES"
        NativeMaxDrawtime -> "NATIVE_MAX_DRAWTIME"
        NativeMinDrawtime -> "NATIVE_MIN_DRAWTIME"
        OpenglAvgDrawtime -> "OPENGL_AVG_DRAWTIME"
        OpenglFps -> "OPENGL_FPS"
        OpenglFrames -> "OPENGL_FRAMES"
        OpenglMaxDrawtime -> "OPENGL_MAX_DRAWTIME"
        OpenglMinDrawtime -> "OPENGL_MIN_DRAWTIME"
        RX -> "RX"
        RxRate -> "RX_RATE"
        TX -> "TX"
        Threads -> "THREADS"
        TxRate -> "TX_RATE"

instance Hashable     SampleType
instance ToByteString SampleType
instance ToQuery      SampleType
instance ToHeader     SampleType

instance FromJSON SampleType where
    parseJSON = parseJSONText "SampleType"

data TestType
    = AppiumJavaJunit
    | AppiumJavaTestng
    | BuiltinExplorer
    | BuiltinFuzz
    | Calabash
    | Instrumentation
    | Uiautomation
    | Uiautomator
    | Xctest
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText TestType where
    parser = takeLowerText >>= \case
        "appium_java_junit" -> pure AppiumJavaJunit
        "appium_java_testng" -> pure AppiumJavaTestng
        "builtin_explorer" -> pure BuiltinExplorer
        "builtin_fuzz" -> pure BuiltinFuzz
        "calabash" -> pure Calabash
        "instrumentation" -> pure Instrumentation
        "uiautomation" -> pure Uiautomation
        "uiautomator" -> pure Uiautomator
        "xctest" -> pure Xctest
        e -> fromTextError $ "Failure parsing TestType from value: '" <> e
           <> "'. Accepted values: APPIUM_JAVA_JUNIT, APPIUM_JAVA_TESTNG, BUILTIN_EXPLORER, BUILTIN_FUZZ, CALABASH, INSTRUMENTATION, UIAUTOMATION, UIAUTOMATOR, XCTEST"

instance ToText TestType where
    toText = \case
        AppiumJavaJunit -> "APPIUM_JAVA_JUNIT"
        AppiumJavaTestng -> "APPIUM_JAVA_TESTNG"
        BuiltinExplorer -> "BUILTIN_EXPLORER"
        BuiltinFuzz -> "BUILTIN_FUZZ"
        Calabash -> "CALABASH"
        Instrumentation -> "INSTRUMENTATION"
        Uiautomation -> "UIAUTOMATION"
        Uiautomator -> "UIAUTOMATOR"
        Xctest -> "XCTEST"

instance Hashable     TestType
instance ToByteString TestType
instance ToQuery      TestType
instance ToHeader     TestType

instance ToJSON TestType where
    toJSON = toJSONText

instance FromJSON TestType where
    parseJSON = parseJSONText "TestType"

data UploadStatus
    = USFailed
    | USInitialized
    | USProcessing
    | USSucceeded
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText UploadStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure USFailed
        "initialized" -> pure USInitialized
        "processing" -> pure USProcessing
        "succeeded" -> pure USSucceeded
        e -> fromTextError $ "Failure parsing UploadStatus from value: '" <> e
           <> "'. Accepted values: FAILED, INITIALIZED, PROCESSING, SUCCEEDED"

instance ToText UploadStatus where
    toText = \case
        USFailed -> "FAILED"
        USInitialized -> "INITIALIZED"
        USProcessing -> "PROCESSING"
        USSucceeded -> "SUCCEEDED"

instance Hashable     UploadStatus
instance ToByteString UploadStatus
instance ToQuery      UploadStatus
instance ToHeader     UploadStatus

instance FromJSON UploadStatus where
    parseJSON = parseJSONText "UploadStatus"

data UploadType
    = AndroidApp
    | AppiumJavaJunitTestPackage
    | AppiumJavaTestngTestPackage
    | CalabashTestPackage
    | ExternalData
    | InstrumentationTestPackage
    | IosApp
    | UiautomationTestPackage
    | UiautomatorTestPackage
    | XctestTestPackage
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText UploadType where
    parser = takeLowerText >>= \case
        "android_app" -> pure AndroidApp
        "appium_java_junit_test_package" -> pure AppiumJavaJunitTestPackage
        "appium_java_testng_test_package" -> pure AppiumJavaTestngTestPackage
        "calabash_test_package" -> pure CalabashTestPackage
        "external_data" -> pure ExternalData
        "instrumentation_test_package" -> pure InstrumentationTestPackage
        "ios_app" -> pure IosApp
        "uiautomation_test_package" -> pure UiautomationTestPackage
        "uiautomator_test_package" -> pure UiautomatorTestPackage
        "xctest_test_package" -> pure XctestTestPackage
        e -> fromTextError $ "Failure parsing UploadType from value: '" <> e
           <> "'. Accepted values: ANDROID_APP, APPIUM_JAVA_JUNIT_TEST_PACKAGE, APPIUM_JAVA_TESTNG_TEST_PACKAGE, CALABASH_TEST_PACKAGE, EXTERNAL_DATA, INSTRUMENTATION_TEST_PACKAGE, IOS_APP, UIAUTOMATION_TEST_PACKAGE, UIAUTOMATOR_TEST_PACKAGE, XCTEST_TEST_PACKAGE"

instance ToText UploadType where
    toText = \case
        AndroidApp -> "ANDROID_APP"
        AppiumJavaJunitTestPackage -> "APPIUM_JAVA_JUNIT_TEST_PACKAGE"
        AppiumJavaTestngTestPackage -> "APPIUM_JAVA_TESTNG_TEST_PACKAGE"
        CalabashTestPackage -> "CALABASH_TEST_PACKAGE"
        ExternalData -> "EXTERNAL_DATA"
        InstrumentationTestPackage -> "INSTRUMENTATION_TEST_PACKAGE"
        IosApp -> "IOS_APP"
        UiautomationTestPackage -> "UIAUTOMATION_TEST_PACKAGE"
        UiautomatorTestPackage -> "UIAUTOMATOR_TEST_PACKAGE"
        XctestTestPackage -> "XCTEST_TEST_PACKAGE"

instance Hashable     UploadType
instance ToByteString UploadType
instance ToQuery      UploadType
instance ToHeader     UploadType

instance ToJSON UploadType where
    toJSON = toJSONText

instance FromJSON UploadType where
    parseJSON = parseJSONText "UploadType"
