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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Sum where

import           Network.AWS.Prelude

data ArtifactCategory
    = ACFile
    | ACLog
    | ACScreenshot
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ArtifactCategory where
    parser = takeLowerText >>= \case
        "file" -> pure ACFile
        "log" -> pure ACLog
        "screenshot" -> pure ACScreenshot
        e -> fromTextError $ "Failure parsing ArtifactCategory from value: '" <> e
           <> "'. Accepted values: file, log, screenshot"

instance ToText ArtifactCategory where
    toText = \case
        ACFile -> "file"
        ACLog -> "log"
        ACScreenshot -> "screenshot"

instance Hashable     ArtifactCategory
instance ToByteString ArtifactCategory
instance ToPath       ArtifactCategory
instance ToQuery      ArtifactCategory
instance ToHeader     ArtifactCategory

instance ToJSON ArtifactCategory where
    toJSON = toJSONText

data ArtifactType
    = DeviceLog
    | Screenshot
    | MessageLog
    | AppiumServerOutput
    | CalabashJavaXMLOutput
    | Unknown
    | ExerciserMonkeyOutput
    | CalabashPrettyOutput
    | InstrumentationOutput
    | AppiumJavaXMLOutput
    | ResultLog
    | CalabashJSONOutput
    | AutomationOutput
    | CalabashStandardOutput
    | AppiumJavaOutput
    | ServiceLog
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ArtifactType where
    parser = takeLowerText >>= \case
        "appium_java_output" -> pure AppiumJavaOutput
        "appium_java_xml_output" -> pure AppiumJavaXMLOutput
        "appium_server_output" -> pure AppiumServerOutput
        "automation_output" -> pure AutomationOutput
        "calabash_json_output" -> pure CalabashJSONOutput
        "calabash_java_xml_output" -> pure CalabashJavaXMLOutput
        "calabash_pretty_output" -> pure CalabashPrettyOutput
        "calabash_standard_output" -> pure CalabashStandardOutput
        "device_log" -> pure DeviceLog
        "exerciser_monkey_output" -> pure ExerciserMonkeyOutput
        "instrumentation_output" -> pure InstrumentationOutput
        "message_log" -> pure MessageLog
        "result_log" -> pure ResultLog
        "screenshot" -> pure Screenshot
        "service_log" -> pure ServiceLog
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing ArtifactType from value: '" <> e
           <> "'. Accepted values: appium_java_output, appium_java_xml_output, appium_server_output, automation_output, calabash_json_output, calabash_java_xml_output, calabash_pretty_output, calabash_standard_output, device_log, exerciser_monkey_output, instrumentation_output, message_log, result_log, screenshot, service_log, unknown"

instance ToText ArtifactType where
    toText = \case
        AppiumJavaOutput -> "appium_java_output"
        AppiumJavaXMLOutput -> "appium_java_xml_output"
        AppiumServerOutput -> "appium_server_output"
        AutomationOutput -> "automation_output"
        CalabashJSONOutput -> "calabash_json_output"
        CalabashJavaXMLOutput -> "calabash_java_xml_output"
        CalabashPrettyOutput -> "calabash_pretty_output"
        CalabashStandardOutput -> "calabash_standard_output"
        DeviceLog -> "device_log"
        ExerciserMonkeyOutput -> "exerciser_monkey_output"
        InstrumentationOutput -> "instrumentation_output"
        MessageLog -> "message_log"
        ResultLog -> "result_log"
        Screenshot -> "screenshot"
        ServiceLog -> "service_log"
        Unknown -> "unknown"

instance Hashable     ArtifactType
instance ToByteString ArtifactType
instance ToPath       ArtifactType
instance ToQuery      ArtifactType
instance ToHeader     ArtifactType

instance FromJSON ArtifactType where
    parseJSON = parseJSONText "ArtifactType"

data DeviceAttribute
    = Platform
    | Manufacturer
    | ARN
    | FormFactor
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DeviceAttribute where
    parser = takeLowerText >>= \case
        "arn" -> pure ARN
        "form_factor" -> pure FormFactor
        "manufacturer" -> pure Manufacturer
        "platform" -> pure Platform
        e -> fromTextError $ "Failure parsing DeviceAttribute from value: '" <> e
           <> "'. Accepted values: arn, form_factor, manufacturer, platform"

instance ToText DeviceAttribute where
    toText = \case
        ARN -> "arn"
        FormFactor -> "form_factor"
        Manufacturer -> "manufacturer"
        Platform -> "platform"

instance Hashable     DeviceAttribute
instance ToByteString DeviceAttribute
instance ToPath       DeviceAttribute
instance ToQuery      DeviceAttribute
instance ToHeader     DeviceAttribute

instance ToJSON DeviceAttribute where
    toJSON = toJSONText

instance FromJSON DeviceAttribute where
    parseJSON = parseJSONText "DeviceAttribute"

data DeviceFormFactor
    = Phone
    | Tablet
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DeviceFormFactor where
    parser = takeLowerText >>= \case
        "phone" -> pure Phone
        "tablet" -> pure Tablet
        e -> fromTextError $ "Failure parsing DeviceFormFactor from value: '" <> e
           <> "'. Accepted values: phone, tablet"

instance ToText DeviceFormFactor where
    toText = \case
        Phone -> "phone"
        Tablet -> "tablet"

instance Hashable     DeviceFormFactor
instance ToByteString DeviceFormFactor
instance ToPath       DeviceFormFactor
instance ToQuery      DeviceFormFactor
instance ToHeader     DeviceFormFactor

instance FromJSON DeviceFormFactor where
    parseJSON = parseJSONText "DeviceFormFactor"

data DevicePlatform =
    Android
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DevicePlatform where
    parser = takeLowerText >>= \case
        "android" -> pure Android
        e -> fromTextError $ "Failure parsing DevicePlatform from value: '" <> e
           <> "'. Accepted values: android"

instance ToText DevicePlatform where
    toText = \case
        Android -> "android"

instance Hashable     DevicePlatform
instance ToByteString DevicePlatform
instance ToPath       DevicePlatform
instance ToQuery      DevicePlatform
instance ToHeader     DevicePlatform

instance FromJSON DevicePlatform where
    parseJSON = parseJSONText "DevicePlatform"

data DevicePoolType
    = Private
    | Curated
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DevicePoolType where
    parser = takeLowerText >>= \case
        "curated" -> pure Curated
        "private" -> pure Private
        e -> fromTextError $ "Failure parsing DevicePoolType from value: '" <> e
           <> "'. Accepted values: curated, private"

instance ToText DevicePoolType where
    toText = \case
        Curated -> "curated"
        Private -> "private"

instance Hashable     DevicePoolType
instance ToByteString DevicePoolType
instance ToPath       DevicePoolType
instance ToQuery      DevicePoolType
instance ToHeader     DevicePoolType

instance ToJSON DevicePoolType where
    toJSON = toJSONText

instance FromJSON DevicePoolType where
    parseJSON = parseJSONText "DevicePoolType"

data ExecutionResult
    = ERFailed
    | ERSkipped
    | ERPending
    | ERErrored
    | ERWarned
    | ERPassed
    | ERStopped
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
           <> "'. Accepted values: errored, failed, passed, pending, skipped, stopped, warned"

instance ToText ExecutionResult where
    toText = \case
        ERErrored -> "errored"
        ERFailed -> "failed"
        ERPassed -> "passed"
        ERPending -> "pending"
        ERSkipped -> "skipped"
        ERStopped -> "stopped"
        ERWarned -> "warned"

instance Hashable     ExecutionResult
instance ToByteString ExecutionResult
instance ToPath       ExecutionResult
instance ToQuery      ExecutionResult
instance ToHeader     ExecutionResult

instance FromJSON ExecutionResult where
    parseJSON = parseJSONText "ExecutionResult"

data ExecutionStatus
    = Pending
    | Running
    | Completed
    | Scheduling
    | Processing
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ExecutionStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "pending" -> pure Pending
        "processing" -> pure Processing
        "running" -> pure Running
        "scheduling" -> pure Scheduling
        e -> fromTextError $ "Failure parsing ExecutionStatus from value: '" <> e
           <> "'. Accepted values: completed, pending, processing, running, scheduling"

instance ToText ExecutionStatus where
    toText = \case
        Completed -> "completed"
        Pending -> "pending"
        Processing -> "processing"
        Running -> "running"
        Scheduling -> "scheduling"

instance Hashable     ExecutionStatus
instance ToByteString ExecutionStatus
instance ToPath       ExecutionStatus
instance ToQuery      ExecutionStatus
instance ToHeader     ExecutionStatus

instance FromJSON ExecutionStatus where
    parseJSON = parseJSONText "ExecutionStatus"

data RuleOperator
    = LessThan
    | IN
    | Equals
    | NotIn
    | GreaterThan
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RuleOperator where
    parser = takeLowerText >>= \case
        "equals" -> pure Equals
        "greater_than" -> pure GreaterThan
        "in" -> pure IN
        "less_than" -> pure LessThan
        "not_in" -> pure NotIn
        e -> fromTextError $ "Failure parsing RuleOperator from value: '" <> e
           <> "'. Accepted values: equals, greater_than, in, less_than, not_in"

instance ToText RuleOperator where
    toText = \case
        Equals -> "equals"
        GreaterThan -> "greater_than"
        IN -> "in"
        LessThan -> "less_than"
        NotIn -> "not_in"

instance Hashable     RuleOperator
instance ToByteString RuleOperator
instance ToPath       RuleOperator
instance ToQuery      RuleOperator
instance ToHeader     RuleOperator

instance ToJSON RuleOperator where
    toJSON = toJSONText

instance FromJSON RuleOperator where
    parseJSON = parseJSONText "RuleOperator"

data SampleType
    = TX
    | NativeAvgDrawtime
    | NativeFps
    | RX
    | OpenglFps
    | OpenglAvgDrawtime
    | NativeMaxDrawtime
    | TxRate
    | NativeMinDrawtime
    | Memory
    | NativeFrames
    | Threads
    | RxRate
    | OpenglMinDrawtime
    | CPU
    | OpenglMaxDrawtime
    | OpenglFrames
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
           <> "'. Accepted values: cpu, memory, native_avg_drawtime, native_fps, native_frames, native_max_drawtime, native_min_drawtime, opengl_avg_drawtime, opengl_fps, opengl_frames, opengl_max_drawtime, opengl_min_drawtime, rx, rx_rate, tx, threads, tx_rate"

instance ToText SampleType where
    toText = \case
        CPU -> "cpu"
        Memory -> "memory"
        NativeAvgDrawtime -> "native_avg_drawtime"
        NativeFps -> "native_fps"
        NativeFrames -> "native_frames"
        NativeMaxDrawtime -> "native_max_drawtime"
        NativeMinDrawtime -> "native_min_drawtime"
        OpenglAvgDrawtime -> "opengl_avg_drawtime"
        OpenglFps -> "opengl_fps"
        OpenglFrames -> "opengl_frames"
        OpenglMaxDrawtime -> "opengl_max_drawtime"
        OpenglMinDrawtime -> "opengl_min_drawtime"
        RX -> "rx"
        RxRate -> "rx_rate"
        TX -> "tx"
        Threads -> "threads"
        TxRate -> "tx_rate"

instance Hashable     SampleType
instance ToByteString SampleType
instance ToPath       SampleType
instance ToQuery      SampleType
instance ToHeader     SampleType

instance FromJSON SampleType where
    parseJSON = parseJSONText "SampleType"

data TestType
    = Calabash
    | Instrumentation
    | AppiumJavaJunit
    | AppiumJavaTestng
    | Uiautomator
    | BuiltinExplorer
    | BuiltinFuzz
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText TestType where
    parser = takeLowerText >>= \case
        "appium_java_junit" -> pure AppiumJavaJunit
        "appium_java_testng" -> pure AppiumJavaTestng
        "builtin_explorer" -> pure BuiltinExplorer
        "builtin_fuzz" -> pure BuiltinFuzz
        "calabash" -> pure Calabash
        "instrumentation" -> pure Instrumentation
        "uiautomator" -> pure Uiautomator
        e -> fromTextError $ "Failure parsing TestType from value: '" <> e
           <> "'. Accepted values: appium_java_junit, appium_java_testng, builtin_explorer, builtin_fuzz, calabash, instrumentation, uiautomator"

instance ToText TestType where
    toText = \case
        AppiumJavaJunit -> "appium_java_junit"
        AppiumJavaTestng -> "appium_java_testng"
        BuiltinExplorer -> "builtin_explorer"
        BuiltinFuzz -> "builtin_fuzz"
        Calabash -> "calabash"
        Instrumentation -> "instrumentation"
        Uiautomator -> "uiautomator"

instance Hashable     TestType
instance ToByteString TestType
instance ToPath       TestType
instance ToQuery      TestType
instance ToHeader     TestType

instance ToJSON TestType where
    toJSON = toJSONText

instance FromJSON TestType where
    parseJSON = parseJSONText "TestType"

data UploadStatus
    = USInitialized
    | USFailed
    | USProcessing
    | USSucceeded
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText UploadStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure USFailed
        "initialized" -> pure USInitialized
        "processing" -> pure USProcessing
        "succeeded" -> pure USSucceeded
        e -> fromTextError $ "Failure parsing UploadStatus from value: '" <> e
           <> "'. Accepted values: failed, initialized, processing, succeeded"

instance ToText UploadStatus where
    toText = \case
        USFailed -> "failed"
        USInitialized -> "initialized"
        USProcessing -> "processing"
        USSucceeded -> "succeeded"

instance Hashable     UploadStatus
instance ToByteString UploadStatus
instance ToPath       UploadStatus
instance ToQuery      UploadStatus
instance ToHeader     UploadStatus

instance FromJSON UploadStatus where
    parseJSON = parseJSONText "UploadStatus"

data UploadType
    = AppiumJavaJunitTestPackage
    | InstrumentationTestPackage
    | AndroidApp
    | ExternalData
    | AppiumJavaTestngTestPackage
    | UiautomatorTestPackage
    | CalabashTestPackage
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText UploadType where
    parser = takeLowerText >>= \case
        "android_app" -> pure AndroidApp
        "appium_java_junit_test_package" -> pure AppiumJavaJunitTestPackage
        "appium_java_testng_test_package" -> pure AppiumJavaTestngTestPackage
        "calabash_test_package" -> pure CalabashTestPackage
        "external_data" -> pure ExternalData
        "instrumentation_test_package" -> pure InstrumentationTestPackage
        "uiautomator_test_package" -> pure UiautomatorTestPackage
        e -> fromTextError $ "Failure parsing UploadType from value: '" <> e
           <> "'. Accepted values: android_app, appium_java_junit_test_package, appium_java_testng_test_package, calabash_test_package, external_data, instrumentation_test_package, uiautomator_test_package"

instance ToText UploadType where
    toText = \case
        AndroidApp -> "android_app"
        AppiumJavaJunitTestPackage -> "appium_java_junit_test_package"
        AppiumJavaTestngTestPackage -> "appium_java_testng_test_package"
        CalabashTestPackage -> "calabash_test_package"
        ExternalData -> "external_data"
        InstrumentationTestPackage -> "instrumentation_test_package"
        UiautomatorTestPackage -> "uiautomator_test_package"

instance Hashable     UploadType
instance ToByteString UploadType
instance ToPath       UploadType
instance ToQuery      UploadType
instance ToHeader     UploadType

instance ToJSON UploadType where
    toJSON = toJSONText

instance FromJSON UploadType where
    parseJSON = parseJSONText "UploadType"
