{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Sum where

import Network.AWS.Prelude

data ArtifactCategory
  = ACFile
  | ACLog
  | ACScreenshot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ArtifactCategory where
    parser = takeLowerText >>= \case
        "file" -> pure ACFile
        "log" -> pure ACLog
        "screenshot" -> pure ACScreenshot
        e -> fromTextError $ "Failure parsing ArtifactCategory from value: '" <> e
           <> "'. Accepted values: file, log, screenshot"

instance ToText ArtifactCategory where
    toText = \case
        ACFile -> "FILE"
        ACLog -> "LOG"
        ACScreenshot -> "SCREENSHOT"

instance Hashable     ArtifactCategory
instance NFData       ArtifactCategory
instance ToByteString ArtifactCategory
instance ToQuery      ArtifactCategory
instance ToHeader     ArtifactCategory

instance ToJSON ArtifactCategory where
    toJSON = toJSONText

data ArtifactType
  = AppiumJavaOutput
  | AppiumJavaXMLOutput
  | AppiumPythonOutput
  | AppiumPythonXMLOutput
  | AppiumServerOutput
  | ApplicationCrashReport
  | AutomationOutput
  | CalabashJSONOutput
  | CalabashJavaXMLOutput
  | CalabashPrettyOutput
  | CalabashStandardOutput
  | CustomerArtifact
  | CustomerArtifactLog
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
  | Video
  | VideoLog
  | WebkitLog
  | XctestLog
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ArtifactType where
    parser = takeLowerText >>= \case
        "appium_java_output" -> pure AppiumJavaOutput
        "appium_java_xml_output" -> pure AppiumJavaXMLOutput
        "appium_python_output" -> pure AppiumPythonOutput
        "appium_python_xml_output" -> pure AppiumPythonXMLOutput
        "appium_server_output" -> pure AppiumServerOutput
        "application_crash_report" -> pure ApplicationCrashReport
        "automation_output" -> pure AutomationOutput
        "calabash_json_output" -> pure CalabashJSONOutput
        "calabash_java_xml_output" -> pure CalabashJavaXMLOutput
        "calabash_pretty_output" -> pure CalabashPrettyOutput
        "calabash_standard_output" -> pure CalabashStandardOutput
        "customer_artifact" -> pure CustomerArtifact
        "customer_artifact_log" -> pure CustomerArtifactLog
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
        "video" -> pure Video
        "video_log" -> pure VideoLog
        "webkit_log" -> pure WebkitLog
        "xctest_log" -> pure XctestLog
        e -> fromTextError $ "Failure parsing ArtifactType from value: '" <> e
           <> "'. Accepted values: appium_java_output, appium_java_xml_output, appium_python_output, appium_python_xml_output, appium_server_output, application_crash_report, automation_output, calabash_json_output, calabash_java_xml_output, calabash_pretty_output, calabash_standard_output, customer_artifact, customer_artifact_log, device_log, exerciser_monkey_output, explorer_event_log, explorer_summary_log, instrumentation_output, message_log, result_log, screenshot, service_log, unknown, video, video_log, webkit_log, xctest_log"

instance ToText ArtifactType where
    toText = \case
        AppiumJavaOutput -> "APPIUM_JAVA_OUTPUT"
        AppiumJavaXMLOutput -> "APPIUM_JAVA_XML_OUTPUT"
        AppiumPythonOutput -> "APPIUM_PYTHON_OUTPUT"
        AppiumPythonXMLOutput -> "APPIUM_PYTHON_XML_OUTPUT"
        AppiumServerOutput -> "APPIUM_SERVER_OUTPUT"
        ApplicationCrashReport -> "APPLICATION_CRASH_REPORT"
        AutomationOutput -> "AUTOMATION_OUTPUT"
        CalabashJSONOutput -> "CALABASH_JSON_OUTPUT"
        CalabashJavaXMLOutput -> "CALABASH_JAVA_XML_OUTPUT"
        CalabashPrettyOutput -> "CALABASH_PRETTY_OUTPUT"
        CalabashStandardOutput -> "CALABASH_STANDARD_OUTPUT"
        CustomerArtifact -> "CUSTOMER_ARTIFACT"
        CustomerArtifactLog -> "CUSTOMER_ARTIFACT_LOG"
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
        Video -> "VIDEO"
        VideoLog -> "VIDEO_LOG"
        WebkitLog -> "WEBKIT_LOG"
        XctestLog -> "XCTEST_LOG"

instance Hashable     ArtifactType
instance NFData       ArtifactType
instance ToByteString ArtifactType
instance ToQuery      ArtifactType
instance ToHeader     ArtifactType

instance FromJSON ArtifactType where
    parseJSON = parseJSONText "ArtifactType"

data BillingMethod
  = Metered
  | Unmetered
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BillingMethod where
    parser = takeLowerText >>= \case
        "metered" -> pure Metered
        "unmetered" -> pure Unmetered
        e -> fromTextError $ "Failure parsing BillingMethod from value: '" <> e
           <> "'. Accepted values: metered, unmetered"

instance ToText BillingMethod where
    toText = \case
        Metered -> "METERED"
        Unmetered -> "UNMETERED"

instance Hashable     BillingMethod
instance NFData       BillingMethod
instance ToByteString BillingMethod
instance ToQuery      BillingMethod
instance ToHeader     BillingMethod

instance ToJSON BillingMethod where
    toJSON = toJSONText

instance FromJSON BillingMethod where
    parseJSON = parseJSONText "BillingMethod"

data CurrencyCode =
  Usd
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CurrencyCode where
    parser = takeLowerText >>= \case
        "usd" -> pure Usd
        e -> fromTextError $ "Failure parsing CurrencyCode from value: '" <> e
           <> "'. Accepted values: usd"

instance ToText CurrencyCode where
    toText = \case
        Usd -> "USD"

instance Hashable     CurrencyCode
instance NFData       CurrencyCode
instance ToByteString CurrencyCode
instance ToQuery      CurrencyCode
instance ToHeader     CurrencyCode

instance FromJSON CurrencyCode where
    parseJSON = parseJSONText "CurrencyCode"

data DeviceAttribute
  = ARN
  | AppiumVersion
  | FleetType
  | FormFactor
  | InstanceARN
  | InstanceLabels
  | Manufacturer
  | Platform
  | RemoteAccessEnabled
  | RemoteDebugEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeviceAttribute where
    parser = takeLowerText >>= \case
        "arn" -> pure ARN
        "appium_version" -> pure AppiumVersion
        "fleet_type" -> pure FleetType
        "form_factor" -> pure FormFactor
        "instance_arn" -> pure InstanceARN
        "instance_labels" -> pure InstanceLabels
        "manufacturer" -> pure Manufacturer
        "platform" -> pure Platform
        "remote_access_enabled" -> pure RemoteAccessEnabled
        "remote_debug_enabled" -> pure RemoteDebugEnabled
        e -> fromTextError $ "Failure parsing DeviceAttribute from value: '" <> e
           <> "'. Accepted values: arn, appium_version, fleet_type, form_factor, instance_arn, instance_labels, manufacturer, platform, remote_access_enabled, remote_debug_enabled"

instance ToText DeviceAttribute where
    toText = \case
        ARN -> "ARN"
        AppiumVersion -> "APPIUM_VERSION"
        FleetType -> "FLEET_TYPE"
        FormFactor -> "FORM_FACTOR"
        InstanceARN -> "INSTANCE_ARN"
        InstanceLabels -> "INSTANCE_LABELS"
        Manufacturer -> "MANUFACTURER"
        Platform -> "PLATFORM"
        RemoteAccessEnabled -> "REMOTE_ACCESS_ENABLED"
        RemoteDebugEnabled -> "REMOTE_DEBUG_ENABLED"

instance Hashable     DeviceAttribute
instance NFData       DeviceAttribute
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeviceFormFactor where
    parser = takeLowerText >>= \case
        "phone" -> pure Phone
        "tablet" -> pure Tablet
        e -> fromTextError $ "Failure parsing DeviceFormFactor from value: '" <> e
           <> "'. Accepted values: phone, tablet"

instance ToText DeviceFormFactor where
    toText = \case
        Phone -> "PHONE"
        Tablet -> "TABLET"

instance Hashable     DeviceFormFactor
instance NFData       DeviceFormFactor
instance ToByteString DeviceFormFactor
instance ToQuery      DeviceFormFactor
instance ToHeader     DeviceFormFactor

instance FromJSON DeviceFormFactor where
    parseJSON = parseJSONText "DeviceFormFactor"

data DevicePlatform
  = Android
  | Ios
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DevicePlatform where
    parser = takeLowerText >>= \case
        "android" -> pure Android
        "ios" -> pure Ios
        e -> fromTextError $ "Failure parsing DevicePlatform from value: '" <> e
           <> "'. Accepted values: android, ios"

instance ToText DevicePlatform where
    toText = \case
        Android -> "ANDROID"
        Ios -> "IOS"

instance Hashable     DevicePlatform
instance NFData       DevicePlatform
instance ToByteString DevicePlatform
instance ToQuery      DevicePlatform
instance ToHeader     DevicePlatform

instance FromJSON DevicePlatform where
    parseJSON = parseJSONText "DevicePlatform"

data DevicePoolType
  = DPTCurated
  | DPTPrivate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DevicePoolType where
    parser = takeLowerText >>= \case
        "curated" -> pure DPTCurated
        "private" -> pure DPTPrivate
        e -> fromTextError $ "Failure parsing DevicePoolType from value: '" <> e
           <> "'. Accepted values: curated, private"

instance ToText DevicePoolType where
    toText = \case
        DPTCurated -> "CURATED"
        DPTPrivate -> "PRIVATE"

instance Hashable     DevicePoolType
instance NFData       DevicePoolType
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
        ERErrored -> "ERRORED"
        ERFailed -> "FAILED"
        ERPassed -> "PASSED"
        ERPending -> "PENDING"
        ERSkipped -> "SKIPPED"
        ERStopped -> "STOPPED"
        ERWarned -> "WARNED"

instance Hashable     ExecutionResult
instance NFData       ExecutionResult
instance ToByteString ExecutionResult
instance ToQuery      ExecutionResult
instance ToHeader     ExecutionResult

instance FromJSON ExecutionResult where
    parseJSON = parseJSONText "ExecutionResult"

data ExecutionResultCode
  = ParsingFailed
  | VPCEndpointSetupFailed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExecutionResultCode where
    parser = takeLowerText >>= \case
        "parsing_failed" -> pure ParsingFailed
        "vpc_endpoint_setup_failed" -> pure VPCEndpointSetupFailed
        e -> fromTextError $ "Failure parsing ExecutionResultCode from value: '" <> e
           <> "'. Accepted values: parsing_failed, vpc_endpoint_setup_failed"

instance ToText ExecutionResultCode where
    toText = \case
        ParsingFailed -> "PARSING_FAILED"
        VPCEndpointSetupFailed -> "VPC_ENDPOINT_SETUP_FAILED"

instance Hashable     ExecutionResultCode
instance NFData       ExecutionResultCode
instance ToByteString ExecutionResultCode
instance ToQuery      ExecutionResultCode
instance ToHeader     ExecutionResultCode

instance FromJSON ExecutionResultCode where
    parseJSON = parseJSONText "ExecutionResultCode"

data ExecutionStatus
  = Completed
  | Pending
  | PendingConcurrency
  | PendingDevice
  | Preparing
  | Processing
  | Running
  | Scheduling
  | Stopping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExecutionStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "pending" -> pure Pending
        "pending_concurrency" -> pure PendingConcurrency
        "pending_device" -> pure PendingDevice
        "preparing" -> pure Preparing
        "processing" -> pure Processing
        "running" -> pure Running
        "scheduling" -> pure Scheduling
        "stopping" -> pure Stopping
        e -> fromTextError $ "Failure parsing ExecutionStatus from value: '" <> e
           <> "'. Accepted values: completed, pending, pending_concurrency, pending_device, preparing, processing, running, scheduling, stopping"

instance ToText ExecutionStatus where
    toText = \case
        Completed -> "COMPLETED"
        Pending -> "PENDING"
        PendingConcurrency -> "PENDING_CONCURRENCY"
        PendingDevice -> "PENDING_DEVICE"
        Preparing -> "PREPARING"
        Processing -> "PROCESSING"
        Running -> "RUNNING"
        Scheduling -> "SCHEDULING"
        Stopping -> "STOPPING"

instance Hashable     ExecutionStatus
instance NFData       ExecutionStatus
instance ToByteString ExecutionStatus
instance ToQuery      ExecutionStatus
instance ToHeader     ExecutionStatus

instance FromJSON ExecutionStatus where
    parseJSON = parseJSONText "ExecutionStatus"

data InstanceStatus
  = ISAvailable
  | ISInUse
  | ISNotAvailable
  | ISPreparing
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceStatus where
    parser = takeLowerText >>= \case
        "available" -> pure ISAvailable
        "in_use" -> pure ISInUse
        "not_available" -> pure ISNotAvailable
        "preparing" -> pure ISPreparing
        e -> fromTextError $ "Failure parsing InstanceStatus from value: '" <> e
           <> "'. Accepted values: available, in_use, not_available, preparing"

instance ToText InstanceStatus where
    toText = \case
        ISAvailable -> "AVAILABLE"
        ISInUse -> "IN_USE"
        ISNotAvailable -> "NOT_AVAILABLE"
        ISPreparing -> "PREPARING"

instance Hashable     InstanceStatus
instance NFData       InstanceStatus
instance ToByteString InstanceStatus
instance ToQuery      InstanceStatus
instance ToHeader     InstanceStatus

instance FromJSON InstanceStatus where
    parseJSON = parseJSONText "InstanceStatus"

data InteractionMode
  = Interactive
  | NoVideo
  | VideoOnly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InteractionMode where
    parser = takeLowerText >>= \case
        "interactive" -> pure Interactive
        "no_video" -> pure NoVideo
        "video_only" -> pure VideoOnly
        e -> fromTextError $ "Failure parsing InteractionMode from value: '" <> e
           <> "'. Accepted values: interactive, no_video, video_only"

instance ToText InteractionMode where
    toText = \case
        Interactive -> "INTERACTIVE"
        NoVideo -> "NO_VIDEO"
        VideoOnly -> "VIDEO_ONLY"

instance Hashable     InteractionMode
instance NFData       InteractionMode
instance ToByteString InteractionMode
instance ToQuery      InteractionMode
instance ToHeader     InteractionMode

instance ToJSON InteractionMode where
    toJSON = toJSONText

instance FromJSON InteractionMode where
    parseJSON = parseJSONText "InteractionMode"

data NetworkProfileType
  = Curated
  | Private
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkProfileType where
    parser = takeLowerText >>= \case
        "curated" -> pure Curated
        "private" -> pure Private
        e -> fromTextError $ "Failure parsing NetworkProfileType from value: '" <> e
           <> "'. Accepted values: curated, private"

instance ToText NetworkProfileType where
    toText = \case
        Curated -> "CURATED"
        Private -> "PRIVATE"

instance Hashable     NetworkProfileType
instance NFData       NetworkProfileType
instance ToByteString NetworkProfileType
instance ToQuery      NetworkProfileType
instance ToHeader     NetworkProfileType

instance ToJSON NetworkProfileType where
    toJSON = toJSONText

instance FromJSON NetworkProfileType where
    parseJSON = parseJSONText "NetworkProfileType"

data OfferingTransactionType
  = Purchase
  | Renew
  | System
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OfferingTransactionType where
    parser = takeLowerText >>= \case
        "purchase" -> pure Purchase
        "renew" -> pure Renew
        "system" -> pure System
        e -> fromTextError $ "Failure parsing OfferingTransactionType from value: '" <> e
           <> "'. Accepted values: purchase, renew, system"

instance ToText OfferingTransactionType where
    toText = \case
        Purchase -> "PURCHASE"
        Renew -> "RENEW"
        System -> "SYSTEM"

instance Hashable     OfferingTransactionType
instance NFData       OfferingTransactionType
instance ToByteString OfferingTransactionType
instance ToQuery      OfferingTransactionType
instance ToHeader     OfferingTransactionType

instance FromJSON OfferingTransactionType where
    parseJSON = parseJSONText "OfferingTransactionType"

data OfferingType =
  Recurring
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OfferingType where
    parser = takeLowerText >>= \case
        "recurring" -> pure Recurring
        e -> fromTextError $ "Failure parsing OfferingType from value: '" <> e
           <> "'. Accepted values: recurring"

instance ToText OfferingType where
    toText = \case
        Recurring -> "RECURRING"

instance Hashable     OfferingType
instance NFData       OfferingType
instance ToByteString OfferingType
instance ToQuery      OfferingType
instance ToHeader     OfferingType

instance FromJSON OfferingType where
    parseJSON = parseJSONText "OfferingType"

data RecurringChargeFrequency =
  Monthly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RecurringChargeFrequency where
    parser = takeLowerText >>= \case
        "monthly" -> pure Monthly
        e -> fromTextError $ "Failure parsing RecurringChargeFrequency from value: '" <> e
           <> "'. Accepted values: monthly"

instance ToText RecurringChargeFrequency where
    toText = \case
        Monthly -> "MONTHLY"

instance Hashable     RecurringChargeFrequency
instance NFData       RecurringChargeFrequency
instance ToByteString RecurringChargeFrequency
instance ToQuery      RecurringChargeFrequency
instance ToHeader     RecurringChargeFrequency

instance FromJSON RecurringChargeFrequency where
    parseJSON = parseJSONText "RecurringChargeFrequency"

data RuleOperator
  = Contains
  | Equals
  | GreaterThan
  | IN
  | LessThan
  | NotIn
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RuleOperator where
    parser = takeLowerText >>= \case
        "contains" -> pure Contains
        "equals" -> pure Equals
        "greater_than" -> pure GreaterThan
        "in" -> pure IN
        "less_than" -> pure LessThan
        "not_in" -> pure NotIn
        e -> fromTextError $ "Failure parsing RuleOperator from value: '" <> e
           <> "'. Accepted values: contains, equals, greater_than, in, less_than, not_in"

instance ToText RuleOperator where
    toText = \case
        Contains -> "CONTAINS"
        Equals -> "EQUALS"
        GreaterThan -> "GREATER_THAN"
        IN -> "IN"
        LessThan -> "LESS_THAN"
        NotIn -> "NOT_IN"

instance Hashable     RuleOperator
instance NFData       RuleOperator
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
instance NFData       SampleType
instance ToByteString SampleType
instance ToQuery      SampleType
instance ToHeader     SampleType

instance FromJSON SampleType where
    parseJSON = parseJSONText "SampleType"

data TestType
  = AppiumJavaJunit
  | AppiumJavaTestng
  | AppiumPython
  | AppiumWebJavaJunit
  | AppiumWebJavaTestng
  | AppiumWebPython
  | BuiltinExplorer
  | BuiltinFuzz
  | Calabash
  | Instrumentation
  | RemoteAccessRecord
  | RemoteAccessReplay
  | Uiautomation
  | Uiautomator
  | WebPerformanceProfile
  | Xctest
  | XctestUi
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TestType where
    parser = takeLowerText >>= \case
        "appium_java_junit" -> pure AppiumJavaJunit
        "appium_java_testng" -> pure AppiumJavaTestng
        "appium_python" -> pure AppiumPython
        "appium_web_java_junit" -> pure AppiumWebJavaJunit
        "appium_web_java_testng" -> pure AppiumWebJavaTestng
        "appium_web_python" -> pure AppiumWebPython
        "builtin_explorer" -> pure BuiltinExplorer
        "builtin_fuzz" -> pure BuiltinFuzz
        "calabash" -> pure Calabash
        "instrumentation" -> pure Instrumentation
        "remote_access_record" -> pure RemoteAccessRecord
        "remote_access_replay" -> pure RemoteAccessReplay
        "uiautomation" -> pure Uiautomation
        "uiautomator" -> pure Uiautomator
        "web_performance_profile" -> pure WebPerformanceProfile
        "xctest" -> pure Xctest
        "xctest_ui" -> pure XctestUi
        e -> fromTextError $ "Failure parsing TestType from value: '" <> e
           <> "'. Accepted values: appium_java_junit, appium_java_testng, appium_python, appium_web_java_junit, appium_web_java_testng, appium_web_python, builtin_explorer, builtin_fuzz, calabash, instrumentation, remote_access_record, remote_access_replay, uiautomation, uiautomator, web_performance_profile, xctest, xctest_ui"

instance ToText TestType where
    toText = \case
        AppiumJavaJunit -> "APPIUM_JAVA_JUNIT"
        AppiumJavaTestng -> "APPIUM_JAVA_TESTNG"
        AppiumPython -> "APPIUM_PYTHON"
        AppiumWebJavaJunit -> "APPIUM_WEB_JAVA_JUNIT"
        AppiumWebJavaTestng -> "APPIUM_WEB_JAVA_TESTNG"
        AppiumWebPython -> "APPIUM_WEB_PYTHON"
        BuiltinExplorer -> "BUILTIN_EXPLORER"
        BuiltinFuzz -> "BUILTIN_FUZZ"
        Calabash -> "CALABASH"
        Instrumentation -> "INSTRUMENTATION"
        RemoteAccessRecord -> "REMOTE_ACCESS_RECORD"
        RemoteAccessReplay -> "REMOTE_ACCESS_REPLAY"
        Uiautomation -> "UIAUTOMATION"
        Uiautomator -> "UIAUTOMATOR"
        WebPerformanceProfile -> "WEB_PERFORMANCE_PROFILE"
        Xctest -> "XCTEST"
        XctestUi -> "XCTEST_UI"

instance Hashable     TestType
instance NFData       TestType
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
        USFailed -> "FAILED"
        USInitialized -> "INITIALIZED"
        USProcessing -> "PROCESSING"
        USSucceeded -> "SUCCEEDED"

instance Hashable     UploadStatus
instance NFData       UploadStatus
instance ToByteString UploadStatus
instance ToQuery      UploadStatus
instance ToHeader     UploadStatus

instance FromJSON UploadStatus where
    parseJSON = parseJSONText "UploadStatus"

data UploadType
  = AndroidApp
  | AppiumJavaJunitTestPackage
  | AppiumJavaTestngTestPackage
  | AppiumPythonTestPackage
  | AppiumWebJavaJunitTestPackage
  | AppiumWebJavaTestngTestPackage
  | AppiumWebPythonTestPackage
  | CalabashTestPackage
  | ExternalData
  | InstrumentationTestPackage
  | IosApp
  | UiautomationTestPackage
  | UiautomatorTestPackage
  | WebApp
  | XctestTestPackage
  | XctestUiTestPackage
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UploadType where
    parser = takeLowerText >>= \case
        "android_app" -> pure AndroidApp
        "appium_java_junit_test_package" -> pure AppiumJavaJunitTestPackage
        "appium_java_testng_test_package" -> pure AppiumJavaTestngTestPackage
        "appium_python_test_package" -> pure AppiumPythonTestPackage
        "appium_web_java_junit_test_package" -> pure AppiumWebJavaJunitTestPackage
        "appium_web_java_testng_test_package" -> pure AppiumWebJavaTestngTestPackage
        "appium_web_python_test_package" -> pure AppiumWebPythonTestPackage
        "calabash_test_package" -> pure CalabashTestPackage
        "external_data" -> pure ExternalData
        "instrumentation_test_package" -> pure InstrumentationTestPackage
        "ios_app" -> pure IosApp
        "uiautomation_test_package" -> pure UiautomationTestPackage
        "uiautomator_test_package" -> pure UiautomatorTestPackage
        "web_app" -> pure WebApp
        "xctest_test_package" -> pure XctestTestPackage
        "xctest_ui_test_package" -> pure XctestUiTestPackage
        e -> fromTextError $ "Failure parsing UploadType from value: '" <> e
           <> "'. Accepted values: android_app, appium_java_junit_test_package, appium_java_testng_test_package, appium_python_test_package, appium_web_java_junit_test_package, appium_web_java_testng_test_package, appium_web_python_test_package, calabash_test_package, external_data, instrumentation_test_package, ios_app, uiautomation_test_package, uiautomator_test_package, web_app, xctest_test_package, xctest_ui_test_package"

instance ToText UploadType where
    toText = \case
        AndroidApp -> "ANDROID_APP"
        AppiumJavaJunitTestPackage -> "APPIUM_JAVA_JUNIT_TEST_PACKAGE"
        AppiumJavaTestngTestPackage -> "APPIUM_JAVA_TESTNG_TEST_PACKAGE"
        AppiumPythonTestPackage -> "APPIUM_PYTHON_TEST_PACKAGE"
        AppiumWebJavaJunitTestPackage -> "APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE"
        AppiumWebJavaTestngTestPackage -> "APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE"
        AppiumWebPythonTestPackage -> "APPIUM_WEB_PYTHON_TEST_PACKAGE"
        CalabashTestPackage -> "CALABASH_TEST_PACKAGE"
        ExternalData -> "EXTERNAL_DATA"
        InstrumentationTestPackage -> "INSTRUMENTATION_TEST_PACKAGE"
        IosApp -> "IOS_APP"
        UiautomationTestPackage -> "UIAUTOMATION_TEST_PACKAGE"
        UiautomatorTestPackage -> "UIAUTOMATOR_TEST_PACKAGE"
        WebApp -> "WEB_APP"
        XctestTestPackage -> "XCTEST_TEST_PACKAGE"
        XctestUiTestPackage -> "XCTEST_UI_TEST_PACKAGE"

instance Hashable     UploadType
instance NFData       UploadType
instance ToByteString UploadType
instance ToQuery      UploadType
instance ToHeader     UploadType

instance ToJSON UploadType where
    toJSON = toJSONText

instance FromJSON UploadType where
    parseJSON = parseJSONText "UploadType"
