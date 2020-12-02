{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ArtifactType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ArtifactType where

import Network.AWS.Prelude

data ArtifactType
  = ATAppiumJavaOutput
  | ATAppiumJavaXMLOutput
  | ATAppiumPythonOutput
  | ATAppiumPythonXMLOutput
  | ATAppiumServerOutput
  | ATApplicationCrashReport
  | ATAutomationOutput
  | ATCalabashJSONOutput
  | ATCalabashJavaXMLOutput
  | ATCalabashPrettyOutput
  | ATCalabashStandardOutput
  | ATCustomerArtifact
  | ATCustomerArtifactLog
  | ATDeviceLog
  | ATExerciserMonkeyOutput
  | ATExplorerEventLog
  | ATExplorerSummaryLog
  | ATInstrumentationOutput
  | ATMessageLog
  | ATResultLog
  | ATScreenshot
  | ATServiceLog
  | ATTestspecOutput
  | ATUnknown
  | ATVideo
  | ATVideoLog
  | ATWebkitLog
  | ATXctestLog
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ArtifactType where
  parser =
    takeLowerText >>= \case
      "appium_java_output" -> pure ATAppiumJavaOutput
      "appium_java_xml_output" -> pure ATAppiumJavaXMLOutput
      "appium_python_output" -> pure ATAppiumPythonOutput
      "appium_python_xml_output" -> pure ATAppiumPythonXMLOutput
      "appium_server_output" -> pure ATAppiumServerOutput
      "application_crash_report" -> pure ATApplicationCrashReport
      "automation_output" -> pure ATAutomationOutput
      "calabash_json_output" -> pure ATCalabashJSONOutput
      "calabash_java_xml_output" -> pure ATCalabashJavaXMLOutput
      "calabash_pretty_output" -> pure ATCalabashPrettyOutput
      "calabash_standard_output" -> pure ATCalabashStandardOutput
      "customer_artifact" -> pure ATCustomerArtifact
      "customer_artifact_log" -> pure ATCustomerArtifactLog
      "device_log" -> pure ATDeviceLog
      "exerciser_monkey_output" -> pure ATExerciserMonkeyOutput
      "explorer_event_log" -> pure ATExplorerEventLog
      "explorer_summary_log" -> pure ATExplorerSummaryLog
      "instrumentation_output" -> pure ATInstrumentationOutput
      "message_log" -> pure ATMessageLog
      "result_log" -> pure ATResultLog
      "screenshot" -> pure ATScreenshot
      "service_log" -> pure ATServiceLog
      "testspec_output" -> pure ATTestspecOutput
      "unknown" -> pure ATUnknown
      "video" -> pure ATVideo
      "video_log" -> pure ATVideoLog
      "webkit_log" -> pure ATWebkitLog
      "xctest_log" -> pure ATXctestLog
      e ->
        fromTextError $
          "Failure parsing ArtifactType from value: '" <> e
            <> "'. Accepted values: appium_java_output, appium_java_xml_output, appium_python_output, appium_python_xml_output, appium_server_output, application_crash_report, automation_output, calabash_json_output, calabash_java_xml_output, calabash_pretty_output, calabash_standard_output, customer_artifact, customer_artifact_log, device_log, exerciser_monkey_output, explorer_event_log, explorer_summary_log, instrumentation_output, message_log, result_log, screenshot, service_log, testspec_output, unknown, video, video_log, webkit_log, xctest_log"

instance ToText ArtifactType where
  toText = \case
    ATAppiumJavaOutput -> "APPIUM_JAVA_OUTPUT"
    ATAppiumJavaXMLOutput -> "APPIUM_JAVA_XML_OUTPUT"
    ATAppiumPythonOutput -> "APPIUM_PYTHON_OUTPUT"
    ATAppiumPythonXMLOutput -> "APPIUM_PYTHON_XML_OUTPUT"
    ATAppiumServerOutput -> "APPIUM_SERVER_OUTPUT"
    ATApplicationCrashReport -> "APPLICATION_CRASH_REPORT"
    ATAutomationOutput -> "AUTOMATION_OUTPUT"
    ATCalabashJSONOutput -> "CALABASH_JSON_OUTPUT"
    ATCalabashJavaXMLOutput -> "CALABASH_JAVA_XML_OUTPUT"
    ATCalabashPrettyOutput -> "CALABASH_PRETTY_OUTPUT"
    ATCalabashStandardOutput -> "CALABASH_STANDARD_OUTPUT"
    ATCustomerArtifact -> "CUSTOMER_ARTIFACT"
    ATCustomerArtifactLog -> "CUSTOMER_ARTIFACT_LOG"
    ATDeviceLog -> "DEVICE_LOG"
    ATExerciserMonkeyOutput -> "EXERCISER_MONKEY_OUTPUT"
    ATExplorerEventLog -> "EXPLORER_EVENT_LOG"
    ATExplorerSummaryLog -> "EXPLORER_SUMMARY_LOG"
    ATInstrumentationOutput -> "INSTRUMENTATION_OUTPUT"
    ATMessageLog -> "MESSAGE_LOG"
    ATResultLog -> "RESULT_LOG"
    ATScreenshot -> "SCREENSHOT"
    ATServiceLog -> "SERVICE_LOG"
    ATTestspecOutput -> "TESTSPEC_OUTPUT"
    ATUnknown -> "UNKNOWN"
    ATVideo -> "VIDEO"
    ATVideoLog -> "VIDEO_LOG"
    ATWebkitLog -> "WEBKIT_LOG"
    ATXctestLog -> "XCTEST_LOG"

instance Hashable ArtifactType

instance NFData ArtifactType

instance ToByteString ArtifactType

instance ToQuery ArtifactType

instance ToHeader ArtifactType

instance FromJSON ArtifactType where
  parseJSON = parseJSONText "ArtifactType"
