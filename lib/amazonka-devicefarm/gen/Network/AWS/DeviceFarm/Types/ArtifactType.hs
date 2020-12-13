{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ArtifactType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ArtifactType
  ( ArtifactType
      ( ArtifactType',
        Unknown,
        Screenshot,
        DeviceLog,
        MessageLog,
        VideoLog,
        ResultLog,
        ServiceLog,
        WebkitLog,
        InstrumentationOutput,
        ExerciserMonkeyOutput,
        CalabashJSONOutput,
        CalabashPrettyOutput,
        CalabashStandardOutput,
        CalabashJavaXMLOutput,
        AutomationOutput,
        AppiumServerOutput,
        AppiumJavaOutput,
        AppiumJavaXMLOutput,
        AppiumPythonOutput,
        AppiumPythonXMLOutput,
        ExplorerEventLog,
        ExplorerSummaryLog,
        ApplicationCrashReport,
        XctestLog,
        Video,
        CustomerArtifact,
        CustomerArtifactLog,
        TestspecOutput
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ArtifactType = ArtifactType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Unknown :: ArtifactType
pattern Unknown = ArtifactType' "UNKNOWN"

pattern Screenshot :: ArtifactType
pattern Screenshot = ArtifactType' "SCREENSHOT"

pattern DeviceLog :: ArtifactType
pattern DeviceLog = ArtifactType' "DEVICE_LOG"

pattern MessageLog :: ArtifactType
pattern MessageLog = ArtifactType' "MESSAGE_LOG"

pattern VideoLog :: ArtifactType
pattern VideoLog = ArtifactType' "VIDEO_LOG"

pattern ResultLog :: ArtifactType
pattern ResultLog = ArtifactType' "RESULT_LOG"

pattern ServiceLog :: ArtifactType
pattern ServiceLog = ArtifactType' "SERVICE_LOG"

pattern WebkitLog :: ArtifactType
pattern WebkitLog = ArtifactType' "WEBKIT_LOG"

pattern InstrumentationOutput :: ArtifactType
pattern InstrumentationOutput = ArtifactType' "INSTRUMENTATION_OUTPUT"

pattern ExerciserMonkeyOutput :: ArtifactType
pattern ExerciserMonkeyOutput = ArtifactType' "EXERCISER_MONKEY_OUTPUT"

pattern CalabashJSONOutput :: ArtifactType
pattern CalabashJSONOutput = ArtifactType' "CALABASH_JSON_OUTPUT"

pattern CalabashPrettyOutput :: ArtifactType
pattern CalabashPrettyOutput = ArtifactType' "CALABASH_PRETTY_OUTPUT"

pattern CalabashStandardOutput :: ArtifactType
pattern CalabashStandardOutput = ArtifactType' "CALABASH_STANDARD_OUTPUT"

pattern CalabashJavaXMLOutput :: ArtifactType
pattern CalabashJavaXMLOutput = ArtifactType' "CALABASH_JAVA_XML_OUTPUT"

pattern AutomationOutput :: ArtifactType
pattern AutomationOutput = ArtifactType' "AUTOMATION_OUTPUT"

pattern AppiumServerOutput :: ArtifactType
pattern AppiumServerOutput = ArtifactType' "APPIUM_SERVER_OUTPUT"

pattern AppiumJavaOutput :: ArtifactType
pattern AppiumJavaOutput = ArtifactType' "APPIUM_JAVA_OUTPUT"

pattern AppiumJavaXMLOutput :: ArtifactType
pattern AppiumJavaXMLOutput = ArtifactType' "APPIUM_JAVA_XML_OUTPUT"

pattern AppiumPythonOutput :: ArtifactType
pattern AppiumPythonOutput = ArtifactType' "APPIUM_PYTHON_OUTPUT"

pattern AppiumPythonXMLOutput :: ArtifactType
pattern AppiumPythonXMLOutput = ArtifactType' "APPIUM_PYTHON_XML_OUTPUT"

pattern ExplorerEventLog :: ArtifactType
pattern ExplorerEventLog = ArtifactType' "EXPLORER_EVENT_LOG"

pattern ExplorerSummaryLog :: ArtifactType
pattern ExplorerSummaryLog = ArtifactType' "EXPLORER_SUMMARY_LOG"

pattern ApplicationCrashReport :: ArtifactType
pattern ApplicationCrashReport = ArtifactType' "APPLICATION_CRASH_REPORT"

pattern XctestLog :: ArtifactType
pattern XctestLog = ArtifactType' "XCTEST_LOG"

pattern Video :: ArtifactType
pattern Video = ArtifactType' "VIDEO"

pattern CustomerArtifact :: ArtifactType
pattern CustomerArtifact = ArtifactType' "CUSTOMER_ARTIFACT"

pattern CustomerArtifactLog :: ArtifactType
pattern CustomerArtifactLog = ArtifactType' "CUSTOMER_ARTIFACT_LOG"

pattern TestspecOutput :: ArtifactType
pattern TestspecOutput = ArtifactType' "TESTSPEC_OUTPUT"

{-# COMPLETE
  Unknown,
  Screenshot,
  DeviceLog,
  MessageLog,
  VideoLog,
  ResultLog,
  ServiceLog,
  WebkitLog,
  InstrumentationOutput,
  ExerciserMonkeyOutput,
  CalabashJSONOutput,
  CalabashPrettyOutput,
  CalabashStandardOutput,
  CalabashJavaXMLOutput,
  AutomationOutput,
  AppiumServerOutput,
  AppiumJavaOutput,
  AppiumJavaXMLOutput,
  AppiumPythonOutput,
  AppiumPythonXMLOutput,
  ExplorerEventLog,
  ExplorerSummaryLog,
  ApplicationCrashReport,
  XctestLog,
  Video,
  CustomerArtifact,
  CustomerArtifactLog,
  TestspecOutput,
  ArtifactType'
  #-}
