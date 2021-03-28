{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ArtifactType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.ArtifactType
  ( ArtifactType
    ( ArtifactType'
    , ArtifactTypeUnknown
    , ArtifactTypeScreenshot
    , ArtifactTypeDeviceLog
    , ArtifactTypeMessageLog
    , ArtifactTypeVideoLog
    , ArtifactTypeResultLog
    , ArtifactTypeServiceLog
    , ArtifactTypeWebkitLog
    , ArtifactTypeInstrumentationOutput
    , ArtifactTypeExerciserMonkeyOutput
    , ArtifactTypeCalabashJsonOutput
    , ArtifactTypeCalabashPrettyOutput
    , ArtifactTypeCalabashStandardOutput
    , ArtifactTypeCalabashJavaXmlOutput
    , ArtifactTypeAutomationOutput
    , ArtifactTypeAppiumServerOutput
    , ArtifactTypeAppiumJavaOutput
    , ArtifactTypeAppiumJavaXmlOutput
    , ArtifactTypeAppiumPythonOutput
    , ArtifactTypeAppiumPythonXmlOutput
    , ArtifactTypeExplorerEventLog
    , ArtifactTypeExplorerSummaryLog
    , ArtifactTypeApplicationCrashReport
    , ArtifactTypeXctestLog
    , ArtifactTypeVideo
    , ArtifactTypeCustomerArtifact
    , ArtifactTypeCustomerArtifactLog
    , ArtifactTypeTestspecOutput
    , fromArtifactType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ArtifactType = ArtifactType'{fromArtifactType :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern ArtifactTypeUnknown :: ArtifactType
pattern ArtifactTypeUnknown = ArtifactType' "UNKNOWN"

pattern ArtifactTypeScreenshot :: ArtifactType
pattern ArtifactTypeScreenshot = ArtifactType' "SCREENSHOT"

pattern ArtifactTypeDeviceLog :: ArtifactType
pattern ArtifactTypeDeviceLog = ArtifactType' "DEVICE_LOG"

pattern ArtifactTypeMessageLog :: ArtifactType
pattern ArtifactTypeMessageLog = ArtifactType' "MESSAGE_LOG"

pattern ArtifactTypeVideoLog :: ArtifactType
pattern ArtifactTypeVideoLog = ArtifactType' "VIDEO_LOG"

pattern ArtifactTypeResultLog :: ArtifactType
pattern ArtifactTypeResultLog = ArtifactType' "RESULT_LOG"

pattern ArtifactTypeServiceLog :: ArtifactType
pattern ArtifactTypeServiceLog = ArtifactType' "SERVICE_LOG"

pattern ArtifactTypeWebkitLog :: ArtifactType
pattern ArtifactTypeWebkitLog = ArtifactType' "WEBKIT_LOG"

pattern ArtifactTypeInstrumentationOutput :: ArtifactType
pattern ArtifactTypeInstrumentationOutput = ArtifactType' "INSTRUMENTATION_OUTPUT"

pattern ArtifactTypeExerciserMonkeyOutput :: ArtifactType
pattern ArtifactTypeExerciserMonkeyOutput = ArtifactType' "EXERCISER_MONKEY_OUTPUT"

pattern ArtifactTypeCalabashJsonOutput :: ArtifactType
pattern ArtifactTypeCalabashJsonOutput = ArtifactType' "CALABASH_JSON_OUTPUT"

pattern ArtifactTypeCalabashPrettyOutput :: ArtifactType
pattern ArtifactTypeCalabashPrettyOutput = ArtifactType' "CALABASH_PRETTY_OUTPUT"

pattern ArtifactTypeCalabashStandardOutput :: ArtifactType
pattern ArtifactTypeCalabashStandardOutput = ArtifactType' "CALABASH_STANDARD_OUTPUT"

pattern ArtifactTypeCalabashJavaXmlOutput :: ArtifactType
pattern ArtifactTypeCalabashJavaXmlOutput = ArtifactType' "CALABASH_JAVA_XML_OUTPUT"

pattern ArtifactTypeAutomationOutput :: ArtifactType
pattern ArtifactTypeAutomationOutput = ArtifactType' "AUTOMATION_OUTPUT"

pattern ArtifactTypeAppiumServerOutput :: ArtifactType
pattern ArtifactTypeAppiumServerOutput = ArtifactType' "APPIUM_SERVER_OUTPUT"

pattern ArtifactTypeAppiumJavaOutput :: ArtifactType
pattern ArtifactTypeAppiumJavaOutput = ArtifactType' "APPIUM_JAVA_OUTPUT"

pattern ArtifactTypeAppiumJavaXmlOutput :: ArtifactType
pattern ArtifactTypeAppiumJavaXmlOutput = ArtifactType' "APPIUM_JAVA_XML_OUTPUT"

pattern ArtifactTypeAppiumPythonOutput :: ArtifactType
pattern ArtifactTypeAppiumPythonOutput = ArtifactType' "APPIUM_PYTHON_OUTPUT"

pattern ArtifactTypeAppiumPythonXmlOutput :: ArtifactType
pattern ArtifactTypeAppiumPythonXmlOutput = ArtifactType' "APPIUM_PYTHON_XML_OUTPUT"

pattern ArtifactTypeExplorerEventLog :: ArtifactType
pattern ArtifactTypeExplorerEventLog = ArtifactType' "EXPLORER_EVENT_LOG"

pattern ArtifactTypeExplorerSummaryLog :: ArtifactType
pattern ArtifactTypeExplorerSummaryLog = ArtifactType' "EXPLORER_SUMMARY_LOG"

pattern ArtifactTypeApplicationCrashReport :: ArtifactType
pattern ArtifactTypeApplicationCrashReport = ArtifactType' "APPLICATION_CRASH_REPORT"

pattern ArtifactTypeXctestLog :: ArtifactType
pattern ArtifactTypeXctestLog = ArtifactType' "XCTEST_LOG"

pattern ArtifactTypeVideo :: ArtifactType
pattern ArtifactTypeVideo = ArtifactType' "VIDEO"

pattern ArtifactTypeCustomerArtifact :: ArtifactType
pattern ArtifactTypeCustomerArtifact = ArtifactType' "CUSTOMER_ARTIFACT"

pattern ArtifactTypeCustomerArtifactLog :: ArtifactType
pattern ArtifactTypeCustomerArtifactLog = ArtifactType' "CUSTOMER_ARTIFACT_LOG"

pattern ArtifactTypeTestspecOutput :: ArtifactType
pattern ArtifactTypeTestspecOutput = ArtifactType' "TESTSPEC_OUTPUT"

{-# COMPLETE 
  ArtifactTypeUnknown,

  ArtifactTypeScreenshot,

  ArtifactTypeDeviceLog,

  ArtifactTypeMessageLog,

  ArtifactTypeVideoLog,

  ArtifactTypeResultLog,

  ArtifactTypeServiceLog,

  ArtifactTypeWebkitLog,

  ArtifactTypeInstrumentationOutput,

  ArtifactTypeExerciserMonkeyOutput,

  ArtifactTypeCalabashJsonOutput,

  ArtifactTypeCalabashPrettyOutput,

  ArtifactTypeCalabashStandardOutput,

  ArtifactTypeCalabashJavaXmlOutput,

  ArtifactTypeAutomationOutput,

  ArtifactTypeAppiumServerOutput,

  ArtifactTypeAppiumJavaOutput,

  ArtifactTypeAppiumJavaXmlOutput,

  ArtifactTypeAppiumPythonOutput,

  ArtifactTypeAppiumPythonXmlOutput,

  ArtifactTypeExplorerEventLog,

  ArtifactTypeExplorerSummaryLog,

  ArtifactTypeApplicationCrashReport,

  ArtifactTypeXctestLog,

  ArtifactTypeVideo,

  ArtifactTypeCustomerArtifact,

  ArtifactTypeCustomerArtifactLog,

  ArtifactTypeTestspecOutput,
  ArtifactType'
  #-}
