{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DeviceFarm.Types.ArtifactType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.ArtifactType
  ( ArtifactType
      ( ..,
        ArtifactType_APPIUM_JAVA_OUTPUT,
        ArtifactType_APPIUM_JAVA_XML_OUTPUT,
        ArtifactType_APPIUM_PYTHON_OUTPUT,
        ArtifactType_APPIUM_PYTHON_XML_OUTPUT,
        ArtifactType_APPIUM_SERVER_OUTPUT,
        ArtifactType_APPLICATION_CRASH_REPORT,
        ArtifactType_AUTOMATION_OUTPUT,
        ArtifactType_CALABASH_JAVA_XML_OUTPUT,
        ArtifactType_CALABASH_JSON_OUTPUT,
        ArtifactType_CALABASH_PRETTY_OUTPUT,
        ArtifactType_CALABASH_STANDARD_OUTPUT,
        ArtifactType_CUSTOMER_ARTIFACT,
        ArtifactType_CUSTOMER_ARTIFACT_LOG,
        ArtifactType_DEVICE_LOG,
        ArtifactType_EXERCISER_MONKEY_OUTPUT,
        ArtifactType_EXPLORER_EVENT_LOG,
        ArtifactType_EXPLORER_SUMMARY_LOG,
        ArtifactType_INSTRUMENTATION_OUTPUT,
        ArtifactType_MESSAGE_LOG,
        ArtifactType_RESULT_LOG,
        ArtifactType_SCREENSHOT,
        ArtifactType_SERVICE_LOG,
        ArtifactType_TESTSPEC_OUTPUT,
        ArtifactType_UNKNOWN,
        ArtifactType_VIDEO,
        ArtifactType_VIDEO_LOG,
        ArtifactType_WEBKIT_LOG,
        ArtifactType_XCTEST_LOG
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ArtifactType = ArtifactType'
  { fromArtifactType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ArtifactType_APPIUM_JAVA_OUTPUT :: ArtifactType
pattern ArtifactType_APPIUM_JAVA_OUTPUT = ArtifactType' "APPIUM_JAVA_OUTPUT"

pattern ArtifactType_APPIUM_JAVA_XML_OUTPUT :: ArtifactType
pattern ArtifactType_APPIUM_JAVA_XML_OUTPUT = ArtifactType' "APPIUM_JAVA_XML_OUTPUT"

pattern ArtifactType_APPIUM_PYTHON_OUTPUT :: ArtifactType
pattern ArtifactType_APPIUM_PYTHON_OUTPUT = ArtifactType' "APPIUM_PYTHON_OUTPUT"

pattern ArtifactType_APPIUM_PYTHON_XML_OUTPUT :: ArtifactType
pattern ArtifactType_APPIUM_PYTHON_XML_OUTPUT = ArtifactType' "APPIUM_PYTHON_XML_OUTPUT"

pattern ArtifactType_APPIUM_SERVER_OUTPUT :: ArtifactType
pattern ArtifactType_APPIUM_SERVER_OUTPUT = ArtifactType' "APPIUM_SERVER_OUTPUT"

pattern ArtifactType_APPLICATION_CRASH_REPORT :: ArtifactType
pattern ArtifactType_APPLICATION_CRASH_REPORT = ArtifactType' "APPLICATION_CRASH_REPORT"

pattern ArtifactType_AUTOMATION_OUTPUT :: ArtifactType
pattern ArtifactType_AUTOMATION_OUTPUT = ArtifactType' "AUTOMATION_OUTPUT"

pattern ArtifactType_CALABASH_JAVA_XML_OUTPUT :: ArtifactType
pattern ArtifactType_CALABASH_JAVA_XML_OUTPUT = ArtifactType' "CALABASH_JAVA_XML_OUTPUT"

pattern ArtifactType_CALABASH_JSON_OUTPUT :: ArtifactType
pattern ArtifactType_CALABASH_JSON_OUTPUT = ArtifactType' "CALABASH_JSON_OUTPUT"

pattern ArtifactType_CALABASH_PRETTY_OUTPUT :: ArtifactType
pattern ArtifactType_CALABASH_PRETTY_OUTPUT = ArtifactType' "CALABASH_PRETTY_OUTPUT"

pattern ArtifactType_CALABASH_STANDARD_OUTPUT :: ArtifactType
pattern ArtifactType_CALABASH_STANDARD_OUTPUT = ArtifactType' "CALABASH_STANDARD_OUTPUT"

pattern ArtifactType_CUSTOMER_ARTIFACT :: ArtifactType
pattern ArtifactType_CUSTOMER_ARTIFACT = ArtifactType' "CUSTOMER_ARTIFACT"

pattern ArtifactType_CUSTOMER_ARTIFACT_LOG :: ArtifactType
pattern ArtifactType_CUSTOMER_ARTIFACT_LOG = ArtifactType' "CUSTOMER_ARTIFACT_LOG"

pattern ArtifactType_DEVICE_LOG :: ArtifactType
pattern ArtifactType_DEVICE_LOG = ArtifactType' "DEVICE_LOG"

pattern ArtifactType_EXERCISER_MONKEY_OUTPUT :: ArtifactType
pattern ArtifactType_EXERCISER_MONKEY_OUTPUT = ArtifactType' "EXERCISER_MONKEY_OUTPUT"

pattern ArtifactType_EXPLORER_EVENT_LOG :: ArtifactType
pattern ArtifactType_EXPLORER_EVENT_LOG = ArtifactType' "EXPLORER_EVENT_LOG"

pattern ArtifactType_EXPLORER_SUMMARY_LOG :: ArtifactType
pattern ArtifactType_EXPLORER_SUMMARY_LOG = ArtifactType' "EXPLORER_SUMMARY_LOG"

pattern ArtifactType_INSTRUMENTATION_OUTPUT :: ArtifactType
pattern ArtifactType_INSTRUMENTATION_OUTPUT = ArtifactType' "INSTRUMENTATION_OUTPUT"

pattern ArtifactType_MESSAGE_LOG :: ArtifactType
pattern ArtifactType_MESSAGE_LOG = ArtifactType' "MESSAGE_LOG"

pattern ArtifactType_RESULT_LOG :: ArtifactType
pattern ArtifactType_RESULT_LOG = ArtifactType' "RESULT_LOG"

pattern ArtifactType_SCREENSHOT :: ArtifactType
pattern ArtifactType_SCREENSHOT = ArtifactType' "SCREENSHOT"

pattern ArtifactType_SERVICE_LOG :: ArtifactType
pattern ArtifactType_SERVICE_LOG = ArtifactType' "SERVICE_LOG"

pattern ArtifactType_TESTSPEC_OUTPUT :: ArtifactType
pattern ArtifactType_TESTSPEC_OUTPUT = ArtifactType' "TESTSPEC_OUTPUT"

pattern ArtifactType_UNKNOWN :: ArtifactType
pattern ArtifactType_UNKNOWN = ArtifactType' "UNKNOWN"

pattern ArtifactType_VIDEO :: ArtifactType
pattern ArtifactType_VIDEO = ArtifactType' "VIDEO"

pattern ArtifactType_VIDEO_LOG :: ArtifactType
pattern ArtifactType_VIDEO_LOG = ArtifactType' "VIDEO_LOG"

pattern ArtifactType_WEBKIT_LOG :: ArtifactType
pattern ArtifactType_WEBKIT_LOG = ArtifactType' "WEBKIT_LOG"

pattern ArtifactType_XCTEST_LOG :: ArtifactType
pattern ArtifactType_XCTEST_LOG = ArtifactType' "XCTEST_LOG"

{-# COMPLETE
  ArtifactType_APPIUM_JAVA_OUTPUT,
  ArtifactType_APPIUM_JAVA_XML_OUTPUT,
  ArtifactType_APPIUM_PYTHON_OUTPUT,
  ArtifactType_APPIUM_PYTHON_XML_OUTPUT,
  ArtifactType_APPIUM_SERVER_OUTPUT,
  ArtifactType_APPLICATION_CRASH_REPORT,
  ArtifactType_AUTOMATION_OUTPUT,
  ArtifactType_CALABASH_JAVA_XML_OUTPUT,
  ArtifactType_CALABASH_JSON_OUTPUT,
  ArtifactType_CALABASH_PRETTY_OUTPUT,
  ArtifactType_CALABASH_STANDARD_OUTPUT,
  ArtifactType_CUSTOMER_ARTIFACT,
  ArtifactType_CUSTOMER_ARTIFACT_LOG,
  ArtifactType_DEVICE_LOG,
  ArtifactType_EXERCISER_MONKEY_OUTPUT,
  ArtifactType_EXPLORER_EVENT_LOG,
  ArtifactType_EXPLORER_SUMMARY_LOG,
  ArtifactType_INSTRUMENTATION_OUTPUT,
  ArtifactType_MESSAGE_LOG,
  ArtifactType_RESULT_LOG,
  ArtifactType_SCREENSHOT,
  ArtifactType_SERVICE_LOG,
  ArtifactType_TESTSPEC_OUTPUT,
  ArtifactType_UNKNOWN,
  ArtifactType_VIDEO,
  ArtifactType_VIDEO_LOG,
  ArtifactType_WEBKIT_LOG,
  ArtifactType_XCTEST_LOG,
  ArtifactType'
  #-}
