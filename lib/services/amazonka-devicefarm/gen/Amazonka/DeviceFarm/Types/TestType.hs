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
-- Module      : Amazonka.DeviceFarm.Types.TestType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.TestType
  ( TestType
      ( ..,
        TestType_APPIUM_JAVA_JUNIT,
        TestType_APPIUM_JAVA_TESTNG,
        TestType_APPIUM_NODE,
        TestType_APPIUM_PYTHON,
        TestType_APPIUM_RUBY,
        TestType_APPIUM_WEB_JAVA_JUNIT,
        TestType_APPIUM_WEB_JAVA_TESTNG,
        TestType_APPIUM_WEB_NODE,
        TestType_APPIUM_WEB_PYTHON,
        TestType_APPIUM_WEB_RUBY,
        TestType_BUILTIN_EXPLORER,
        TestType_BUILTIN_FUZZ,
        TestType_CALABASH,
        TestType_INSTRUMENTATION,
        TestType_REMOTE_ACCESS_RECORD,
        TestType_REMOTE_ACCESS_REPLAY,
        TestType_UIAUTOMATION,
        TestType_UIAUTOMATOR,
        TestType_WEB_PERFORMANCE_PROFILE,
        TestType_XCTEST,
        TestType_XCTEST_UI
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TestType = TestType'
  { fromTestType ::
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

pattern TestType_APPIUM_JAVA_JUNIT :: TestType
pattern TestType_APPIUM_JAVA_JUNIT = TestType' "APPIUM_JAVA_JUNIT"

pattern TestType_APPIUM_JAVA_TESTNG :: TestType
pattern TestType_APPIUM_JAVA_TESTNG = TestType' "APPIUM_JAVA_TESTNG"

pattern TestType_APPIUM_NODE :: TestType
pattern TestType_APPIUM_NODE = TestType' "APPIUM_NODE"

pattern TestType_APPIUM_PYTHON :: TestType
pattern TestType_APPIUM_PYTHON = TestType' "APPIUM_PYTHON"

pattern TestType_APPIUM_RUBY :: TestType
pattern TestType_APPIUM_RUBY = TestType' "APPIUM_RUBY"

pattern TestType_APPIUM_WEB_JAVA_JUNIT :: TestType
pattern TestType_APPIUM_WEB_JAVA_JUNIT = TestType' "APPIUM_WEB_JAVA_JUNIT"

pattern TestType_APPIUM_WEB_JAVA_TESTNG :: TestType
pattern TestType_APPIUM_WEB_JAVA_TESTNG = TestType' "APPIUM_WEB_JAVA_TESTNG"

pattern TestType_APPIUM_WEB_NODE :: TestType
pattern TestType_APPIUM_WEB_NODE = TestType' "APPIUM_WEB_NODE"

pattern TestType_APPIUM_WEB_PYTHON :: TestType
pattern TestType_APPIUM_WEB_PYTHON = TestType' "APPIUM_WEB_PYTHON"

pattern TestType_APPIUM_WEB_RUBY :: TestType
pattern TestType_APPIUM_WEB_RUBY = TestType' "APPIUM_WEB_RUBY"

pattern TestType_BUILTIN_EXPLORER :: TestType
pattern TestType_BUILTIN_EXPLORER = TestType' "BUILTIN_EXPLORER"

pattern TestType_BUILTIN_FUZZ :: TestType
pattern TestType_BUILTIN_FUZZ = TestType' "BUILTIN_FUZZ"

pattern TestType_CALABASH :: TestType
pattern TestType_CALABASH = TestType' "CALABASH"

pattern TestType_INSTRUMENTATION :: TestType
pattern TestType_INSTRUMENTATION = TestType' "INSTRUMENTATION"

pattern TestType_REMOTE_ACCESS_RECORD :: TestType
pattern TestType_REMOTE_ACCESS_RECORD = TestType' "REMOTE_ACCESS_RECORD"

pattern TestType_REMOTE_ACCESS_REPLAY :: TestType
pattern TestType_REMOTE_ACCESS_REPLAY = TestType' "REMOTE_ACCESS_REPLAY"

pattern TestType_UIAUTOMATION :: TestType
pattern TestType_UIAUTOMATION = TestType' "UIAUTOMATION"

pattern TestType_UIAUTOMATOR :: TestType
pattern TestType_UIAUTOMATOR = TestType' "UIAUTOMATOR"

pattern TestType_WEB_PERFORMANCE_PROFILE :: TestType
pattern TestType_WEB_PERFORMANCE_PROFILE = TestType' "WEB_PERFORMANCE_PROFILE"

pattern TestType_XCTEST :: TestType
pattern TestType_XCTEST = TestType' "XCTEST"

pattern TestType_XCTEST_UI :: TestType
pattern TestType_XCTEST_UI = TestType' "XCTEST_UI"

{-# COMPLETE
  TestType_APPIUM_JAVA_JUNIT,
  TestType_APPIUM_JAVA_TESTNG,
  TestType_APPIUM_NODE,
  TestType_APPIUM_PYTHON,
  TestType_APPIUM_RUBY,
  TestType_APPIUM_WEB_JAVA_JUNIT,
  TestType_APPIUM_WEB_JAVA_TESTNG,
  TestType_APPIUM_WEB_NODE,
  TestType_APPIUM_WEB_PYTHON,
  TestType_APPIUM_WEB_RUBY,
  TestType_BUILTIN_EXPLORER,
  TestType_BUILTIN_FUZZ,
  TestType_CALABASH,
  TestType_INSTRUMENTATION,
  TestType_REMOTE_ACCESS_RECORD,
  TestType_REMOTE_ACCESS_REPLAY,
  TestType_UIAUTOMATION,
  TestType_UIAUTOMATOR,
  TestType_WEB_PERFORMANCE_PROFILE,
  TestType_XCTEST,
  TestType_XCTEST_UI,
  TestType'
  #-}
