{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestType where

import Network.AWS.Prelude

data TestType
  = AppiumJavaJunit
  | AppiumJavaTestng
  | AppiumNode
  | AppiumPython
  | AppiumRuby
  | AppiumWebJavaJunit
  | AppiumWebJavaTestng
  | AppiumWebNode
  | AppiumWebPython
  | AppiumWebRuby
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

instance FromText TestType where
  parser =
    takeLowerText >>= \case
      "appium_java_junit" -> pure AppiumJavaJunit
      "appium_java_testng" -> pure AppiumJavaTestng
      "appium_node" -> pure AppiumNode
      "appium_python" -> pure AppiumPython
      "appium_ruby" -> pure AppiumRuby
      "appium_web_java_junit" -> pure AppiumWebJavaJunit
      "appium_web_java_testng" -> pure AppiumWebJavaTestng
      "appium_web_node" -> pure AppiumWebNode
      "appium_web_python" -> pure AppiumWebPython
      "appium_web_ruby" -> pure AppiumWebRuby
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
      e ->
        fromTextError $
          "Failure parsing TestType from value: '" <> e
            <> "'. Accepted values: appium_java_junit, appium_java_testng, appium_node, appium_python, appium_ruby, appium_web_java_junit, appium_web_java_testng, appium_web_node, appium_web_python, appium_web_ruby, builtin_explorer, builtin_fuzz, calabash, instrumentation, remote_access_record, remote_access_replay, uiautomation, uiautomator, web_performance_profile, xctest, xctest_ui"

instance ToText TestType where
  toText = \case
    AppiumJavaJunit -> "APPIUM_JAVA_JUNIT"
    AppiumJavaTestng -> "APPIUM_JAVA_TESTNG"
    AppiumNode -> "APPIUM_NODE"
    AppiumPython -> "APPIUM_PYTHON"
    AppiumRuby -> "APPIUM_RUBY"
    AppiumWebJavaJunit -> "APPIUM_WEB_JAVA_JUNIT"
    AppiumWebJavaTestng -> "APPIUM_WEB_JAVA_TESTNG"
    AppiumWebNode -> "APPIUM_WEB_NODE"
    AppiumWebPython -> "APPIUM_WEB_PYTHON"
    AppiumWebRuby -> "APPIUM_WEB_RUBY"
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

instance Hashable TestType

instance NFData TestType

instance ToByteString TestType

instance ToQuery TestType

instance ToHeader TestType

instance ToJSON TestType where
  toJSON = toJSONText

instance FromJSON TestType where
  parseJSON = parseJSONText "TestType"
