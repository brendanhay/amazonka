{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.UploadType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.UploadType where

import Network.AWS.Prelude

data UploadType
  = AndroidApp
  | AppiumJavaJunitTestPackage
  | AppiumJavaJunitTestSpec
  | AppiumJavaTestngTestPackage
  | AppiumJavaTestngTestSpec
  | AppiumNodeTestPackage
  | AppiumNodeTestSpec
  | AppiumPythonTestPackage
  | AppiumPythonTestSpec
  | AppiumRubyTestPackage
  | AppiumRubyTestSpec
  | AppiumWebJavaJunitTestPackage
  | AppiumWebJavaJunitTestSpec
  | AppiumWebJavaTestngTestPackage
  | AppiumWebJavaTestngTestSpec
  | AppiumWebNodeTestPackage
  | AppiumWebNodeTestSpec
  | AppiumWebPythonTestPackage
  | AppiumWebPythonTestSpec
  | AppiumWebRubyTestPackage
  | AppiumWebRubyTestSpec
  | CalabashTestPackage
  | ExternalData
  | InstrumentationTestPackage
  | InstrumentationTestSpec
  | IosApp
  | UiautomationTestPackage
  | UiautomatorTestPackage
  | WebApp
  | XctestTestPackage
  | XctestUiTestPackage
  | XctestUiTestSpec
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

instance FromText UploadType where
  parser =
    takeLowerText >>= \case
      "android_app" -> pure AndroidApp
      "appium_java_junit_test_package" -> pure AppiumJavaJunitTestPackage
      "appium_java_junit_test_spec" -> pure AppiumJavaJunitTestSpec
      "appium_java_testng_test_package" -> pure AppiumJavaTestngTestPackage
      "appium_java_testng_test_spec" -> pure AppiumJavaTestngTestSpec
      "appium_node_test_package" -> pure AppiumNodeTestPackage
      "appium_node_test_spec" -> pure AppiumNodeTestSpec
      "appium_python_test_package" -> pure AppiumPythonTestPackage
      "appium_python_test_spec" -> pure AppiumPythonTestSpec
      "appium_ruby_test_package" -> pure AppiumRubyTestPackage
      "appium_ruby_test_spec" -> pure AppiumRubyTestSpec
      "appium_web_java_junit_test_package" -> pure AppiumWebJavaJunitTestPackage
      "appium_web_java_junit_test_spec" -> pure AppiumWebJavaJunitTestSpec
      "appium_web_java_testng_test_package" -> pure AppiumWebJavaTestngTestPackage
      "appium_web_java_testng_test_spec" -> pure AppiumWebJavaTestngTestSpec
      "appium_web_node_test_package" -> pure AppiumWebNodeTestPackage
      "appium_web_node_test_spec" -> pure AppiumWebNodeTestSpec
      "appium_web_python_test_package" -> pure AppiumWebPythonTestPackage
      "appium_web_python_test_spec" -> pure AppiumWebPythonTestSpec
      "appium_web_ruby_test_package" -> pure AppiumWebRubyTestPackage
      "appium_web_ruby_test_spec" -> pure AppiumWebRubyTestSpec
      "calabash_test_package" -> pure CalabashTestPackage
      "external_data" -> pure ExternalData
      "instrumentation_test_package" -> pure InstrumentationTestPackage
      "instrumentation_test_spec" -> pure InstrumentationTestSpec
      "ios_app" -> pure IosApp
      "uiautomation_test_package" -> pure UiautomationTestPackage
      "uiautomator_test_package" -> pure UiautomatorTestPackage
      "web_app" -> pure WebApp
      "xctest_test_package" -> pure XctestTestPackage
      "xctest_ui_test_package" -> pure XctestUiTestPackage
      "xctest_ui_test_spec" -> pure XctestUiTestSpec
      e ->
        fromTextError $
          "Failure parsing UploadType from value: '" <> e
            <> "'. Accepted values: android_app, appium_java_junit_test_package, appium_java_junit_test_spec, appium_java_testng_test_package, appium_java_testng_test_spec, appium_node_test_package, appium_node_test_spec, appium_python_test_package, appium_python_test_spec, appium_ruby_test_package, appium_ruby_test_spec, appium_web_java_junit_test_package, appium_web_java_junit_test_spec, appium_web_java_testng_test_package, appium_web_java_testng_test_spec, appium_web_node_test_package, appium_web_node_test_spec, appium_web_python_test_package, appium_web_python_test_spec, appium_web_ruby_test_package, appium_web_ruby_test_spec, calabash_test_package, external_data, instrumentation_test_package, instrumentation_test_spec, ios_app, uiautomation_test_package, uiautomator_test_package, web_app, xctest_test_package, xctest_ui_test_package, xctest_ui_test_spec"

instance ToText UploadType where
  toText = \case
    AndroidApp -> "ANDROID_APP"
    AppiumJavaJunitTestPackage -> "APPIUM_JAVA_JUNIT_TEST_PACKAGE"
    AppiumJavaJunitTestSpec -> "APPIUM_JAVA_JUNIT_TEST_SPEC"
    AppiumJavaTestngTestPackage -> "APPIUM_JAVA_TESTNG_TEST_PACKAGE"
    AppiumJavaTestngTestSpec -> "APPIUM_JAVA_TESTNG_TEST_SPEC"
    AppiumNodeTestPackage -> "APPIUM_NODE_TEST_PACKAGE"
    AppiumNodeTestSpec -> "APPIUM_NODE_TEST_SPEC"
    AppiumPythonTestPackage -> "APPIUM_PYTHON_TEST_PACKAGE"
    AppiumPythonTestSpec -> "APPIUM_PYTHON_TEST_SPEC"
    AppiumRubyTestPackage -> "APPIUM_RUBY_TEST_PACKAGE"
    AppiumRubyTestSpec -> "APPIUM_RUBY_TEST_SPEC"
    AppiumWebJavaJunitTestPackage -> "APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE"
    AppiumWebJavaJunitTestSpec -> "APPIUM_WEB_JAVA_JUNIT_TEST_SPEC"
    AppiumWebJavaTestngTestPackage -> "APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE"
    AppiumWebJavaTestngTestSpec -> "APPIUM_WEB_JAVA_TESTNG_TEST_SPEC"
    AppiumWebNodeTestPackage -> "APPIUM_WEB_NODE_TEST_PACKAGE"
    AppiumWebNodeTestSpec -> "APPIUM_WEB_NODE_TEST_SPEC"
    AppiumWebPythonTestPackage -> "APPIUM_WEB_PYTHON_TEST_PACKAGE"
    AppiumWebPythonTestSpec -> "APPIUM_WEB_PYTHON_TEST_SPEC"
    AppiumWebRubyTestPackage -> "APPIUM_WEB_RUBY_TEST_PACKAGE"
    AppiumWebRubyTestSpec -> "APPIUM_WEB_RUBY_TEST_SPEC"
    CalabashTestPackage -> "CALABASH_TEST_PACKAGE"
    ExternalData -> "EXTERNAL_DATA"
    InstrumentationTestPackage -> "INSTRUMENTATION_TEST_PACKAGE"
    InstrumentationTestSpec -> "INSTRUMENTATION_TEST_SPEC"
    IosApp -> "IOS_APP"
    UiautomationTestPackage -> "UIAUTOMATION_TEST_PACKAGE"
    UiautomatorTestPackage -> "UIAUTOMATOR_TEST_PACKAGE"
    WebApp -> "WEB_APP"
    XctestTestPackage -> "XCTEST_TEST_PACKAGE"
    XctestUiTestPackage -> "XCTEST_UI_TEST_PACKAGE"
    XctestUiTestSpec -> "XCTEST_UI_TEST_SPEC"

instance Hashable UploadType

instance NFData UploadType

instance ToByteString UploadType

instance ToQuery UploadType

instance ToHeader UploadType

instance ToJSON UploadType where
  toJSON = toJSONText

instance FromJSON UploadType where
  parseJSON = parseJSONText "UploadType"
