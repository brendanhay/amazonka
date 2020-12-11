-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.UploadType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.UploadType
  ( UploadType
      ( UploadType',
        AndroidApp,
        AppiumJavaJunitTestPackage,
        AppiumJavaJunitTestSpec,
        AppiumJavaTestngTestPackage,
        AppiumJavaTestngTestSpec,
        AppiumNodeTestPackage,
        AppiumNodeTestSpec,
        AppiumPythonTestPackage,
        AppiumPythonTestSpec,
        AppiumRubyTestPackage,
        AppiumRubyTestSpec,
        AppiumWebJavaJunitTestPackage,
        AppiumWebJavaJunitTestSpec,
        AppiumWebJavaTestngTestPackage,
        AppiumWebJavaTestngTestSpec,
        AppiumWebNodeTestPackage,
        AppiumWebNodeTestSpec,
        AppiumWebPythonTestPackage,
        AppiumWebPythonTestSpec,
        AppiumWebRubyTestPackage,
        AppiumWebRubyTestSpec,
        CalabashTestPackage,
        ExternalData,
        InstrumentationTestPackage,
        InstrumentationTestSpec,
        IosApp,
        UiautomationTestPackage,
        UiautomatorTestPackage,
        WebApp,
        XctestTestPackage,
        XctestUiTestPackage,
        XctestUiTestSpec
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UploadType = UploadType' Lude.Text
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

pattern AndroidApp :: UploadType
pattern AndroidApp = UploadType' "ANDROID_APP"

pattern AppiumJavaJunitTestPackage :: UploadType
pattern AppiumJavaJunitTestPackage = UploadType' "APPIUM_JAVA_JUNIT_TEST_PACKAGE"

pattern AppiumJavaJunitTestSpec :: UploadType
pattern AppiumJavaJunitTestSpec = UploadType' "APPIUM_JAVA_JUNIT_TEST_SPEC"

pattern AppiumJavaTestngTestPackage :: UploadType
pattern AppiumJavaTestngTestPackage = UploadType' "APPIUM_JAVA_TESTNG_TEST_PACKAGE"

pattern AppiumJavaTestngTestSpec :: UploadType
pattern AppiumJavaTestngTestSpec = UploadType' "APPIUM_JAVA_TESTNG_TEST_SPEC"

pattern AppiumNodeTestPackage :: UploadType
pattern AppiumNodeTestPackage = UploadType' "APPIUM_NODE_TEST_PACKAGE"

pattern AppiumNodeTestSpec :: UploadType
pattern AppiumNodeTestSpec = UploadType' "APPIUM_NODE_TEST_SPEC"

pattern AppiumPythonTestPackage :: UploadType
pattern AppiumPythonTestPackage = UploadType' "APPIUM_PYTHON_TEST_PACKAGE"

pattern AppiumPythonTestSpec :: UploadType
pattern AppiumPythonTestSpec = UploadType' "APPIUM_PYTHON_TEST_SPEC"

pattern AppiumRubyTestPackage :: UploadType
pattern AppiumRubyTestPackage = UploadType' "APPIUM_RUBY_TEST_PACKAGE"

pattern AppiumRubyTestSpec :: UploadType
pattern AppiumRubyTestSpec = UploadType' "APPIUM_RUBY_TEST_SPEC"

pattern AppiumWebJavaJunitTestPackage :: UploadType
pattern AppiumWebJavaJunitTestPackage = UploadType' "APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE"

pattern AppiumWebJavaJunitTestSpec :: UploadType
pattern AppiumWebJavaJunitTestSpec = UploadType' "APPIUM_WEB_JAVA_JUNIT_TEST_SPEC"

pattern AppiumWebJavaTestngTestPackage :: UploadType
pattern AppiumWebJavaTestngTestPackage = UploadType' "APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE"

pattern AppiumWebJavaTestngTestSpec :: UploadType
pattern AppiumWebJavaTestngTestSpec = UploadType' "APPIUM_WEB_JAVA_TESTNG_TEST_SPEC"

pattern AppiumWebNodeTestPackage :: UploadType
pattern AppiumWebNodeTestPackage = UploadType' "APPIUM_WEB_NODE_TEST_PACKAGE"

pattern AppiumWebNodeTestSpec :: UploadType
pattern AppiumWebNodeTestSpec = UploadType' "APPIUM_WEB_NODE_TEST_SPEC"

pattern AppiumWebPythonTestPackage :: UploadType
pattern AppiumWebPythonTestPackage = UploadType' "APPIUM_WEB_PYTHON_TEST_PACKAGE"

pattern AppiumWebPythonTestSpec :: UploadType
pattern AppiumWebPythonTestSpec = UploadType' "APPIUM_WEB_PYTHON_TEST_SPEC"

pattern AppiumWebRubyTestPackage :: UploadType
pattern AppiumWebRubyTestPackage = UploadType' "APPIUM_WEB_RUBY_TEST_PACKAGE"

pattern AppiumWebRubyTestSpec :: UploadType
pattern AppiumWebRubyTestSpec = UploadType' "APPIUM_WEB_RUBY_TEST_SPEC"

pattern CalabashTestPackage :: UploadType
pattern CalabashTestPackage = UploadType' "CALABASH_TEST_PACKAGE"

pattern ExternalData :: UploadType
pattern ExternalData = UploadType' "EXTERNAL_DATA"

pattern InstrumentationTestPackage :: UploadType
pattern InstrumentationTestPackage = UploadType' "INSTRUMENTATION_TEST_PACKAGE"

pattern InstrumentationTestSpec :: UploadType
pattern InstrumentationTestSpec = UploadType' "INSTRUMENTATION_TEST_SPEC"

pattern IosApp :: UploadType
pattern IosApp = UploadType' "IOS_APP"

pattern UiautomationTestPackage :: UploadType
pattern UiautomationTestPackage = UploadType' "UIAUTOMATION_TEST_PACKAGE"

pattern UiautomatorTestPackage :: UploadType
pattern UiautomatorTestPackage = UploadType' "UIAUTOMATOR_TEST_PACKAGE"

pattern WebApp :: UploadType
pattern WebApp = UploadType' "WEB_APP"

pattern XctestTestPackage :: UploadType
pattern XctestTestPackage = UploadType' "XCTEST_TEST_PACKAGE"

pattern XctestUiTestPackage :: UploadType
pattern XctestUiTestPackage = UploadType' "XCTEST_UI_TEST_PACKAGE"

pattern XctestUiTestSpec :: UploadType
pattern XctestUiTestSpec = UploadType' "XCTEST_UI_TEST_SPEC"

{-# COMPLETE
  AndroidApp,
  AppiumJavaJunitTestPackage,
  AppiumJavaJunitTestSpec,
  AppiumJavaTestngTestPackage,
  AppiumJavaTestngTestSpec,
  AppiumNodeTestPackage,
  AppiumNodeTestSpec,
  AppiumPythonTestPackage,
  AppiumPythonTestSpec,
  AppiumRubyTestPackage,
  AppiumRubyTestSpec,
  AppiumWebJavaJunitTestPackage,
  AppiumWebJavaJunitTestSpec,
  AppiumWebJavaTestngTestPackage,
  AppiumWebJavaTestngTestSpec,
  AppiumWebNodeTestPackage,
  AppiumWebNodeTestSpec,
  AppiumWebPythonTestPackage,
  AppiumWebPythonTestSpec,
  AppiumWebRubyTestPackage,
  AppiumWebRubyTestSpec,
  CalabashTestPackage,
  ExternalData,
  InstrumentationTestPackage,
  InstrumentationTestSpec,
  IosApp,
  UiautomationTestPackage,
  UiautomatorTestPackage,
  WebApp,
  XctestTestPackage,
  XctestUiTestPackage,
  XctestUiTestSpec,
  UploadType'
  #-}
