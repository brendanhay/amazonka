{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        IosApp,
        WebApp,
        ExternalData,
        AppiumJavaJunitTestPackage,
        AppiumJavaTestngTestPackage,
        AppiumPythonTestPackage,
        AppiumNodeTestPackage,
        AppiumRubyTestPackage,
        AppiumWebJavaJunitTestPackage,
        AppiumWebJavaTestngTestPackage,
        AppiumWebPythonTestPackage,
        AppiumWebNodeTestPackage,
        AppiumWebRubyTestPackage,
        CalabashTestPackage,
        InstrumentationTestPackage,
        UiautomationTestPackage,
        UiautomatorTestPackage,
        XctestTestPackage,
        XctestUiTestPackage,
        AppiumJavaJunitTestSpec,
        AppiumJavaTestngTestSpec,
        AppiumPythonTestSpec,
        AppiumNodeTestSpec,
        AppiumRubyTestSpec,
        AppiumWebJavaJunitTestSpec,
        AppiumWebJavaTestngTestSpec,
        AppiumWebPythonTestSpec,
        AppiumWebNodeTestSpec,
        AppiumWebRubyTestSpec,
        InstrumentationTestSpec,
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

pattern IosApp :: UploadType
pattern IosApp = UploadType' "IOS_APP"

pattern WebApp :: UploadType
pattern WebApp = UploadType' "WEB_APP"

pattern ExternalData :: UploadType
pattern ExternalData = UploadType' "EXTERNAL_DATA"

pattern AppiumJavaJunitTestPackage :: UploadType
pattern AppiumJavaJunitTestPackage = UploadType' "APPIUM_JAVA_JUNIT_TEST_PACKAGE"

pattern AppiumJavaTestngTestPackage :: UploadType
pattern AppiumJavaTestngTestPackage = UploadType' "APPIUM_JAVA_TESTNG_TEST_PACKAGE"

pattern AppiumPythonTestPackage :: UploadType
pattern AppiumPythonTestPackage = UploadType' "APPIUM_PYTHON_TEST_PACKAGE"

pattern AppiumNodeTestPackage :: UploadType
pattern AppiumNodeTestPackage = UploadType' "APPIUM_NODE_TEST_PACKAGE"

pattern AppiumRubyTestPackage :: UploadType
pattern AppiumRubyTestPackage = UploadType' "APPIUM_RUBY_TEST_PACKAGE"

pattern AppiumWebJavaJunitTestPackage :: UploadType
pattern AppiumWebJavaJunitTestPackage = UploadType' "APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE"

pattern AppiumWebJavaTestngTestPackage :: UploadType
pattern AppiumWebJavaTestngTestPackage = UploadType' "APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE"

pattern AppiumWebPythonTestPackage :: UploadType
pattern AppiumWebPythonTestPackage = UploadType' "APPIUM_WEB_PYTHON_TEST_PACKAGE"

pattern AppiumWebNodeTestPackage :: UploadType
pattern AppiumWebNodeTestPackage = UploadType' "APPIUM_WEB_NODE_TEST_PACKAGE"

pattern AppiumWebRubyTestPackage :: UploadType
pattern AppiumWebRubyTestPackage = UploadType' "APPIUM_WEB_RUBY_TEST_PACKAGE"

pattern CalabashTestPackage :: UploadType
pattern CalabashTestPackage = UploadType' "CALABASH_TEST_PACKAGE"

pattern InstrumentationTestPackage :: UploadType
pattern InstrumentationTestPackage = UploadType' "INSTRUMENTATION_TEST_PACKAGE"

pattern UiautomationTestPackage :: UploadType
pattern UiautomationTestPackage = UploadType' "UIAUTOMATION_TEST_PACKAGE"

pattern UiautomatorTestPackage :: UploadType
pattern UiautomatorTestPackage = UploadType' "UIAUTOMATOR_TEST_PACKAGE"

pattern XctestTestPackage :: UploadType
pattern XctestTestPackage = UploadType' "XCTEST_TEST_PACKAGE"

pattern XctestUiTestPackage :: UploadType
pattern XctestUiTestPackage = UploadType' "XCTEST_UI_TEST_PACKAGE"

pattern AppiumJavaJunitTestSpec :: UploadType
pattern AppiumJavaJunitTestSpec = UploadType' "APPIUM_JAVA_JUNIT_TEST_SPEC"

pattern AppiumJavaTestngTestSpec :: UploadType
pattern AppiumJavaTestngTestSpec = UploadType' "APPIUM_JAVA_TESTNG_TEST_SPEC"

pattern AppiumPythonTestSpec :: UploadType
pattern AppiumPythonTestSpec = UploadType' "APPIUM_PYTHON_TEST_SPEC"

pattern AppiumNodeTestSpec :: UploadType
pattern AppiumNodeTestSpec = UploadType' "APPIUM_NODE_TEST_SPEC"

pattern AppiumRubyTestSpec :: UploadType
pattern AppiumRubyTestSpec = UploadType' "APPIUM_RUBY_TEST_SPEC"

pattern AppiumWebJavaJunitTestSpec :: UploadType
pattern AppiumWebJavaJunitTestSpec = UploadType' "APPIUM_WEB_JAVA_JUNIT_TEST_SPEC"

pattern AppiumWebJavaTestngTestSpec :: UploadType
pattern AppiumWebJavaTestngTestSpec = UploadType' "APPIUM_WEB_JAVA_TESTNG_TEST_SPEC"

pattern AppiumWebPythonTestSpec :: UploadType
pattern AppiumWebPythonTestSpec = UploadType' "APPIUM_WEB_PYTHON_TEST_SPEC"

pattern AppiumWebNodeTestSpec :: UploadType
pattern AppiumWebNodeTestSpec = UploadType' "APPIUM_WEB_NODE_TEST_SPEC"

pattern AppiumWebRubyTestSpec :: UploadType
pattern AppiumWebRubyTestSpec = UploadType' "APPIUM_WEB_RUBY_TEST_SPEC"

pattern InstrumentationTestSpec :: UploadType
pattern InstrumentationTestSpec = UploadType' "INSTRUMENTATION_TEST_SPEC"

pattern XctestUiTestSpec :: UploadType
pattern XctestUiTestSpec = UploadType' "XCTEST_UI_TEST_SPEC"

{-# COMPLETE
  AndroidApp,
  IosApp,
  WebApp,
  ExternalData,
  AppiumJavaJunitTestPackage,
  AppiumJavaTestngTestPackage,
  AppiumPythonTestPackage,
  AppiumNodeTestPackage,
  AppiumRubyTestPackage,
  AppiumWebJavaJunitTestPackage,
  AppiumWebJavaTestngTestPackage,
  AppiumWebPythonTestPackage,
  AppiumWebNodeTestPackage,
  AppiumWebRubyTestPackage,
  CalabashTestPackage,
  InstrumentationTestPackage,
  UiautomationTestPackage,
  UiautomatorTestPackage,
  XctestTestPackage,
  XctestUiTestPackage,
  AppiumJavaJunitTestSpec,
  AppiumJavaTestngTestSpec,
  AppiumPythonTestSpec,
  AppiumNodeTestSpec,
  AppiumRubyTestSpec,
  AppiumWebJavaJunitTestSpec,
  AppiumWebJavaTestngTestSpec,
  AppiumWebPythonTestSpec,
  AppiumWebNodeTestSpec,
  AppiumWebRubyTestSpec,
  InstrumentationTestSpec,
  XctestUiTestSpec,
  UploadType'
  #-}
