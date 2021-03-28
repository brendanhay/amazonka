{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.UploadType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.UploadType
  ( UploadType
    ( UploadType'
    , UploadTypeAndroidApp
    , UploadTypeIosApp
    , UploadTypeWebApp
    , UploadTypeExternalData
    , UploadTypeAppiumJavaJunitTestPackage
    , UploadTypeAppiumJavaTestngTestPackage
    , UploadTypeAppiumPythonTestPackage
    , UploadTypeAppiumNodeTestPackage
    , UploadTypeAppiumRubyTestPackage
    , UploadTypeAppiumWebJavaJunitTestPackage
    , UploadTypeAppiumWebJavaTestngTestPackage
    , UploadTypeAppiumWebPythonTestPackage
    , UploadTypeAppiumWebNodeTestPackage
    , UploadTypeAppiumWebRubyTestPackage
    , UploadTypeCalabashTestPackage
    , UploadTypeInstrumentationTestPackage
    , UploadTypeUiautomationTestPackage
    , UploadTypeUiautomatorTestPackage
    , UploadTypeXctestTestPackage
    , UploadTypeXctestUiTestPackage
    , UploadTypeAppiumJavaJunitTestSpec
    , UploadTypeAppiumJavaTestngTestSpec
    , UploadTypeAppiumPythonTestSpec
    , UploadTypeAppiumNodeTestSpec
    , UploadTypeAppiumRubyTestSpec
    , UploadTypeAppiumWebJavaJunitTestSpec
    , UploadTypeAppiumWebJavaTestngTestSpec
    , UploadTypeAppiumWebPythonTestSpec
    , UploadTypeAppiumWebNodeTestSpec
    , UploadTypeAppiumWebRubyTestSpec
    , UploadTypeInstrumentationTestSpec
    , UploadTypeXctestUiTestSpec
    , fromUploadType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype UploadType = UploadType'{fromUploadType :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern UploadTypeAndroidApp :: UploadType
pattern UploadTypeAndroidApp = UploadType' "ANDROID_APP"

pattern UploadTypeIosApp :: UploadType
pattern UploadTypeIosApp = UploadType' "IOS_APP"

pattern UploadTypeWebApp :: UploadType
pattern UploadTypeWebApp = UploadType' "WEB_APP"

pattern UploadTypeExternalData :: UploadType
pattern UploadTypeExternalData = UploadType' "EXTERNAL_DATA"

pattern UploadTypeAppiumJavaJunitTestPackage :: UploadType
pattern UploadTypeAppiumJavaJunitTestPackage = UploadType' "APPIUM_JAVA_JUNIT_TEST_PACKAGE"

pattern UploadTypeAppiumJavaTestngTestPackage :: UploadType
pattern UploadTypeAppiumJavaTestngTestPackage = UploadType' "APPIUM_JAVA_TESTNG_TEST_PACKAGE"

pattern UploadTypeAppiumPythonTestPackage :: UploadType
pattern UploadTypeAppiumPythonTestPackage = UploadType' "APPIUM_PYTHON_TEST_PACKAGE"

pattern UploadTypeAppiumNodeTestPackage :: UploadType
pattern UploadTypeAppiumNodeTestPackage = UploadType' "APPIUM_NODE_TEST_PACKAGE"

pattern UploadTypeAppiumRubyTestPackage :: UploadType
pattern UploadTypeAppiumRubyTestPackage = UploadType' "APPIUM_RUBY_TEST_PACKAGE"

pattern UploadTypeAppiumWebJavaJunitTestPackage :: UploadType
pattern UploadTypeAppiumWebJavaJunitTestPackage = UploadType' "APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE"

pattern UploadTypeAppiumWebJavaTestngTestPackage :: UploadType
pattern UploadTypeAppiumWebJavaTestngTestPackage = UploadType' "APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE"

pattern UploadTypeAppiumWebPythonTestPackage :: UploadType
pattern UploadTypeAppiumWebPythonTestPackage = UploadType' "APPIUM_WEB_PYTHON_TEST_PACKAGE"

pattern UploadTypeAppiumWebNodeTestPackage :: UploadType
pattern UploadTypeAppiumWebNodeTestPackage = UploadType' "APPIUM_WEB_NODE_TEST_PACKAGE"

pattern UploadTypeAppiumWebRubyTestPackage :: UploadType
pattern UploadTypeAppiumWebRubyTestPackage = UploadType' "APPIUM_WEB_RUBY_TEST_PACKAGE"

pattern UploadTypeCalabashTestPackage :: UploadType
pattern UploadTypeCalabashTestPackage = UploadType' "CALABASH_TEST_PACKAGE"

pattern UploadTypeInstrumentationTestPackage :: UploadType
pattern UploadTypeInstrumentationTestPackage = UploadType' "INSTRUMENTATION_TEST_PACKAGE"

pattern UploadTypeUiautomationTestPackage :: UploadType
pattern UploadTypeUiautomationTestPackage = UploadType' "UIAUTOMATION_TEST_PACKAGE"

pattern UploadTypeUiautomatorTestPackage :: UploadType
pattern UploadTypeUiautomatorTestPackage = UploadType' "UIAUTOMATOR_TEST_PACKAGE"

pattern UploadTypeXctestTestPackage :: UploadType
pattern UploadTypeXctestTestPackage = UploadType' "XCTEST_TEST_PACKAGE"

pattern UploadTypeXctestUiTestPackage :: UploadType
pattern UploadTypeXctestUiTestPackage = UploadType' "XCTEST_UI_TEST_PACKAGE"

pattern UploadTypeAppiumJavaJunitTestSpec :: UploadType
pattern UploadTypeAppiumJavaJunitTestSpec = UploadType' "APPIUM_JAVA_JUNIT_TEST_SPEC"

pattern UploadTypeAppiumJavaTestngTestSpec :: UploadType
pattern UploadTypeAppiumJavaTestngTestSpec = UploadType' "APPIUM_JAVA_TESTNG_TEST_SPEC"

pattern UploadTypeAppiumPythonTestSpec :: UploadType
pattern UploadTypeAppiumPythonTestSpec = UploadType' "APPIUM_PYTHON_TEST_SPEC"

pattern UploadTypeAppiumNodeTestSpec :: UploadType
pattern UploadTypeAppiumNodeTestSpec = UploadType' "APPIUM_NODE_TEST_SPEC"

pattern UploadTypeAppiumRubyTestSpec :: UploadType
pattern UploadTypeAppiumRubyTestSpec = UploadType' "APPIUM_RUBY_TEST_SPEC"

pattern UploadTypeAppiumWebJavaJunitTestSpec :: UploadType
pattern UploadTypeAppiumWebJavaJunitTestSpec = UploadType' "APPIUM_WEB_JAVA_JUNIT_TEST_SPEC"

pattern UploadTypeAppiumWebJavaTestngTestSpec :: UploadType
pattern UploadTypeAppiumWebJavaTestngTestSpec = UploadType' "APPIUM_WEB_JAVA_TESTNG_TEST_SPEC"

pattern UploadTypeAppiumWebPythonTestSpec :: UploadType
pattern UploadTypeAppiumWebPythonTestSpec = UploadType' "APPIUM_WEB_PYTHON_TEST_SPEC"

pattern UploadTypeAppiumWebNodeTestSpec :: UploadType
pattern UploadTypeAppiumWebNodeTestSpec = UploadType' "APPIUM_WEB_NODE_TEST_SPEC"

pattern UploadTypeAppiumWebRubyTestSpec :: UploadType
pattern UploadTypeAppiumWebRubyTestSpec = UploadType' "APPIUM_WEB_RUBY_TEST_SPEC"

pattern UploadTypeInstrumentationTestSpec :: UploadType
pattern UploadTypeInstrumentationTestSpec = UploadType' "INSTRUMENTATION_TEST_SPEC"

pattern UploadTypeXctestUiTestSpec :: UploadType
pattern UploadTypeXctestUiTestSpec = UploadType' "XCTEST_UI_TEST_SPEC"

{-# COMPLETE 
  UploadTypeAndroidApp,

  UploadTypeIosApp,

  UploadTypeWebApp,

  UploadTypeExternalData,

  UploadTypeAppiumJavaJunitTestPackage,

  UploadTypeAppiumJavaTestngTestPackage,

  UploadTypeAppiumPythonTestPackage,

  UploadTypeAppiumNodeTestPackage,

  UploadTypeAppiumRubyTestPackage,

  UploadTypeAppiumWebJavaJunitTestPackage,

  UploadTypeAppiumWebJavaTestngTestPackage,

  UploadTypeAppiumWebPythonTestPackage,

  UploadTypeAppiumWebNodeTestPackage,

  UploadTypeAppiumWebRubyTestPackage,

  UploadTypeCalabashTestPackage,

  UploadTypeInstrumentationTestPackage,

  UploadTypeUiautomationTestPackage,

  UploadTypeUiautomatorTestPackage,

  UploadTypeXctestTestPackage,

  UploadTypeXctestUiTestPackage,

  UploadTypeAppiumJavaJunitTestSpec,

  UploadTypeAppiumJavaTestngTestSpec,

  UploadTypeAppiumPythonTestSpec,

  UploadTypeAppiumNodeTestSpec,

  UploadTypeAppiumRubyTestSpec,

  UploadTypeAppiumWebJavaJunitTestSpec,

  UploadTypeAppiumWebJavaTestngTestSpec,

  UploadTypeAppiumWebPythonTestSpec,

  UploadTypeAppiumWebNodeTestSpec,

  UploadTypeAppiumWebRubyTestSpec,

  UploadTypeInstrumentationTestSpec,

  UploadTypeXctestUiTestSpec,
  UploadType'
  #-}
