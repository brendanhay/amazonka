{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.TestType
  ( TestType
    ( TestType'
    , TestTypeBuiltinFuzz
    , TestTypeBuiltinExplorer
    , TestTypeWebPerformanceProfile
    , TestTypeAppiumJavaJunit
    , TestTypeAppiumJavaTestng
    , TestTypeAppiumPython
    , TestTypeAppiumNode
    , TestTypeAppiumRuby
    , TestTypeAppiumWebJavaJunit
    , TestTypeAppiumWebJavaTestng
    , TestTypeAppiumWebPython
    , TestTypeAppiumWebNode
    , TestTypeAppiumWebRuby
    , TestTypeCalabash
    , TestTypeInstrumentation
    , TestTypeUiautomation
    , TestTypeUiautomator
    , TestTypeXctest
    , TestTypeXctestUi
    , TestTypeRemoteAccessRecord
    , TestTypeRemoteAccessReplay
    , fromTestType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TestType = TestType'{fromTestType :: Core.Text}
                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                     Core.Generic)
                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                       Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                       Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                       Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern TestTypeBuiltinFuzz :: TestType
pattern TestTypeBuiltinFuzz = TestType' "BUILTIN_FUZZ"

pattern TestTypeBuiltinExplorer :: TestType
pattern TestTypeBuiltinExplorer = TestType' "BUILTIN_EXPLORER"

pattern TestTypeWebPerformanceProfile :: TestType
pattern TestTypeWebPerformanceProfile = TestType' "WEB_PERFORMANCE_PROFILE"

pattern TestTypeAppiumJavaJunit :: TestType
pattern TestTypeAppiumJavaJunit = TestType' "APPIUM_JAVA_JUNIT"

pattern TestTypeAppiumJavaTestng :: TestType
pattern TestTypeAppiumJavaTestng = TestType' "APPIUM_JAVA_TESTNG"

pattern TestTypeAppiumPython :: TestType
pattern TestTypeAppiumPython = TestType' "APPIUM_PYTHON"

pattern TestTypeAppiumNode :: TestType
pattern TestTypeAppiumNode = TestType' "APPIUM_NODE"

pattern TestTypeAppiumRuby :: TestType
pattern TestTypeAppiumRuby = TestType' "APPIUM_RUBY"

pattern TestTypeAppiumWebJavaJunit :: TestType
pattern TestTypeAppiumWebJavaJunit = TestType' "APPIUM_WEB_JAVA_JUNIT"

pattern TestTypeAppiumWebJavaTestng :: TestType
pattern TestTypeAppiumWebJavaTestng = TestType' "APPIUM_WEB_JAVA_TESTNG"

pattern TestTypeAppiumWebPython :: TestType
pattern TestTypeAppiumWebPython = TestType' "APPIUM_WEB_PYTHON"

pattern TestTypeAppiumWebNode :: TestType
pattern TestTypeAppiumWebNode = TestType' "APPIUM_WEB_NODE"

pattern TestTypeAppiumWebRuby :: TestType
pattern TestTypeAppiumWebRuby = TestType' "APPIUM_WEB_RUBY"

pattern TestTypeCalabash :: TestType
pattern TestTypeCalabash = TestType' "CALABASH"

pattern TestTypeInstrumentation :: TestType
pattern TestTypeInstrumentation = TestType' "INSTRUMENTATION"

pattern TestTypeUiautomation :: TestType
pattern TestTypeUiautomation = TestType' "UIAUTOMATION"

pattern TestTypeUiautomator :: TestType
pattern TestTypeUiautomator = TestType' "UIAUTOMATOR"

pattern TestTypeXctest :: TestType
pattern TestTypeXctest = TestType' "XCTEST"

pattern TestTypeXctestUi :: TestType
pattern TestTypeXctestUi = TestType' "XCTEST_UI"

pattern TestTypeRemoteAccessRecord :: TestType
pattern TestTypeRemoteAccessRecord = TestType' "REMOTE_ACCESS_RECORD"

pattern TestTypeRemoteAccessReplay :: TestType
pattern TestTypeRemoteAccessReplay = TestType' "REMOTE_ACCESS_REPLAY"

{-# COMPLETE 
  TestTypeBuiltinFuzz,

  TestTypeBuiltinExplorer,

  TestTypeWebPerformanceProfile,

  TestTypeAppiumJavaJunit,

  TestTypeAppiumJavaTestng,

  TestTypeAppiumPython,

  TestTypeAppiumNode,

  TestTypeAppiumRuby,

  TestTypeAppiumWebJavaJunit,

  TestTypeAppiumWebJavaTestng,

  TestTypeAppiumWebPython,

  TestTypeAppiumWebNode,

  TestTypeAppiumWebRuby,

  TestTypeCalabash,

  TestTypeInstrumentation,

  TestTypeUiautomation,

  TestTypeUiautomator,

  TestTypeXctest,

  TestTypeXctestUi,

  TestTypeRemoteAccessRecord,

  TestTypeRemoteAccessReplay,
  TestType'
  #-}
