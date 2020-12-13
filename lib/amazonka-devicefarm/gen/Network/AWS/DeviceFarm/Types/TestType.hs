{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestType
  ( TestType
      ( TestType',
        BuiltinFuzz,
        BuiltinExplorer,
        WebPerformanceProfile,
        AppiumJavaJunit,
        AppiumJavaTestng,
        AppiumPython,
        AppiumNode,
        AppiumRuby,
        AppiumWebJavaJunit,
        AppiumWebJavaTestng,
        AppiumWebPython,
        AppiumWebNode,
        AppiumWebRuby,
        Calabash,
        Instrumentation,
        Uiautomation,
        Uiautomator,
        Xctest,
        XctestUi,
        RemoteAccessRecord,
        RemoteAccessReplay
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TestType = TestType' Lude.Text
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

pattern BuiltinFuzz :: TestType
pattern BuiltinFuzz = TestType' "BUILTIN_FUZZ"

pattern BuiltinExplorer :: TestType
pattern BuiltinExplorer = TestType' "BUILTIN_EXPLORER"

pattern WebPerformanceProfile :: TestType
pattern WebPerformanceProfile = TestType' "WEB_PERFORMANCE_PROFILE"

pattern AppiumJavaJunit :: TestType
pattern AppiumJavaJunit = TestType' "APPIUM_JAVA_JUNIT"

pattern AppiumJavaTestng :: TestType
pattern AppiumJavaTestng = TestType' "APPIUM_JAVA_TESTNG"

pattern AppiumPython :: TestType
pattern AppiumPython = TestType' "APPIUM_PYTHON"

pattern AppiumNode :: TestType
pattern AppiumNode = TestType' "APPIUM_NODE"

pattern AppiumRuby :: TestType
pattern AppiumRuby = TestType' "APPIUM_RUBY"

pattern AppiumWebJavaJunit :: TestType
pattern AppiumWebJavaJunit = TestType' "APPIUM_WEB_JAVA_JUNIT"

pattern AppiumWebJavaTestng :: TestType
pattern AppiumWebJavaTestng = TestType' "APPIUM_WEB_JAVA_TESTNG"

pattern AppiumWebPython :: TestType
pattern AppiumWebPython = TestType' "APPIUM_WEB_PYTHON"

pattern AppiumWebNode :: TestType
pattern AppiumWebNode = TestType' "APPIUM_WEB_NODE"

pattern AppiumWebRuby :: TestType
pattern AppiumWebRuby = TestType' "APPIUM_WEB_RUBY"

pattern Calabash :: TestType
pattern Calabash = TestType' "CALABASH"

pattern Instrumentation :: TestType
pattern Instrumentation = TestType' "INSTRUMENTATION"

pattern Uiautomation :: TestType
pattern Uiautomation = TestType' "UIAUTOMATION"

pattern Uiautomator :: TestType
pattern Uiautomator = TestType' "UIAUTOMATOR"

pattern Xctest :: TestType
pattern Xctest = TestType' "XCTEST"

pattern XctestUi :: TestType
pattern XctestUi = TestType' "XCTEST_UI"

pattern RemoteAccessRecord :: TestType
pattern RemoteAccessRecord = TestType' "REMOTE_ACCESS_RECORD"

pattern RemoteAccessReplay :: TestType
pattern RemoteAccessReplay = TestType' "REMOTE_ACCESS_REPLAY"

{-# COMPLETE
  BuiltinFuzz,
  BuiltinExplorer,
  WebPerformanceProfile,
  AppiumJavaJunit,
  AppiumJavaTestng,
  AppiumPython,
  AppiumNode,
  AppiumRuby,
  AppiumWebJavaJunit,
  AppiumWebJavaTestng,
  AppiumWebPython,
  AppiumWebNode,
  AppiumWebRuby,
  Calabash,
  Instrumentation,
  Uiautomation,
  Uiautomator,
  Xctest,
  XctestUi,
  RemoteAccessRecord,
  RemoteAccessReplay,
  TestType'
  #-}
