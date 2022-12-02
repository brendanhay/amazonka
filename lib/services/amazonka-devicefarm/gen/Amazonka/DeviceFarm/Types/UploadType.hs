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
-- Module      : Amazonka.DeviceFarm.Types.UploadType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.UploadType
  ( UploadType
      ( ..,
        UploadType_ANDROID_APP,
        UploadType_APPIUM_JAVA_JUNIT_TEST_PACKAGE,
        UploadType_APPIUM_JAVA_JUNIT_TEST_SPEC,
        UploadType_APPIUM_JAVA_TESTNG_TEST_PACKAGE,
        UploadType_APPIUM_JAVA_TESTNG_TEST_SPEC,
        UploadType_APPIUM_NODE_TEST_PACKAGE,
        UploadType_APPIUM_NODE_TEST_SPEC,
        UploadType_APPIUM_PYTHON_TEST_PACKAGE,
        UploadType_APPIUM_PYTHON_TEST_SPEC,
        UploadType_APPIUM_RUBY_TEST_PACKAGE,
        UploadType_APPIUM_RUBY_TEST_SPEC,
        UploadType_APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE,
        UploadType_APPIUM_WEB_JAVA_JUNIT_TEST_SPEC,
        UploadType_APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE,
        UploadType_APPIUM_WEB_JAVA_TESTNG_TEST_SPEC,
        UploadType_APPIUM_WEB_NODE_TEST_PACKAGE,
        UploadType_APPIUM_WEB_NODE_TEST_SPEC,
        UploadType_APPIUM_WEB_PYTHON_TEST_PACKAGE,
        UploadType_APPIUM_WEB_PYTHON_TEST_SPEC,
        UploadType_APPIUM_WEB_RUBY_TEST_PACKAGE,
        UploadType_APPIUM_WEB_RUBY_TEST_SPEC,
        UploadType_CALABASH_TEST_PACKAGE,
        UploadType_EXTERNAL_DATA,
        UploadType_INSTRUMENTATION_TEST_PACKAGE,
        UploadType_INSTRUMENTATION_TEST_SPEC,
        UploadType_IOS_APP,
        UploadType_UIAUTOMATION_TEST_PACKAGE,
        UploadType_UIAUTOMATOR_TEST_PACKAGE,
        UploadType_WEB_APP,
        UploadType_XCTEST_TEST_PACKAGE,
        UploadType_XCTEST_UI_TEST_PACKAGE,
        UploadType_XCTEST_UI_TEST_SPEC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UploadType = UploadType'
  { fromUploadType ::
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

pattern UploadType_ANDROID_APP :: UploadType
pattern UploadType_ANDROID_APP = UploadType' "ANDROID_APP"

pattern UploadType_APPIUM_JAVA_JUNIT_TEST_PACKAGE :: UploadType
pattern UploadType_APPIUM_JAVA_JUNIT_TEST_PACKAGE = UploadType' "APPIUM_JAVA_JUNIT_TEST_PACKAGE"

pattern UploadType_APPIUM_JAVA_JUNIT_TEST_SPEC :: UploadType
pattern UploadType_APPIUM_JAVA_JUNIT_TEST_SPEC = UploadType' "APPIUM_JAVA_JUNIT_TEST_SPEC"

pattern UploadType_APPIUM_JAVA_TESTNG_TEST_PACKAGE :: UploadType
pattern UploadType_APPIUM_JAVA_TESTNG_TEST_PACKAGE = UploadType' "APPIUM_JAVA_TESTNG_TEST_PACKAGE"

pattern UploadType_APPIUM_JAVA_TESTNG_TEST_SPEC :: UploadType
pattern UploadType_APPIUM_JAVA_TESTNG_TEST_SPEC = UploadType' "APPIUM_JAVA_TESTNG_TEST_SPEC"

pattern UploadType_APPIUM_NODE_TEST_PACKAGE :: UploadType
pattern UploadType_APPIUM_NODE_TEST_PACKAGE = UploadType' "APPIUM_NODE_TEST_PACKAGE"

pattern UploadType_APPIUM_NODE_TEST_SPEC :: UploadType
pattern UploadType_APPIUM_NODE_TEST_SPEC = UploadType' "APPIUM_NODE_TEST_SPEC"

pattern UploadType_APPIUM_PYTHON_TEST_PACKAGE :: UploadType
pattern UploadType_APPIUM_PYTHON_TEST_PACKAGE = UploadType' "APPIUM_PYTHON_TEST_PACKAGE"

pattern UploadType_APPIUM_PYTHON_TEST_SPEC :: UploadType
pattern UploadType_APPIUM_PYTHON_TEST_SPEC = UploadType' "APPIUM_PYTHON_TEST_SPEC"

pattern UploadType_APPIUM_RUBY_TEST_PACKAGE :: UploadType
pattern UploadType_APPIUM_RUBY_TEST_PACKAGE = UploadType' "APPIUM_RUBY_TEST_PACKAGE"

pattern UploadType_APPIUM_RUBY_TEST_SPEC :: UploadType
pattern UploadType_APPIUM_RUBY_TEST_SPEC = UploadType' "APPIUM_RUBY_TEST_SPEC"

pattern UploadType_APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE :: UploadType
pattern UploadType_APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE = UploadType' "APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE"

pattern UploadType_APPIUM_WEB_JAVA_JUNIT_TEST_SPEC :: UploadType
pattern UploadType_APPIUM_WEB_JAVA_JUNIT_TEST_SPEC = UploadType' "APPIUM_WEB_JAVA_JUNIT_TEST_SPEC"

pattern UploadType_APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE :: UploadType
pattern UploadType_APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE = UploadType' "APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE"

pattern UploadType_APPIUM_WEB_JAVA_TESTNG_TEST_SPEC :: UploadType
pattern UploadType_APPIUM_WEB_JAVA_TESTNG_TEST_SPEC = UploadType' "APPIUM_WEB_JAVA_TESTNG_TEST_SPEC"

pattern UploadType_APPIUM_WEB_NODE_TEST_PACKAGE :: UploadType
pattern UploadType_APPIUM_WEB_NODE_TEST_PACKAGE = UploadType' "APPIUM_WEB_NODE_TEST_PACKAGE"

pattern UploadType_APPIUM_WEB_NODE_TEST_SPEC :: UploadType
pattern UploadType_APPIUM_WEB_NODE_TEST_SPEC = UploadType' "APPIUM_WEB_NODE_TEST_SPEC"

pattern UploadType_APPIUM_WEB_PYTHON_TEST_PACKAGE :: UploadType
pattern UploadType_APPIUM_WEB_PYTHON_TEST_PACKAGE = UploadType' "APPIUM_WEB_PYTHON_TEST_PACKAGE"

pattern UploadType_APPIUM_WEB_PYTHON_TEST_SPEC :: UploadType
pattern UploadType_APPIUM_WEB_PYTHON_TEST_SPEC = UploadType' "APPIUM_WEB_PYTHON_TEST_SPEC"

pattern UploadType_APPIUM_WEB_RUBY_TEST_PACKAGE :: UploadType
pattern UploadType_APPIUM_WEB_RUBY_TEST_PACKAGE = UploadType' "APPIUM_WEB_RUBY_TEST_PACKAGE"

pattern UploadType_APPIUM_WEB_RUBY_TEST_SPEC :: UploadType
pattern UploadType_APPIUM_WEB_RUBY_TEST_SPEC = UploadType' "APPIUM_WEB_RUBY_TEST_SPEC"

pattern UploadType_CALABASH_TEST_PACKAGE :: UploadType
pattern UploadType_CALABASH_TEST_PACKAGE = UploadType' "CALABASH_TEST_PACKAGE"

pattern UploadType_EXTERNAL_DATA :: UploadType
pattern UploadType_EXTERNAL_DATA = UploadType' "EXTERNAL_DATA"

pattern UploadType_INSTRUMENTATION_TEST_PACKAGE :: UploadType
pattern UploadType_INSTRUMENTATION_TEST_PACKAGE = UploadType' "INSTRUMENTATION_TEST_PACKAGE"

pattern UploadType_INSTRUMENTATION_TEST_SPEC :: UploadType
pattern UploadType_INSTRUMENTATION_TEST_SPEC = UploadType' "INSTRUMENTATION_TEST_SPEC"

pattern UploadType_IOS_APP :: UploadType
pattern UploadType_IOS_APP = UploadType' "IOS_APP"

pattern UploadType_UIAUTOMATION_TEST_PACKAGE :: UploadType
pattern UploadType_UIAUTOMATION_TEST_PACKAGE = UploadType' "UIAUTOMATION_TEST_PACKAGE"

pattern UploadType_UIAUTOMATOR_TEST_PACKAGE :: UploadType
pattern UploadType_UIAUTOMATOR_TEST_PACKAGE = UploadType' "UIAUTOMATOR_TEST_PACKAGE"

pattern UploadType_WEB_APP :: UploadType
pattern UploadType_WEB_APP = UploadType' "WEB_APP"

pattern UploadType_XCTEST_TEST_PACKAGE :: UploadType
pattern UploadType_XCTEST_TEST_PACKAGE = UploadType' "XCTEST_TEST_PACKAGE"

pattern UploadType_XCTEST_UI_TEST_PACKAGE :: UploadType
pattern UploadType_XCTEST_UI_TEST_PACKAGE = UploadType' "XCTEST_UI_TEST_PACKAGE"

pattern UploadType_XCTEST_UI_TEST_SPEC :: UploadType
pattern UploadType_XCTEST_UI_TEST_SPEC = UploadType' "XCTEST_UI_TEST_SPEC"

{-# COMPLETE
  UploadType_ANDROID_APP,
  UploadType_APPIUM_JAVA_JUNIT_TEST_PACKAGE,
  UploadType_APPIUM_JAVA_JUNIT_TEST_SPEC,
  UploadType_APPIUM_JAVA_TESTNG_TEST_PACKAGE,
  UploadType_APPIUM_JAVA_TESTNG_TEST_SPEC,
  UploadType_APPIUM_NODE_TEST_PACKAGE,
  UploadType_APPIUM_NODE_TEST_SPEC,
  UploadType_APPIUM_PYTHON_TEST_PACKAGE,
  UploadType_APPIUM_PYTHON_TEST_SPEC,
  UploadType_APPIUM_RUBY_TEST_PACKAGE,
  UploadType_APPIUM_RUBY_TEST_SPEC,
  UploadType_APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE,
  UploadType_APPIUM_WEB_JAVA_JUNIT_TEST_SPEC,
  UploadType_APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE,
  UploadType_APPIUM_WEB_JAVA_TESTNG_TEST_SPEC,
  UploadType_APPIUM_WEB_NODE_TEST_PACKAGE,
  UploadType_APPIUM_WEB_NODE_TEST_SPEC,
  UploadType_APPIUM_WEB_PYTHON_TEST_PACKAGE,
  UploadType_APPIUM_WEB_PYTHON_TEST_SPEC,
  UploadType_APPIUM_WEB_RUBY_TEST_PACKAGE,
  UploadType_APPIUM_WEB_RUBY_TEST_SPEC,
  UploadType_CALABASH_TEST_PACKAGE,
  UploadType_EXTERNAL_DATA,
  UploadType_INSTRUMENTATION_TEST_PACKAGE,
  UploadType_INSTRUMENTATION_TEST_SPEC,
  UploadType_IOS_APP,
  UploadType_UIAUTOMATION_TEST_PACKAGE,
  UploadType_UIAUTOMATOR_TEST_PACKAGE,
  UploadType_WEB_APP,
  UploadType_XCTEST_TEST_PACKAGE,
  UploadType_XCTEST_UI_TEST_PACKAGE,
  UploadType_XCTEST_UI_TEST_SPEC,
  UploadType'
  #-}
