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
-- Module      : Amazonka.IoTDeviceAdvisor.Types.TestCaseScenarioStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.TestCaseScenarioStatus
  ( TestCaseScenarioStatus
      ( ..,
        TestCaseScenarioStatus_CANCELED,
        TestCaseScenarioStatus_ERROR,
        TestCaseScenarioStatus_FAIL,
        TestCaseScenarioStatus_PASS,
        TestCaseScenarioStatus_PASS_WITH_WARNINGS,
        TestCaseScenarioStatus_PENDING,
        TestCaseScenarioStatus_RUNNING,
        TestCaseScenarioStatus_STOPPED,
        TestCaseScenarioStatus_STOPPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TestCaseScenarioStatus = TestCaseScenarioStatus'
  { fromTestCaseScenarioStatus ::
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

pattern TestCaseScenarioStatus_CANCELED :: TestCaseScenarioStatus
pattern TestCaseScenarioStatus_CANCELED = TestCaseScenarioStatus' "CANCELED"

pattern TestCaseScenarioStatus_ERROR :: TestCaseScenarioStatus
pattern TestCaseScenarioStatus_ERROR = TestCaseScenarioStatus' "ERROR"

pattern TestCaseScenarioStatus_FAIL :: TestCaseScenarioStatus
pattern TestCaseScenarioStatus_FAIL = TestCaseScenarioStatus' "FAIL"

pattern TestCaseScenarioStatus_PASS :: TestCaseScenarioStatus
pattern TestCaseScenarioStatus_PASS = TestCaseScenarioStatus' "PASS"

pattern TestCaseScenarioStatus_PASS_WITH_WARNINGS :: TestCaseScenarioStatus
pattern TestCaseScenarioStatus_PASS_WITH_WARNINGS = TestCaseScenarioStatus' "PASS_WITH_WARNINGS"

pattern TestCaseScenarioStatus_PENDING :: TestCaseScenarioStatus
pattern TestCaseScenarioStatus_PENDING = TestCaseScenarioStatus' "PENDING"

pattern TestCaseScenarioStatus_RUNNING :: TestCaseScenarioStatus
pattern TestCaseScenarioStatus_RUNNING = TestCaseScenarioStatus' "RUNNING"

pattern TestCaseScenarioStatus_STOPPED :: TestCaseScenarioStatus
pattern TestCaseScenarioStatus_STOPPED = TestCaseScenarioStatus' "STOPPED"

pattern TestCaseScenarioStatus_STOPPING :: TestCaseScenarioStatus
pattern TestCaseScenarioStatus_STOPPING = TestCaseScenarioStatus' "STOPPING"

{-# COMPLETE
  TestCaseScenarioStatus_CANCELED,
  TestCaseScenarioStatus_ERROR,
  TestCaseScenarioStatus_FAIL,
  TestCaseScenarioStatus_PASS,
  TestCaseScenarioStatus_PASS_WITH_WARNINGS,
  TestCaseScenarioStatus_PENDING,
  TestCaseScenarioStatus_RUNNING,
  TestCaseScenarioStatus_STOPPED,
  TestCaseScenarioStatus_STOPPING,
  TestCaseScenarioStatus'
  #-}
