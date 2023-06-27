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
-- Module      : Amazonka.LexV2Models.Types.TestExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestExecutionStatus
  ( TestExecutionStatus
      ( ..,
        TestExecutionStatus_Completed,
        TestExecutionStatus_Failed,
        TestExecutionStatus_InProgress,
        TestExecutionStatus_Pending,
        TestExecutionStatus_Stopped,
        TestExecutionStatus_Stopping,
        TestExecutionStatus_Waiting
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TestExecutionStatus = TestExecutionStatus'
  { fromTestExecutionStatus ::
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

pattern TestExecutionStatus_Completed :: TestExecutionStatus
pattern TestExecutionStatus_Completed = TestExecutionStatus' "Completed"

pattern TestExecutionStatus_Failed :: TestExecutionStatus
pattern TestExecutionStatus_Failed = TestExecutionStatus' "Failed"

pattern TestExecutionStatus_InProgress :: TestExecutionStatus
pattern TestExecutionStatus_InProgress = TestExecutionStatus' "InProgress"

pattern TestExecutionStatus_Pending :: TestExecutionStatus
pattern TestExecutionStatus_Pending = TestExecutionStatus' "Pending"

pattern TestExecutionStatus_Stopped :: TestExecutionStatus
pattern TestExecutionStatus_Stopped = TestExecutionStatus' "Stopped"

pattern TestExecutionStatus_Stopping :: TestExecutionStatus
pattern TestExecutionStatus_Stopping = TestExecutionStatus' "Stopping"

pattern TestExecutionStatus_Waiting :: TestExecutionStatus
pattern TestExecutionStatus_Waiting = TestExecutionStatus' "Waiting"

{-# COMPLETE
  TestExecutionStatus_Completed,
  TestExecutionStatus_Failed,
  TestExecutionStatus_InProgress,
  TestExecutionStatus_Pending,
  TestExecutionStatus_Stopped,
  TestExecutionStatus_Stopping,
  TestExecutionStatus_Waiting,
  TestExecutionStatus'
  #-}
