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
-- Module      : Amazonka.LexV2Models.Types.TestResultMatchStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestResultMatchStatus
  ( TestResultMatchStatus
      ( ..,
        TestResultMatchStatus_ExecutionError,
        TestResultMatchStatus_Matched,
        TestResultMatchStatus_Mismatched
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TestResultMatchStatus = TestResultMatchStatus'
  { fromTestResultMatchStatus ::
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

pattern TestResultMatchStatus_ExecutionError :: TestResultMatchStatus
pattern TestResultMatchStatus_ExecutionError = TestResultMatchStatus' "ExecutionError"

pattern TestResultMatchStatus_Matched :: TestResultMatchStatus
pattern TestResultMatchStatus_Matched = TestResultMatchStatus' "Matched"

pattern TestResultMatchStatus_Mismatched :: TestResultMatchStatus
pattern TestResultMatchStatus_Mismatched = TestResultMatchStatus' "Mismatched"

{-# COMPLETE
  TestResultMatchStatus_ExecutionError,
  TestResultMatchStatus_Matched,
  TestResultMatchStatus_Mismatched,
  TestResultMatchStatus'
  #-}
