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
-- Module      : Amazonka.DeviceFarm.Types.ExecutionResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.ExecutionResult
  ( ExecutionResult
      ( ..,
        ExecutionResult_ERRORED,
        ExecutionResult_FAILED,
        ExecutionResult_PASSED,
        ExecutionResult_PENDING,
        ExecutionResult_SKIPPED,
        ExecutionResult_STOPPED,
        ExecutionResult_WARNED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExecutionResult = ExecutionResult'
  { fromExecutionResult ::
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

pattern ExecutionResult_ERRORED :: ExecutionResult
pattern ExecutionResult_ERRORED = ExecutionResult' "ERRORED"

pattern ExecutionResult_FAILED :: ExecutionResult
pattern ExecutionResult_FAILED = ExecutionResult' "FAILED"

pattern ExecutionResult_PASSED :: ExecutionResult
pattern ExecutionResult_PASSED = ExecutionResult' "PASSED"

pattern ExecutionResult_PENDING :: ExecutionResult
pattern ExecutionResult_PENDING = ExecutionResult' "PENDING"

pattern ExecutionResult_SKIPPED :: ExecutionResult
pattern ExecutionResult_SKIPPED = ExecutionResult' "SKIPPED"

pattern ExecutionResult_STOPPED :: ExecutionResult
pattern ExecutionResult_STOPPED = ExecutionResult' "STOPPED"

pattern ExecutionResult_WARNED :: ExecutionResult
pattern ExecutionResult_WARNED = ExecutionResult' "WARNED"

{-# COMPLETE
  ExecutionResult_ERRORED,
  ExecutionResult_FAILED,
  ExecutionResult_PASSED,
  ExecutionResult_PENDING,
  ExecutionResult_SKIPPED,
  ExecutionResult_STOPPED,
  ExecutionResult_WARNED,
  ExecutionResult'
  #-}
