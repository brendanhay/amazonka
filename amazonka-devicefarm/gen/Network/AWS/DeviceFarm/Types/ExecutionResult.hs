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
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ExecutionResult
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

import qualified Network.AWS.Core as Core

newtype ExecutionResult = ExecutionResult'
  { fromExecutionResult ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
