{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ExecutionResult = ExecutionResult'
  { fromExecutionResult ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
