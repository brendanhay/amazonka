{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.ExecutionResult
  ( ExecutionResult
    ( ExecutionResult'
    , ExecutionResultPending
    , ExecutionResultPassed
    , ExecutionResultWarned
    , ExecutionResultFailed
    , ExecutionResultSkipped
    , ExecutionResultErrored
    , ExecutionResultStopped
    , fromExecutionResult
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ExecutionResult = ExecutionResult'{fromExecutionResult ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern ExecutionResultPending :: ExecutionResult
pattern ExecutionResultPending = ExecutionResult' "PENDING"

pattern ExecutionResultPassed :: ExecutionResult
pattern ExecutionResultPassed = ExecutionResult' "PASSED"

pattern ExecutionResultWarned :: ExecutionResult
pattern ExecutionResultWarned = ExecutionResult' "WARNED"

pattern ExecutionResultFailed :: ExecutionResult
pattern ExecutionResultFailed = ExecutionResult' "FAILED"

pattern ExecutionResultSkipped :: ExecutionResult
pattern ExecutionResultSkipped = ExecutionResult' "SKIPPED"

pattern ExecutionResultErrored :: ExecutionResult
pattern ExecutionResultErrored = ExecutionResult' "ERRORED"

pattern ExecutionResultStopped :: ExecutionResult
pattern ExecutionResultStopped = ExecutionResult' "STOPPED"

{-# COMPLETE 
  ExecutionResultPending,

  ExecutionResultPassed,

  ExecutionResultWarned,

  ExecutionResultFailed,

  ExecutionResultSkipped,

  ExecutionResultErrored,

  ExecutionResultStopped,
  ExecutionResult'
  #-}
