{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.QueryExecutionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.QueryExecutionState
  ( QueryExecutionState
      ( QueryExecutionState',
        QueryExecutionStateQueued,
        QueryExecutionStateRunning,
        QueryExecutionStateSucceeded,
        QueryExecutionStateFailed,
        QueryExecutionStateCancelled,
        fromQueryExecutionState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype QueryExecutionState = QueryExecutionState'
  { fromQueryExecutionState ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern QueryExecutionStateQueued :: QueryExecutionState
pattern QueryExecutionStateQueued = QueryExecutionState' "QUEUED"

pattern QueryExecutionStateRunning :: QueryExecutionState
pattern QueryExecutionStateRunning = QueryExecutionState' "RUNNING"

pattern QueryExecutionStateSucceeded :: QueryExecutionState
pattern QueryExecutionStateSucceeded = QueryExecutionState' "SUCCEEDED"

pattern QueryExecutionStateFailed :: QueryExecutionState
pattern QueryExecutionStateFailed = QueryExecutionState' "FAILED"

pattern QueryExecutionStateCancelled :: QueryExecutionState
pattern QueryExecutionStateCancelled = QueryExecutionState' "CANCELLED"

{-# COMPLETE
  QueryExecutionStateQueued,
  QueryExecutionStateRunning,
  QueryExecutionStateSucceeded,
  QueryExecutionStateFailed,
  QueryExecutionStateCancelled,
  QueryExecutionState'
  #-}
