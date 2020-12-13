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
        Queued,
        Running,
        Succeeded,
        Failed,
        Cancelled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype QueryExecutionState = QueryExecutionState' Lude.Text
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

pattern Queued :: QueryExecutionState
pattern Queued = QueryExecutionState' "QUEUED"

pattern Running :: QueryExecutionState
pattern Running = QueryExecutionState' "RUNNING"

pattern Succeeded :: QueryExecutionState
pattern Succeeded = QueryExecutionState' "SUCCEEDED"

pattern Failed :: QueryExecutionState
pattern Failed = QueryExecutionState' "FAILED"

pattern Cancelled :: QueryExecutionState
pattern Cancelled = QueryExecutionState' "CANCELLED"

{-# COMPLETE
  Queued,
  Running,
  Succeeded,
  Failed,
  Cancelled,
  QueryExecutionState'
  #-}
