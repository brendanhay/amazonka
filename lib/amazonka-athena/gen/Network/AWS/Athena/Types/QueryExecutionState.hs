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
        Cancelled,
        Failed,
        Queued,
        Running,
        Succeeded
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

pattern Cancelled :: QueryExecutionState
pattern Cancelled = QueryExecutionState' "CANCELLED"

pattern Failed :: QueryExecutionState
pattern Failed = QueryExecutionState' "FAILED"

pattern Queued :: QueryExecutionState
pattern Queued = QueryExecutionState' "QUEUED"

pattern Running :: QueryExecutionState
pattern Running = QueryExecutionState' "RUNNING"

pattern Succeeded :: QueryExecutionState
pattern Succeeded = QueryExecutionState' "SUCCEEDED"

{-# COMPLETE
  Cancelled,
  Failed,
  Queued,
  Running,
  Succeeded,
  QueryExecutionState'
  #-}
