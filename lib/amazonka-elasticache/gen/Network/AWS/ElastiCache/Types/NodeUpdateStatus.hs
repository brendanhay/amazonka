-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeUpdateStatus
  ( NodeUpdateStatus
      ( NodeUpdateStatus',
        NUSComplete,
        NUSInProgress,
        NUSNotApplied,
        NUSStopped,
        NUSStopping,
        NUSWaitingToStart
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NodeUpdateStatus = NodeUpdateStatus' Lude.Text
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

pattern NUSComplete :: NodeUpdateStatus
pattern NUSComplete = NodeUpdateStatus' "complete"

pattern NUSInProgress :: NodeUpdateStatus
pattern NUSInProgress = NodeUpdateStatus' "in-progress"

pattern NUSNotApplied :: NodeUpdateStatus
pattern NUSNotApplied = NodeUpdateStatus' "not-applied"

pattern NUSStopped :: NodeUpdateStatus
pattern NUSStopped = NodeUpdateStatus' "stopped"

pattern NUSStopping :: NodeUpdateStatus
pattern NUSStopping = NodeUpdateStatus' "stopping"

pattern NUSWaitingToStart :: NodeUpdateStatus
pattern NUSWaitingToStart = NodeUpdateStatus' "waiting-to-start"

{-# COMPLETE
  NUSComplete,
  NUSInProgress,
  NUSNotApplied,
  NUSStopped,
  NUSStopping,
  NUSWaitingToStart,
  NodeUpdateStatus'
  #-}
