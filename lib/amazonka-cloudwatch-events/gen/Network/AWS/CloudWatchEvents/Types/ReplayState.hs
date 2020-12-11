-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.ReplayState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ReplayState
  ( ReplayState
      ( ReplayState',
        Cancelled,
        Cancelling,
        Completed,
        Failed,
        Running,
        Starting
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReplayState = ReplayState' Lude.Text
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

pattern Cancelled :: ReplayState
pattern Cancelled = ReplayState' "CANCELLED"

pattern Cancelling :: ReplayState
pattern Cancelling = ReplayState' "CANCELLING"

pattern Completed :: ReplayState
pattern Completed = ReplayState' "COMPLETED"

pattern Failed :: ReplayState
pattern Failed = ReplayState' "FAILED"

pattern Running :: ReplayState
pattern Running = ReplayState' "RUNNING"

pattern Starting :: ReplayState
pattern Starting = ReplayState' "STARTING"

{-# COMPLETE
  Cancelled,
  Cancelling,
  Completed,
  Failed,
  Running,
  Starting,
  ReplayState'
  #-}
