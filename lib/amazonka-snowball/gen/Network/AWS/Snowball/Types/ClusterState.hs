-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ClusterState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ClusterState
  ( ClusterState
      ( ClusterState',
        AwaitingQuorum,
        Cancelled,
        Complete,
        InUse,
        Pending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ClusterState = ClusterState' Lude.Text
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

pattern AwaitingQuorum :: ClusterState
pattern AwaitingQuorum = ClusterState' "AwaitingQuorum"

pattern Cancelled :: ClusterState
pattern Cancelled = ClusterState' "Cancelled"

pattern Complete :: ClusterState
pattern Complete = ClusterState' "Complete"

pattern InUse :: ClusterState
pattern InUse = ClusterState' "InUse"

pattern Pending :: ClusterState
pattern Pending = ClusterState' "Pending"

{-# COMPLETE
  AwaitingQuorum,
  Cancelled,
  Complete,
  InUse,
  Pending,
  ClusterState'
  #-}
