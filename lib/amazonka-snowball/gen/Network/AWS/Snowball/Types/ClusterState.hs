{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        CSAwaitingQuorum,
        CSPending,
        CSInUse,
        CSComplete,
        CSCancelled
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

pattern CSAwaitingQuorum :: ClusterState
pattern CSAwaitingQuorum = ClusterState' "AwaitingQuorum"

pattern CSPending :: ClusterState
pattern CSPending = ClusterState' "Pending"

pattern CSInUse :: ClusterState
pattern CSInUse = ClusterState' "InUse"

pattern CSComplete :: ClusterState
pattern CSComplete = ClusterState' "Complete"

pattern CSCancelled :: ClusterState
pattern CSCancelled = ClusterState' "Cancelled"

{-# COMPLETE
  CSAwaitingQuorum,
  CSPending,
  CSInUse,
  CSComplete,
  CSCancelled,
  ClusterState'
  #-}
