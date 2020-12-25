{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.BGPPeerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.BGPPeerState
  ( BGPPeerState
      ( BGPPeerState',
        BGPPeerStateVerifying,
        BGPPeerStatePending,
        BGPPeerStateAvailable,
        BGPPeerStateDeleting,
        BGPPeerStateDeleted,
        fromBGPPeerState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype BGPPeerState = BGPPeerState' {fromBGPPeerState :: Core.Text}
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

pattern BGPPeerStateVerifying :: BGPPeerState
pattern BGPPeerStateVerifying = BGPPeerState' "verifying"

pattern BGPPeerStatePending :: BGPPeerState
pattern BGPPeerStatePending = BGPPeerState' "pending"

pattern BGPPeerStateAvailable :: BGPPeerState
pattern BGPPeerStateAvailable = BGPPeerState' "available"

pattern BGPPeerStateDeleting :: BGPPeerState
pattern BGPPeerStateDeleting = BGPPeerState' "deleting"

pattern BGPPeerStateDeleted :: BGPPeerState
pattern BGPPeerStateDeleted = BGPPeerState' "deleted"

{-# COMPLETE
  BGPPeerStateVerifying,
  BGPPeerStatePending,
  BGPPeerStateAvailable,
  BGPPeerStateDeleting,
  BGPPeerStateDeleted,
  BGPPeerState'
  #-}
