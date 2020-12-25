{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.ConnectionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.ConnectionState
  ( ConnectionState
      ( ConnectionState',
        ConnectionStateOrdering,
        ConnectionStateRequested,
        ConnectionStatePending,
        ConnectionStateAvailable,
        ConnectionStateDown,
        ConnectionStateDeleting,
        ConnectionStateDeleted,
        ConnectionStateRejected,
        ConnectionStateUnknown,
        fromConnectionState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ConnectionState = ConnectionState'
  { fromConnectionState ::
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

pattern ConnectionStateOrdering :: ConnectionState
pattern ConnectionStateOrdering = ConnectionState' "ordering"

pattern ConnectionStateRequested :: ConnectionState
pattern ConnectionStateRequested = ConnectionState' "requested"

pattern ConnectionStatePending :: ConnectionState
pattern ConnectionStatePending = ConnectionState' "pending"

pattern ConnectionStateAvailable :: ConnectionState
pattern ConnectionStateAvailable = ConnectionState' "available"

pattern ConnectionStateDown :: ConnectionState
pattern ConnectionStateDown = ConnectionState' "down"

pattern ConnectionStateDeleting :: ConnectionState
pattern ConnectionStateDeleting = ConnectionState' "deleting"

pattern ConnectionStateDeleted :: ConnectionState
pattern ConnectionStateDeleted = ConnectionState' "deleted"

pattern ConnectionStateRejected :: ConnectionState
pattern ConnectionStateRejected = ConnectionState' "rejected"

pattern ConnectionStateUnknown :: ConnectionState
pattern ConnectionStateUnknown = ConnectionState' "unknown"

{-# COMPLETE
  ConnectionStateOrdering,
  ConnectionStateRequested,
  ConnectionStatePending,
  ConnectionStateAvailable,
  ConnectionStateDown,
  ConnectionStateDeleting,
  ConnectionStateDeleted,
  ConnectionStateRejected,
  ConnectionStateUnknown,
  ConnectionState'
  #-}
