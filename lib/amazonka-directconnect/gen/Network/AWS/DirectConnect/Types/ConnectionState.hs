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
        CSAvailable,
        CSDeleted,
        CSDeleting,
        CSDown,
        CSOrdering,
        CSPending,
        CSRejected,
        CSRequested,
        CSUnknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConnectionState = ConnectionState' Lude.Text
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

pattern CSAvailable :: ConnectionState
pattern CSAvailable = ConnectionState' "available"

pattern CSDeleted :: ConnectionState
pattern CSDeleted = ConnectionState' "deleted"

pattern CSDeleting :: ConnectionState
pattern CSDeleting = ConnectionState' "deleting"

pattern CSDown :: ConnectionState
pattern CSDown = ConnectionState' "down"

pattern CSOrdering :: ConnectionState
pattern CSOrdering = ConnectionState' "ordering"

pattern CSPending :: ConnectionState
pattern CSPending = ConnectionState' "pending"

pattern CSRejected :: ConnectionState
pattern CSRejected = ConnectionState' "rejected"

pattern CSRequested :: ConnectionState
pattern CSRequested = ConnectionState' "requested"

pattern CSUnknown :: ConnectionState
pattern CSUnknown = ConnectionState' "unknown"

{-# COMPLETE
  CSAvailable,
  CSDeleted,
  CSDeleting,
  CSDown,
  CSOrdering,
  CSPending,
  CSRejected,
  CSRequested,
  CSUnknown,
  ConnectionState'
  #-}
