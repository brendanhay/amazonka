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
        CSOrdering,
        CSRequested,
        CSPending,
        CSAvailable,
        CSDown,
        CSDeleting,
        CSDeleted,
        CSRejected,
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

pattern CSOrdering :: ConnectionState
pattern CSOrdering = ConnectionState' "ordering"

pattern CSRequested :: ConnectionState
pattern CSRequested = ConnectionState' "requested"

pattern CSPending :: ConnectionState
pattern CSPending = ConnectionState' "pending"

pattern CSAvailable :: ConnectionState
pattern CSAvailable = ConnectionState' "available"

pattern CSDown :: ConnectionState
pattern CSDown = ConnectionState' "down"

pattern CSDeleting :: ConnectionState
pattern CSDeleting = ConnectionState' "deleting"

pattern CSDeleted :: ConnectionState
pattern CSDeleted = ConnectionState' "deleted"

pattern CSRejected :: ConnectionState
pattern CSRejected = ConnectionState' "rejected"

pattern CSUnknown :: ConnectionState
pattern CSUnknown = ConnectionState' "unknown"

{-# COMPLETE
  CSOrdering,
  CSRequested,
  CSPending,
  CSAvailable,
  CSDown,
  CSDeleting,
  CSDeleted,
  CSRejected,
  CSUnknown,
  ConnectionState'
  #-}
