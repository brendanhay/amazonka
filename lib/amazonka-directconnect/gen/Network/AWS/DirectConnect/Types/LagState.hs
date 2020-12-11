-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.LagState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.LagState
  ( LagState
      ( LagState',
        LSAvailable,
        LSDeleted,
        LSDeleting,
        LSDown,
        LSPending,
        LSRequested,
        LSUnknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LagState = LagState' Lude.Text
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

pattern LSAvailable :: LagState
pattern LSAvailable = LagState' "available"

pattern LSDeleted :: LagState
pattern LSDeleted = LagState' "deleted"

pattern LSDeleting :: LagState
pattern LSDeleting = LagState' "deleting"

pattern LSDown :: LagState
pattern LSDown = LagState' "down"

pattern LSPending :: LagState
pattern LSPending = LagState' "pending"

pattern LSRequested :: LagState
pattern LSRequested = LagState' "requested"

pattern LSUnknown :: LagState
pattern LSUnknown = LagState' "unknown"

{-# COMPLETE
  LSAvailable,
  LSDeleted,
  LSDeleting,
  LSDown,
  LSPending,
  LSRequested,
  LSUnknown,
  LagState'
  #-}
