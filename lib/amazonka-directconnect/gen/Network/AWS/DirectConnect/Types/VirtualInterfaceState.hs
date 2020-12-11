-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.VirtualInterfaceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.VirtualInterfaceState
  ( VirtualInterfaceState
      ( VirtualInterfaceState',
        VISAvailable,
        VISConfirming,
        VISDeleted,
        VISDeleting,
        VISDown,
        VISPending,
        VISRejected,
        VISUnknown,
        VISVerifying
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VirtualInterfaceState = VirtualInterfaceState' Lude.Text
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

pattern VISAvailable :: VirtualInterfaceState
pattern VISAvailable = VirtualInterfaceState' "available"

pattern VISConfirming :: VirtualInterfaceState
pattern VISConfirming = VirtualInterfaceState' "confirming"

pattern VISDeleted :: VirtualInterfaceState
pattern VISDeleted = VirtualInterfaceState' "deleted"

pattern VISDeleting :: VirtualInterfaceState
pattern VISDeleting = VirtualInterfaceState' "deleting"

pattern VISDown :: VirtualInterfaceState
pattern VISDown = VirtualInterfaceState' "down"

pattern VISPending :: VirtualInterfaceState
pattern VISPending = VirtualInterfaceState' "pending"

pattern VISRejected :: VirtualInterfaceState
pattern VISRejected = VirtualInterfaceState' "rejected"

pattern VISUnknown :: VirtualInterfaceState
pattern VISUnknown = VirtualInterfaceState' "unknown"

pattern VISVerifying :: VirtualInterfaceState
pattern VISVerifying = VirtualInterfaceState' "verifying"

{-# COMPLETE
  VISAvailable,
  VISConfirming,
  VISDeleted,
  VISDeleting,
  VISDown,
  VISPending,
  VISRejected,
  VISUnknown,
  VISVerifying,
  VirtualInterfaceState'
  #-}
