{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        VISConfirming,
        VISVerifying,
        VISPending,
        VISAvailable,
        VISDown,
        VISDeleting,
        VISDeleted,
        VISRejected,
        VISUnknown
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

pattern VISConfirming :: VirtualInterfaceState
pattern VISConfirming = VirtualInterfaceState' "confirming"

pattern VISVerifying :: VirtualInterfaceState
pattern VISVerifying = VirtualInterfaceState' "verifying"

pattern VISPending :: VirtualInterfaceState
pattern VISPending = VirtualInterfaceState' "pending"

pattern VISAvailable :: VirtualInterfaceState
pattern VISAvailable = VirtualInterfaceState' "available"

pattern VISDown :: VirtualInterfaceState
pattern VISDown = VirtualInterfaceState' "down"

pattern VISDeleting :: VirtualInterfaceState
pattern VISDeleting = VirtualInterfaceState' "deleting"

pattern VISDeleted :: VirtualInterfaceState
pattern VISDeleted = VirtualInterfaceState' "deleted"

pattern VISRejected :: VirtualInterfaceState
pattern VISRejected = VirtualInterfaceState' "rejected"

pattern VISUnknown :: VirtualInterfaceState
pattern VISUnknown = VirtualInterfaceState' "unknown"

{-# COMPLETE
  VISConfirming,
  VISVerifying,
  VISPending,
  VISAvailable,
  VISDown,
  VISDeleting,
  VISDeleted,
  VISRejected,
  VISUnknown,
  VirtualInterfaceState'
  #-}
