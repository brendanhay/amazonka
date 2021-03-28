{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.VirtualInterfaceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.VirtualInterfaceState
  ( VirtualInterfaceState
    ( VirtualInterfaceState'
    , VirtualInterfaceStateConfirming
    , VirtualInterfaceStateVerifying
    , VirtualInterfaceStatePending
    , VirtualInterfaceStateAvailable
    , VirtualInterfaceStateDown
    , VirtualInterfaceStateDeleting
    , VirtualInterfaceStateDeleted
    , VirtualInterfaceStateRejected
    , VirtualInterfaceStateUnknown
    , fromVirtualInterfaceState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype VirtualInterfaceState = VirtualInterfaceState'{fromVirtualInterfaceState
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern VirtualInterfaceStateConfirming :: VirtualInterfaceState
pattern VirtualInterfaceStateConfirming = VirtualInterfaceState' "confirming"

pattern VirtualInterfaceStateVerifying :: VirtualInterfaceState
pattern VirtualInterfaceStateVerifying = VirtualInterfaceState' "verifying"

pattern VirtualInterfaceStatePending :: VirtualInterfaceState
pattern VirtualInterfaceStatePending = VirtualInterfaceState' "pending"

pattern VirtualInterfaceStateAvailable :: VirtualInterfaceState
pattern VirtualInterfaceStateAvailable = VirtualInterfaceState' "available"

pattern VirtualInterfaceStateDown :: VirtualInterfaceState
pattern VirtualInterfaceStateDown = VirtualInterfaceState' "down"

pattern VirtualInterfaceStateDeleting :: VirtualInterfaceState
pattern VirtualInterfaceStateDeleting = VirtualInterfaceState' "deleting"

pattern VirtualInterfaceStateDeleted :: VirtualInterfaceState
pattern VirtualInterfaceStateDeleted = VirtualInterfaceState' "deleted"

pattern VirtualInterfaceStateRejected :: VirtualInterfaceState
pattern VirtualInterfaceStateRejected = VirtualInterfaceState' "rejected"

pattern VirtualInterfaceStateUnknown :: VirtualInterfaceState
pattern VirtualInterfaceStateUnknown = VirtualInterfaceState' "unknown"

{-# COMPLETE 
  VirtualInterfaceStateConfirming,

  VirtualInterfaceStateVerifying,

  VirtualInterfaceStatePending,

  VirtualInterfaceStateAvailable,

  VirtualInterfaceStateDown,

  VirtualInterfaceStateDeleting,

  VirtualInterfaceStateDeleted,

  VirtualInterfaceStateRejected,

  VirtualInterfaceStateUnknown,
  VirtualInterfaceState'
  #-}
