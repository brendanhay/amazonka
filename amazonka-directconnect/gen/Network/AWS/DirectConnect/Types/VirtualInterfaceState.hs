{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.VirtualInterfaceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.VirtualInterfaceState
  ( VirtualInterfaceState
      ( ..,
        VirtualInterfaceState_Available,
        VirtualInterfaceState_Confirming,
        VirtualInterfaceState_Deleted,
        VirtualInterfaceState_Deleting,
        VirtualInterfaceState_Down,
        VirtualInterfaceState_Pending,
        VirtualInterfaceState_Rejected,
        VirtualInterfaceState_Unknown,
        VirtualInterfaceState_Verifying
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype VirtualInterfaceState = VirtualInterfaceState'
  { fromVirtualInterfaceState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern VirtualInterfaceState_Available :: VirtualInterfaceState
pattern VirtualInterfaceState_Available = VirtualInterfaceState' "available"

pattern VirtualInterfaceState_Confirming :: VirtualInterfaceState
pattern VirtualInterfaceState_Confirming = VirtualInterfaceState' "confirming"

pattern VirtualInterfaceState_Deleted :: VirtualInterfaceState
pattern VirtualInterfaceState_Deleted = VirtualInterfaceState' "deleted"

pattern VirtualInterfaceState_Deleting :: VirtualInterfaceState
pattern VirtualInterfaceState_Deleting = VirtualInterfaceState' "deleting"

pattern VirtualInterfaceState_Down :: VirtualInterfaceState
pattern VirtualInterfaceState_Down = VirtualInterfaceState' "down"

pattern VirtualInterfaceState_Pending :: VirtualInterfaceState
pattern VirtualInterfaceState_Pending = VirtualInterfaceState' "pending"

pattern VirtualInterfaceState_Rejected :: VirtualInterfaceState
pattern VirtualInterfaceState_Rejected = VirtualInterfaceState' "rejected"

pattern VirtualInterfaceState_Unknown :: VirtualInterfaceState
pattern VirtualInterfaceState_Unknown = VirtualInterfaceState' "unknown"

pattern VirtualInterfaceState_Verifying :: VirtualInterfaceState
pattern VirtualInterfaceState_Verifying = VirtualInterfaceState' "verifying"

{-# COMPLETE
  VirtualInterfaceState_Available,
  VirtualInterfaceState_Confirming,
  VirtualInterfaceState_Deleted,
  VirtualInterfaceState_Deleting,
  VirtualInterfaceState_Down,
  VirtualInterfaceState_Pending,
  VirtualInterfaceState_Rejected,
  VirtualInterfaceState_Unknown,
  VirtualInterfaceState_Verifying,
  VirtualInterfaceState'
  #-}
