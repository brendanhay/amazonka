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
-- Module      : Amazonka.DirectConnect.Types.VirtualInterfaceState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.VirtualInterfaceState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VirtualInterfaceState = VirtualInterfaceState'
  { fromVirtualInterfaceState ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
