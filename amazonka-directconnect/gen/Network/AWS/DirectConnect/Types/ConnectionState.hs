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
-- Module      : Network.AWS.DirectConnect.Types.ConnectionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.ConnectionState
  ( ConnectionState
      ( ..,
        ConnectionState_Available,
        ConnectionState_Deleted,
        ConnectionState_Deleting,
        ConnectionState_Down,
        ConnectionState_Ordering,
        ConnectionState_Pending,
        ConnectionState_Rejected,
        ConnectionState_Requested,
        ConnectionState_Unknown
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ConnectionState = ConnectionState'
  { fromConnectionState ::
      Core.Text
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

pattern ConnectionState_Available :: ConnectionState
pattern ConnectionState_Available = ConnectionState' "available"

pattern ConnectionState_Deleted :: ConnectionState
pattern ConnectionState_Deleted = ConnectionState' "deleted"

pattern ConnectionState_Deleting :: ConnectionState
pattern ConnectionState_Deleting = ConnectionState' "deleting"

pattern ConnectionState_Down :: ConnectionState
pattern ConnectionState_Down = ConnectionState' "down"

pattern ConnectionState_Ordering :: ConnectionState
pattern ConnectionState_Ordering = ConnectionState' "ordering"

pattern ConnectionState_Pending :: ConnectionState
pattern ConnectionState_Pending = ConnectionState' "pending"

pattern ConnectionState_Rejected :: ConnectionState
pattern ConnectionState_Rejected = ConnectionState' "rejected"

pattern ConnectionState_Requested :: ConnectionState
pattern ConnectionState_Requested = ConnectionState' "requested"

pattern ConnectionState_Unknown :: ConnectionState
pattern ConnectionState_Unknown = ConnectionState' "unknown"

{-# COMPLETE
  ConnectionState_Available,
  ConnectionState_Deleted,
  ConnectionState_Deleting,
  ConnectionState_Down,
  ConnectionState_Ordering,
  ConnectionState_Pending,
  ConnectionState_Rejected,
  ConnectionState_Requested,
  ConnectionState_Unknown,
  ConnectionState'
  #-}
