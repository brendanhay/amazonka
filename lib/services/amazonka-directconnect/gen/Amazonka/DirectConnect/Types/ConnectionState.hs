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
-- Module      : Amazonka.DirectConnect.Types.ConnectionState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.ConnectionState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConnectionState = ConnectionState'
  { fromConnectionState ::
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
