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
-- Module      : Amazonka.DirectConnect.Types.InterconnectState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.InterconnectState
  ( InterconnectState
      ( ..,
        InterconnectState_Available,
        InterconnectState_Deleted,
        InterconnectState_Deleting,
        InterconnectState_Down,
        InterconnectState_Pending,
        InterconnectState_Requested,
        InterconnectState_Unknown
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InterconnectState = InterconnectState'
  { fromInterconnectState ::
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

pattern InterconnectState_Available :: InterconnectState
pattern InterconnectState_Available = InterconnectState' "available"

pattern InterconnectState_Deleted :: InterconnectState
pattern InterconnectState_Deleted = InterconnectState' "deleted"

pattern InterconnectState_Deleting :: InterconnectState
pattern InterconnectState_Deleting = InterconnectState' "deleting"

pattern InterconnectState_Down :: InterconnectState
pattern InterconnectState_Down = InterconnectState' "down"

pattern InterconnectState_Pending :: InterconnectState
pattern InterconnectState_Pending = InterconnectState' "pending"

pattern InterconnectState_Requested :: InterconnectState
pattern InterconnectState_Requested = InterconnectState' "requested"

pattern InterconnectState_Unknown :: InterconnectState
pattern InterconnectState_Unknown = InterconnectState' "unknown"

{-# COMPLETE
  InterconnectState_Available,
  InterconnectState_Deleted,
  InterconnectState_Deleting,
  InterconnectState_Down,
  InterconnectState_Pending,
  InterconnectState_Requested,
  InterconnectState_Unknown,
  InterconnectState'
  #-}
