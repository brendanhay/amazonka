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
-- Module      : Amazonka.DirectConnect.Types.LagState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.LagState
  ( LagState
      ( ..,
        LagState_Available,
        LagState_Deleted,
        LagState_Deleting,
        LagState_Down,
        LagState_Pending,
        LagState_Requested,
        LagState_Unknown
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype LagState = LagState'
  { fromLagState ::
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

pattern LagState_Available :: LagState
pattern LagState_Available = LagState' "available"

pattern LagState_Deleted :: LagState
pattern LagState_Deleted = LagState' "deleted"

pattern LagState_Deleting :: LagState
pattern LagState_Deleting = LagState' "deleting"

pattern LagState_Down :: LagState
pattern LagState_Down = LagState' "down"

pattern LagState_Pending :: LagState
pattern LagState_Pending = LagState' "pending"

pattern LagState_Requested :: LagState
pattern LagState_Requested = LagState' "requested"

pattern LagState_Unknown :: LagState
pattern LagState_Unknown = LagState' "unknown"

{-# COMPLETE
  LagState_Available,
  LagState_Deleted,
  LagState_Deleting,
  LagState_Down,
  LagState_Pending,
  LagState_Requested,
  LagState_Unknown,
  LagState'
  #-}
