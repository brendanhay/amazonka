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
-- Module      : Network.AWS.DirectConnect.Types.LagState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.LagState
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

import qualified Network.AWS.Core as Core

newtype LagState = LagState'
  { fromLagState ::
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
