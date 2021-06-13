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
-- Module      : Network.AWS.EC2.Types.CapacityReservationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationState
  ( CapacityReservationState
      ( ..,
        CapacityReservationState_Active,
        CapacityReservationState_Cancelled,
        CapacityReservationState_Expired,
        CapacityReservationState_Failed,
        CapacityReservationState_Pending
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype CapacityReservationState = CapacityReservationState'
  { fromCapacityReservationState ::
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

pattern CapacityReservationState_Active :: CapacityReservationState
pattern CapacityReservationState_Active = CapacityReservationState' "active"

pattern CapacityReservationState_Cancelled :: CapacityReservationState
pattern CapacityReservationState_Cancelled = CapacityReservationState' "cancelled"

pattern CapacityReservationState_Expired :: CapacityReservationState
pattern CapacityReservationState_Expired = CapacityReservationState' "expired"

pattern CapacityReservationState_Failed :: CapacityReservationState
pattern CapacityReservationState_Failed = CapacityReservationState' "failed"

pattern CapacityReservationState_Pending :: CapacityReservationState
pattern CapacityReservationState_Pending = CapacityReservationState' "pending"

{-# COMPLETE
  CapacityReservationState_Active,
  CapacityReservationState_Cancelled,
  CapacityReservationState_Expired,
  CapacityReservationState_Failed,
  CapacityReservationState_Pending,
  CapacityReservationState'
  #-}
