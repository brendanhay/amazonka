{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationState
  ( CapacityReservationState
      ( CapacityReservationState',
        CapacityReservationStateActive,
        CapacityReservationStateExpired,
        CapacityReservationStateCancelled,
        CapacityReservationStatePending,
        CapacityReservationStateFailed,
        fromCapacityReservationState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CapacityReservationState = CapacityReservationState'
  { fromCapacityReservationState ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern CapacityReservationStateActive :: CapacityReservationState
pattern CapacityReservationStateActive = CapacityReservationState' "active"

pattern CapacityReservationStateExpired :: CapacityReservationState
pattern CapacityReservationStateExpired = CapacityReservationState' "expired"

pattern CapacityReservationStateCancelled :: CapacityReservationState
pattern CapacityReservationStateCancelled = CapacityReservationState' "cancelled"

pattern CapacityReservationStatePending :: CapacityReservationState
pattern CapacityReservationStatePending = CapacityReservationState' "pending"

pattern CapacityReservationStateFailed :: CapacityReservationState
pattern CapacityReservationStateFailed = CapacityReservationState' "failed"

{-# COMPLETE
  CapacityReservationStateActive,
  CapacityReservationStateExpired,
  CapacityReservationStateCancelled,
  CapacityReservationStatePending,
  CapacityReservationStateFailed,
  CapacityReservationState'
  #-}
