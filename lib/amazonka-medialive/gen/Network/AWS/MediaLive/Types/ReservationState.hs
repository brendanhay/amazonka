{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationState
  ( ReservationState
      ( ReservationState',
        ReservationStateActive,
        ReservationStateExpired,
        ReservationStateCanceled,
        ReservationStateDeleted,
        fromReservationState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Current reservation state
newtype ReservationState = ReservationState'
  { fromReservationState ::
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

pattern ReservationStateActive :: ReservationState
pattern ReservationStateActive = ReservationState' "ACTIVE"

pattern ReservationStateExpired :: ReservationState
pattern ReservationStateExpired = ReservationState' "EXPIRED"

pattern ReservationStateCanceled :: ReservationState
pattern ReservationStateCanceled = ReservationState' "CANCELED"

pattern ReservationStateDeleted :: ReservationState
pattern ReservationStateDeleted = ReservationState' "DELETED"

{-# COMPLETE
  ReservationStateActive,
  ReservationStateExpired,
  ReservationStateCanceled,
  ReservationStateDeleted,
  ReservationState'
  #-}
