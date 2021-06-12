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
-- Module      : Network.AWS.MediaLive.Types.ReservationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationState
  ( ReservationState
      ( ..,
        ReservationState_ACTIVE,
        ReservationState_CANCELED,
        ReservationState_DELETED,
        ReservationState_EXPIRED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Current reservation state
newtype ReservationState = ReservationState'
  { fromReservationState ::
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

pattern ReservationState_ACTIVE :: ReservationState
pattern ReservationState_ACTIVE = ReservationState' "ACTIVE"

pattern ReservationState_CANCELED :: ReservationState
pattern ReservationState_CANCELED = ReservationState' "CANCELED"

pattern ReservationState_DELETED :: ReservationState
pattern ReservationState_DELETED = ReservationState' "DELETED"

pattern ReservationState_EXPIRED :: ReservationState
pattern ReservationState_EXPIRED = ReservationState' "EXPIRED"

{-# COMPLETE
  ReservationState_ACTIVE,
  ReservationState_CANCELED,
  ReservationState_DELETED,
  ReservationState_EXPIRED,
  ReservationState'
  #-}
