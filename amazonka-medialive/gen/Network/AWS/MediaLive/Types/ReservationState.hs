{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | Current reservation state
newtype ReservationState = ReservationState'
  { fromReservationState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
