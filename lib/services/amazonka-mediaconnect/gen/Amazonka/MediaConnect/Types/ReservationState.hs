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
-- Module      : Amazonka.MediaConnect.Types.ReservationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.ReservationState
  ( ReservationState
      ( ..,
        ReservationState_ACTIVE,
        ReservationState_CANCELED,
        ReservationState_EXPIRED,
        ReservationState_PROCESSING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ReservationState = ReservationState'
  { fromReservationState ::
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

pattern ReservationState_ACTIVE :: ReservationState
pattern ReservationState_ACTIVE = ReservationState' "ACTIVE"

pattern ReservationState_CANCELED :: ReservationState
pattern ReservationState_CANCELED = ReservationState' "CANCELED"

pattern ReservationState_EXPIRED :: ReservationState
pattern ReservationState_EXPIRED = ReservationState' "EXPIRED"

pattern ReservationState_PROCESSING :: ReservationState
pattern ReservationState_PROCESSING = ReservationState' "PROCESSING"

{-# COMPLETE
  ReservationState_ACTIVE,
  ReservationState_CANCELED,
  ReservationState_EXPIRED,
  ReservationState_PROCESSING,
  ReservationState'
  #-}
