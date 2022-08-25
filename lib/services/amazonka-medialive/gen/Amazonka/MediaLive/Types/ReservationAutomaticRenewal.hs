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
-- Module      : Amazonka.MediaLive.Types.ReservationAutomaticRenewal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ReservationAutomaticRenewal
  ( ReservationAutomaticRenewal
      ( ..,
        ReservationAutomaticRenewal_DISABLED,
        ReservationAutomaticRenewal_ENABLED,
        ReservationAutomaticRenewal_UNAVAILABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Automatic Renewal Status for Reservation
newtype ReservationAutomaticRenewal = ReservationAutomaticRenewal'
  { fromReservationAutomaticRenewal ::
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

pattern ReservationAutomaticRenewal_DISABLED :: ReservationAutomaticRenewal
pattern ReservationAutomaticRenewal_DISABLED = ReservationAutomaticRenewal' "DISABLED"

pattern ReservationAutomaticRenewal_ENABLED :: ReservationAutomaticRenewal
pattern ReservationAutomaticRenewal_ENABLED = ReservationAutomaticRenewal' "ENABLED"

pattern ReservationAutomaticRenewal_UNAVAILABLE :: ReservationAutomaticRenewal
pattern ReservationAutomaticRenewal_UNAVAILABLE = ReservationAutomaticRenewal' "UNAVAILABLE"

{-# COMPLETE
  ReservationAutomaticRenewal_DISABLED,
  ReservationAutomaticRenewal_ENABLED,
  ReservationAutomaticRenewal_UNAVAILABLE,
  ReservationAutomaticRenewal'
  #-}
