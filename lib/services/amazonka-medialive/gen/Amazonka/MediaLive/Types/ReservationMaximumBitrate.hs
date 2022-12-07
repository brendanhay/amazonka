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
-- Module      : Amazonka.MediaLive.Types.ReservationMaximumBitrate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ReservationMaximumBitrate
  ( ReservationMaximumBitrate
      ( ..,
        ReservationMaximumBitrate_MAX_10_MBPS,
        ReservationMaximumBitrate_MAX_20_MBPS,
        ReservationMaximumBitrate_MAX_50_MBPS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Maximum bitrate in megabits per second
newtype ReservationMaximumBitrate = ReservationMaximumBitrate'
  { fromReservationMaximumBitrate ::
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

pattern ReservationMaximumBitrate_MAX_10_MBPS :: ReservationMaximumBitrate
pattern ReservationMaximumBitrate_MAX_10_MBPS = ReservationMaximumBitrate' "MAX_10_MBPS"

pattern ReservationMaximumBitrate_MAX_20_MBPS :: ReservationMaximumBitrate
pattern ReservationMaximumBitrate_MAX_20_MBPS = ReservationMaximumBitrate' "MAX_20_MBPS"

pattern ReservationMaximumBitrate_MAX_50_MBPS :: ReservationMaximumBitrate
pattern ReservationMaximumBitrate_MAX_50_MBPS = ReservationMaximumBitrate' "MAX_50_MBPS"

{-# COMPLETE
  ReservationMaximumBitrate_MAX_10_MBPS,
  ReservationMaximumBitrate_MAX_20_MBPS,
  ReservationMaximumBitrate_MAX_50_MBPS,
  ReservationMaximumBitrate'
  #-}
