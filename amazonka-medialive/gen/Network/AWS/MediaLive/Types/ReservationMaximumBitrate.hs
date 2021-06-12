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
-- Module      : Network.AWS.MediaLive.Types.ReservationMaximumBitrate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationMaximumBitrate
  ( ReservationMaximumBitrate
      ( ..,
        ReservationMaximumBitrate_MAX_10_MBPS,
        ReservationMaximumBitrate_MAX_20_MBPS,
        ReservationMaximumBitrate_MAX_50_MBPS
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Maximum bitrate in megabits per second
newtype ReservationMaximumBitrate = ReservationMaximumBitrate'
  { fromReservationMaximumBitrate ::
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
