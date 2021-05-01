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

import qualified Network.AWS.Prelude as Prelude

-- | Maximum bitrate in megabits per second
newtype ReservationMaximumBitrate = ReservationMaximumBitrate'
  { fromReservationMaximumBitrate ::
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
