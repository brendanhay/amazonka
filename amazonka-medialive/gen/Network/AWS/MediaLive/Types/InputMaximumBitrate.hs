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
-- Module      : Network.AWS.MediaLive.Types.InputMaximumBitrate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputMaximumBitrate
  ( InputMaximumBitrate
      ( ..,
        InputMaximumBitrate_MAX_10_MBPS,
        InputMaximumBitrate_MAX_20_MBPS,
        InputMaximumBitrate_MAX_50_MBPS
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Maximum input bitrate in megabits per second. Bitrates up to 50 Mbps are
-- supported currently.
newtype InputMaximumBitrate = InputMaximumBitrate'
  { fromInputMaximumBitrate ::
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

pattern InputMaximumBitrate_MAX_10_MBPS :: InputMaximumBitrate
pattern InputMaximumBitrate_MAX_10_MBPS = InputMaximumBitrate' "MAX_10_MBPS"

pattern InputMaximumBitrate_MAX_20_MBPS :: InputMaximumBitrate
pattern InputMaximumBitrate_MAX_20_MBPS = InputMaximumBitrate' "MAX_20_MBPS"

pattern InputMaximumBitrate_MAX_50_MBPS :: InputMaximumBitrate
pattern InputMaximumBitrate_MAX_50_MBPS = InputMaximumBitrate' "MAX_50_MBPS"

{-# COMPLETE
  InputMaximumBitrate_MAX_10_MBPS,
  InputMaximumBitrate_MAX_20_MBPS,
  InputMaximumBitrate_MAX_50_MBPS,
  InputMaximumBitrate'
  #-}
