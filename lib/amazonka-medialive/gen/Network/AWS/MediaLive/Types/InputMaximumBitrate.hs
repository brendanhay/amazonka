{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputMaximumBitrate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputMaximumBitrate
  ( InputMaximumBitrate
    ( InputMaximumBitrate'
    , InputMaximumBitrateMax10Mbps
    , InputMaximumBitrateMax20Mbps
    , InputMaximumBitrateMax50Mbps
    , fromInputMaximumBitrate
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Maximum input bitrate in megabits per second. Bitrates up to 50 Mbps are supported currently.
newtype InputMaximumBitrate = InputMaximumBitrate'{fromInputMaximumBitrate
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern InputMaximumBitrateMax10Mbps :: InputMaximumBitrate
pattern InputMaximumBitrateMax10Mbps = InputMaximumBitrate' "MAX_10_MBPS"

pattern InputMaximumBitrateMax20Mbps :: InputMaximumBitrate
pattern InputMaximumBitrateMax20Mbps = InputMaximumBitrate' "MAX_20_MBPS"

pattern InputMaximumBitrateMax50Mbps :: InputMaximumBitrate
pattern InputMaximumBitrateMax50Mbps = InputMaximumBitrate' "MAX_50_MBPS"

{-# COMPLETE 
  InputMaximumBitrateMax10Mbps,

  InputMaximumBitrateMax20Mbps,

  InputMaximumBitrateMax50Mbps,
  InputMaximumBitrate'
  #-}
