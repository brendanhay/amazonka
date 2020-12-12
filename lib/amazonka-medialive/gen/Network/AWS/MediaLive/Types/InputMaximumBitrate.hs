{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputMaximumBitrate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputMaximumBitrate
  ( InputMaximumBitrate
      ( InputMaximumBitrate',
        Max10Mbps,
        Max20Mbps,
        Max50Mbps
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Maximum input bitrate in megabits per second. Bitrates up to 50 Mbps are supported currently.
newtype InputMaximumBitrate = InputMaximumBitrate' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Max10Mbps :: InputMaximumBitrate
pattern Max10Mbps = InputMaximumBitrate' "MAX_10_MBPS"

pattern Max20Mbps :: InputMaximumBitrate
pattern Max20Mbps = InputMaximumBitrate' "MAX_20_MBPS"

pattern Max50Mbps :: InputMaximumBitrate
pattern Max50Mbps = InputMaximumBitrate' "MAX_50_MBPS"

{-# COMPLETE
  Max10Mbps,
  Max20Mbps,
  Max50Mbps,
  InputMaximumBitrate'
  #-}
