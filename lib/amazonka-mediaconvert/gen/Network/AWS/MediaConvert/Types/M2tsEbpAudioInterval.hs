{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsEbpAudioInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsEbpAudioInterval
  ( M2tsEbpAudioInterval
      ( M2tsEbpAudioInterval',
        M2tsEbpAudioIntervalVideoAndFixedIntervals,
        M2tsEbpAudioIntervalVideoInterval,
        fromM2tsEbpAudioInterval
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. When set to VIDEO_INTERVAL, these additional markers will not be inserted. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
newtype M2tsEbpAudioInterval = M2tsEbpAudioInterval'
  { fromM2tsEbpAudioInterval ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern M2tsEbpAudioIntervalVideoAndFixedIntervals :: M2tsEbpAudioInterval
pattern M2tsEbpAudioIntervalVideoAndFixedIntervals = M2tsEbpAudioInterval' "VIDEO_AND_FIXED_INTERVALS"

pattern M2tsEbpAudioIntervalVideoInterval :: M2tsEbpAudioInterval
pattern M2tsEbpAudioIntervalVideoInterval = M2tsEbpAudioInterval' "VIDEO_INTERVAL"

{-# COMPLETE
  M2tsEbpAudioIntervalVideoAndFixedIntervals,
  M2tsEbpAudioIntervalVideoInterval,
  M2tsEbpAudioInterval'
  #-}
