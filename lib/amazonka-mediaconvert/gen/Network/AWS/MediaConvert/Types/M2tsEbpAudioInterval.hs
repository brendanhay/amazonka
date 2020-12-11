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
        VideoAndFixedIntervals,
        VideoInterval
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. When set to VIDEO_INTERVAL, these additional markers will not be inserted. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
newtype M2tsEbpAudioInterval = M2tsEbpAudioInterval' Lude.Text
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

pattern VideoAndFixedIntervals :: M2tsEbpAudioInterval
pattern VideoAndFixedIntervals = M2tsEbpAudioInterval' "VIDEO_AND_FIXED_INTERVALS"

pattern VideoInterval :: M2tsEbpAudioInterval
pattern VideoInterval = M2tsEbpAudioInterval' "VIDEO_INTERVAL"

{-# COMPLETE
  VideoAndFixedIntervals,
  VideoInterval,
  M2tsEbpAudioInterval'
  #-}
