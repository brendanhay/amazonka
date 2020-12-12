{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupSegmentationMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupSegmentationMode
  ( SmoothGroupSegmentationMode
      ( SmoothGroupSegmentationMode',
        UseInputSegmentation,
        UseSegmentDuration
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Smooth Group Segmentation Mode
newtype SmoothGroupSegmentationMode = SmoothGroupSegmentationMode' Lude.Text
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

pattern UseInputSegmentation :: SmoothGroupSegmentationMode
pattern UseInputSegmentation = SmoothGroupSegmentationMode' "USE_INPUT_SEGMENTATION"

pattern UseSegmentDuration :: SmoothGroupSegmentationMode
pattern UseSegmentDuration = SmoothGroupSegmentationMode' "USE_SEGMENT_DURATION"

{-# COMPLETE
  UseInputSegmentation,
  UseSegmentDuration,
  SmoothGroupSegmentationMode'
  #-}
