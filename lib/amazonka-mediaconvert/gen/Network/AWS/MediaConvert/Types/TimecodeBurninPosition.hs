{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimecodeBurninPosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimecodeBurninPosition
  ( TimecodeBurninPosition
      ( TimecodeBurninPosition',
        TimecodeBurninPositionTopCenter,
        TimecodeBurninPositionTopLeft,
        TimecodeBurninPositionTopRight,
        TimecodeBurninPositionMiddleLeft,
        TimecodeBurninPositionMiddleCenter,
        TimecodeBurninPositionMiddleRight,
        TimecodeBurninPositionBottomLeft,
        TimecodeBurninPositionBottomCenter,
        TimecodeBurninPositionBottomRight,
        fromTimecodeBurninPosition
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.
newtype TimecodeBurninPosition = TimecodeBurninPosition'
  { fromTimecodeBurninPosition ::
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

pattern TimecodeBurninPositionTopCenter :: TimecodeBurninPosition
pattern TimecodeBurninPositionTopCenter = TimecodeBurninPosition' "TOP_CENTER"

pattern TimecodeBurninPositionTopLeft :: TimecodeBurninPosition
pattern TimecodeBurninPositionTopLeft = TimecodeBurninPosition' "TOP_LEFT"

pattern TimecodeBurninPositionTopRight :: TimecodeBurninPosition
pattern TimecodeBurninPositionTopRight = TimecodeBurninPosition' "TOP_RIGHT"

pattern TimecodeBurninPositionMiddleLeft :: TimecodeBurninPosition
pattern TimecodeBurninPositionMiddleLeft = TimecodeBurninPosition' "MIDDLE_LEFT"

pattern TimecodeBurninPositionMiddleCenter :: TimecodeBurninPosition
pattern TimecodeBurninPositionMiddleCenter = TimecodeBurninPosition' "MIDDLE_CENTER"

pattern TimecodeBurninPositionMiddleRight :: TimecodeBurninPosition
pattern TimecodeBurninPositionMiddleRight = TimecodeBurninPosition' "MIDDLE_RIGHT"

pattern TimecodeBurninPositionBottomLeft :: TimecodeBurninPosition
pattern TimecodeBurninPositionBottomLeft = TimecodeBurninPosition' "BOTTOM_LEFT"

pattern TimecodeBurninPositionBottomCenter :: TimecodeBurninPosition
pattern TimecodeBurninPositionBottomCenter = TimecodeBurninPosition' "BOTTOM_CENTER"

pattern TimecodeBurninPositionBottomRight :: TimecodeBurninPosition
pattern TimecodeBurninPositionBottomRight = TimecodeBurninPosition' "BOTTOM_RIGHT"

{-# COMPLETE
  TimecodeBurninPositionTopCenter,
  TimecodeBurninPositionTopLeft,
  TimecodeBurninPositionTopRight,
  TimecodeBurninPositionMiddleLeft,
  TimecodeBurninPositionMiddleCenter,
  TimecodeBurninPositionMiddleRight,
  TimecodeBurninPositionBottomLeft,
  TimecodeBurninPositionBottomCenter,
  TimecodeBurninPositionBottomRight,
  TimecodeBurninPosition'
  #-}
