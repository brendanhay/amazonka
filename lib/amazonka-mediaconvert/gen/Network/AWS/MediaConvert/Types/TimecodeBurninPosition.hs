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
        TopCenter,
        TopLeft,
        TopRight,
        MiddleLeft,
        MiddleCenter,
        MiddleRight,
        BottomLeft,
        BottomCenter,
        BottomRight
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.
newtype TimecodeBurninPosition = TimecodeBurninPosition' Lude.Text
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

pattern TopCenter :: TimecodeBurninPosition
pattern TopCenter = TimecodeBurninPosition' "TOP_CENTER"

pattern TopLeft :: TimecodeBurninPosition
pattern TopLeft = TimecodeBurninPosition' "TOP_LEFT"

pattern TopRight :: TimecodeBurninPosition
pattern TopRight = TimecodeBurninPosition' "TOP_RIGHT"

pattern MiddleLeft :: TimecodeBurninPosition
pattern MiddleLeft = TimecodeBurninPosition' "MIDDLE_LEFT"

pattern MiddleCenter :: TimecodeBurninPosition
pattern MiddleCenter = TimecodeBurninPosition' "MIDDLE_CENTER"

pattern MiddleRight :: TimecodeBurninPosition
pattern MiddleRight = TimecodeBurninPosition' "MIDDLE_RIGHT"

pattern BottomLeft :: TimecodeBurninPosition
pattern BottomLeft = TimecodeBurninPosition' "BOTTOM_LEFT"

pattern BottomCenter :: TimecodeBurninPosition
pattern BottomCenter = TimecodeBurninPosition' "BOTTOM_CENTER"

pattern BottomRight :: TimecodeBurninPosition
pattern BottomRight = TimecodeBurninPosition' "BOTTOM_RIGHT"

{-# COMPLETE
  TopCenter,
  TopLeft,
  TopRight,
  MiddleLeft,
  MiddleCenter,
  MiddleRight,
  BottomLeft,
  BottomCenter,
  BottomRight,
  TimecodeBurninPosition'
  #-}
