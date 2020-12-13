{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BurninSubtitleFontColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninSubtitleFontColor
  ( BurninSubtitleFontColor
      ( BurninSubtitleFontColor',
        BSFCWhite,
        BSFCBlack,
        BSFCYellow,
        BSFCRed,
        BSFCGreen,
        BSFCBlue
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleFontColor = BurninSubtitleFontColor' Lude.Text
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

pattern BSFCWhite :: BurninSubtitleFontColor
pattern BSFCWhite = BurninSubtitleFontColor' "WHITE"

pattern BSFCBlack :: BurninSubtitleFontColor
pattern BSFCBlack = BurninSubtitleFontColor' "BLACK"

pattern BSFCYellow :: BurninSubtitleFontColor
pattern BSFCYellow = BurninSubtitleFontColor' "YELLOW"

pattern BSFCRed :: BurninSubtitleFontColor
pattern BSFCRed = BurninSubtitleFontColor' "RED"

pattern BSFCGreen :: BurninSubtitleFontColor
pattern BSFCGreen = BurninSubtitleFontColor' "GREEN"

pattern BSFCBlue :: BurninSubtitleFontColor
pattern BSFCBlue = BurninSubtitleFontColor' "BLUE"

{-# COMPLETE
  BSFCWhite,
  BSFCBlack,
  BSFCYellow,
  BSFCRed,
  BSFCGreen,
  BSFCBlue,
  BurninSubtitleFontColor'
  #-}
