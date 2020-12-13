{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
  ( DvbSubtitleFontColor
      ( DvbSubtitleFontColor',
        DSFCWhite,
        DSFCBlack,
        DSFCYellow,
        DSFCRed,
        DSFCGreen,
        DSFCBlue
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleFontColor = DvbSubtitleFontColor' Lude.Text
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

pattern DSFCWhite :: DvbSubtitleFontColor
pattern DSFCWhite = DvbSubtitleFontColor' "WHITE"

pattern DSFCBlack :: DvbSubtitleFontColor
pattern DSFCBlack = DvbSubtitleFontColor' "BLACK"

pattern DSFCYellow :: DvbSubtitleFontColor
pattern DSFCYellow = DvbSubtitleFontColor' "YELLOW"

pattern DSFCRed :: DvbSubtitleFontColor
pattern DSFCRed = DvbSubtitleFontColor' "RED"

pattern DSFCGreen :: DvbSubtitleFontColor
pattern DSFCGreen = DvbSubtitleFontColor' "GREEN"

pattern DSFCBlue :: DvbSubtitleFontColor
pattern DSFCBlue = DvbSubtitleFontColor' "BLUE"

{-# COMPLETE
  DSFCWhite,
  DSFCBlack,
  DSFCYellow,
  DSFCRed,
  DSFCGreen,
  DSFCBlue,
  DvbSubtitleFontColor'
  #-}
