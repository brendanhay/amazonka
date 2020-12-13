{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleOutlineColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleOutlineColor
  ( DvbSubtitleOutlineColor
      ( DvbSubtitleOutlineColor',
        Black,
        White,
        Yellow,
        Red,
        Green,
        Blue
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleOutlineColor = DvbSubtitleOutlineColor' Lude.Text
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

pattern Black :: DvbSubtitleOutlineColor
pattern Black = DvbSubtitleOutlineColor' "BLACK"

pattern White :: DvbSubtitleOutlineColor
pattern White = DvbSubtitleOutlineColor' "WHITE"

pattern Yellow :: DvbSubtitleOutlineColor
pattern Yellow = DvbSubtitleOutlineColor' "YELLOW"

pattern Red :: DvbSubtitleOutlineColor
pattern Red = DvbSubtitleOutlineColor' "RED"

pattern Green :: DvbSubtitleOutlineColor
pattern Green = DvbSubtitleOutlineColor' "GREEN"

pattern Blue :: DvbSubtitleOutlineColor
pattern Blue = DvbSubtitleOutlineColor' "BLUE"

{-# COMPLETE
  Black,
  White,
  Yellow,
  Red,
  Green,
  Blue,
  DvbSubtitleOutlineColor'
  #-}
