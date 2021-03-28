{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
  ( DvbSubtitleFontColor
    ( DvbSubtitleFontColor'
    , DvbSubtitleFontColorWhite
    , DvbSubtitleFontColorBlack
    , DvbSubtitleFontColorYellow
    , DvbSubtitleFontColorRed
    , DvbSubtitleFontColorGreen
    , DvbSubtitleFontColorBlue
    , fromDvbSubtitleFontColor
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleFontColor = DvbSubtitleFontColor'{fromDvbSubtitleFontColor
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern DvbSubtitleFontColorWhite :: DvbSubtitleFontColor
pattern DvbSubtitleFontColorWhite = DvbSubtitleFontColor' "WHITE"

pattern DvbSubtitleFontColorBlack :: DvbSubtitleFontColor
pattern DvbSubtitleFontColorBlack = DvbSubtitleFontColor' "BLACK"

pattern DvbSubtitleFontColorYellow :: DvbSubtitleFontColor
pattern DvbSubtitleFontColorYellow = DvbSubtitleFontColor' "YELLOW"

pattern DvbSubtitleFontColorRed :: DvbSubtitleFontColor
pattern DvbSubtitleFontColorRed = DvbSubtitleFontColor' "RED"

pattern DvbSubtitleFontColorGreen :: DvbSubtitleFontColor
pattern DvbSubtitleFontColorGreen = DvbSubtitleFontColor' "GREEN"

pattern DvbSubtitleFontColorBlue :: DvbSubtitleFontColor
pattern DvbSubtitleFontColorBlue = DvbSubtitleFontColor' "BLUE"

{-# COMPLETE 
  DvbSubtitleFontColorWhite,

  DvbSubtitleFontColorBlack,

  DvbSubtitleFontColorYellow,

  DvbSubtitleFontColorRed,

  DvbSubtitleFontColorGreen,

  DvbSubtitleFontColorBlue,
  DvbSubtitleFontColor'
  #-}
