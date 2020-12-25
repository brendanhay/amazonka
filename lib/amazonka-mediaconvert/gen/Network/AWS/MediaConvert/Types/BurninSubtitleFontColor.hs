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
        BurninSubtitleFontColorWhite,
        BurninSubtitleFontColorBlack,
        BurninSubtitleFontColorYellow,
        BurninSubtitleFontColorRed,
        BurninSubtitleFontColorGreen,
        BurninSubtitleFontColorBlue,
        fromBurninSubtitleFontColor
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleFontColor = BurninSubtitleFontColor'
  { fromBurninSubtitleFontColor ::
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

pattern BurninSubtitleFontColorWhite :: BurninSubtitleFontColor
pattern BurninSubtitleFontColorWhite = BurninSubtitleFontColor' "WHITE"

pattern BurninSubtitleFontColorBlack :: BurninSubtitleFontColor
pattern BurninSubtitleFontColorBlack = BurninSubtitleFontColor' "BLACK"

pattern BurninSubtitleFontColorYellow :: BurninSubtitleFontColor
pattern BurninSubtitleFontColorYellow = BurninSubtitleFontColor' "YELLOW"

pattern BurninSubtitleFontColorRed :: BurninSubtitleFontColor
pattern BurninSubtitleFontColorRed = BurninSubtitleFontColor' "RED"

pattern BurninSubtitleFontColorGreen :: BurninSubtitleFontColor
pattern BurninSubtitleFontColorGreen = BurninSubtitleFontColor' "GREEN"

pattern BurninSubtitleFontColorBlue :: BurninSubtitleFontColor
pattern BurninSubtitleFontColorBlue = BurninSubtitleFontColor' "BLUE"

{-# COMPLETE
  BurninSubtitleFontColorWhite,
  BurninSubtitleFontColorBlack,
  BurninSubtitleFontColorYellow,
  BurninSubtitleFontColorRed,
  BurninSubtitleFontColorGreen,
  BurninSubtitleFontColorBlue,
  BurninSubtitleFontColor'
  #-}
