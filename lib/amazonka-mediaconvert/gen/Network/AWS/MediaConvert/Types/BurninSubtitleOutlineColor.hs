{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BurninSubtitleOutlineColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninSubtitleOutlineColor
  ( BurninSubtitleOutlineColor
      ( BurninSubtitleOutlineColor',
        BurninSubtitleOutlineColorBlack,
        BurninSubtitleOutlineColorWhite,
        BurninSubtitleOutlineColorYellow,
        BurninSubtitleOutlineColorRed,
        BurninSubtitleOutlineColorGreen,
        BurninSubtitleOutlineColorBlue,
        fromBurninSubtitleOutlineColor
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleOutlineColor = BurninSubtitleOutlineColor'
  { fromBurninSubtitleOutlineColor ::
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

pattern BurninSubtitleOutlineColorBlack :: BurninSubtitleOutlineColor
pattern BurninSubtitleOutlineColorBlack = BurninSubtitleOutlineColor' "BLACK"

pattern BurninSubtitleOutlineColorWhite :: BurninSubtitleOutlineColor
pattern BurninSubtitleOutlineColorWhite = BurninSubtitleOutlineColor' "WHITE"

pattern BurninSubtitleOutlineColorYellow :: BurninSubtitleOutlineColor
pattern BurninSubtitleOutlineColorYellow = BurninSubtitleOutlineColor' "YELLOW"

pattern BurninSubtitleOutlineColorRed :: BurninSubtitleOutlineColor
pattern BurninSubtitleOutlineColorRed = BurninSubtitleOutlineColor' "RED"

pattern BurninSubtitleOutlineColorGreen :: BurninSubtitleOutlineColor
pattern BurninSubtitleOutlineColorGreen = BurninSubtitleOutlineColor' "GREEN"

pattern BurninSubtitleOutlineColorBlue :: BurninSubtitleOutlineColor
pattern BurninSubtitleOutlineColorBlue = BurninSubtitleOutlineColor' "BLUE"

{-# COMPLETE
  BurninSubtitleOutlineColorBlack,
  BurninSubtitleOutlineColorWhite,
  BurninSubtitleOutlineColorYellow,
  BurninSubtitleOutlineColorRed,
  BurninSubtitleOutlineColorGreen,
  BurninSubtitleOutlineColorBlue,
  BurninSubtitleOutlineColor'
  #-}
