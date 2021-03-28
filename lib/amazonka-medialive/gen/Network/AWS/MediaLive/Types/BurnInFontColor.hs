{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInFontColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.BurnInFontColor
  ( BurnInFontColor
    ( BurnInFontColor'
    , BurnInFontColorBlack
    , BurnInFontColorBlue
    , BurnInFontColorGreen
    , BurnInFontColorRed
    , BurnInFontColorWhite
    , BurnInFontColorYellow
    , fromBurnInFontColor
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Burn In Font Color
newtype BurnInFontColor = BurnInFontColor'{fromBurnInFontColor ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern BurnInFontColorBlack :: BurnInFontColor
pattern BurnInFontColorBlack = BurnInFontColor' "BLACK"

pattern BurnInFontColorBlue :: BurnInFontColor
pattern BurnInFontColorBlue = BurnInFontColor' "BLUE"

pattern BurnInFontColorGreen :: BurnInFontColor
pattern BurnInFontColorGreen = BurnInFontColor' "GREEN"

pattern BurnInFontColorRed :: BurnInFontColor
pattern BurnInFontColorRed = BurnInFontColor' "RED"

pattern BurnInFontColorWhite :: BurnInFontColor
pattern BurnInFontColorWhite = BurnInFontColor' "WHITE"

pattern BurnInFontColorYellow :: BurnInFontColor
pattern BurnInFontColorYellow = BurnInFontColor' "YELLOW"

{-# COMPLETE 
  BurnInFontColorBlack,

  BurnInFontColorBlue,

  BurnInFontColorGreen,

  BurnInFontColorRed,

  BurnInFontColorWhite,

  BurnInFontColorYellow,
  BurnInFontColor'
  #-}
