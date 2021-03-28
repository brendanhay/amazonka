{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInOutlineColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.BurnInOutlineColor
  ( BurnInOutlineColor
    ( BurnInOutlineColor'
    , BurnInOutlineColorBlack
    , BurnInOutlineColorBlue
    , BurnInOutlineColorGreen
    , BurnInOutlineColorRed
    , BurnInOutlineColorWhite
    , BurnInOutlineColorYellow
    , fromBurnInOutlineColor
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Burn In Outline Color
newtype BurnInOutlineColor = BurnInOutlineColor'{fromBurnInOutlineColor
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern BurnInOutlineColorBlack :: BurnInOutlineColor
pattern BurnInOutlineColorBlack = BurnInOutlineColor' "BLACK"

pattern BurnInOutlineColorBlue :: BurnInOutlineColor
pattern BurnInOutlineColorBlue = BurnInOutlineColor' "BLUE"

pattern BurnInOutlineColorGreen :: BurnInOutlineColor
pattern BurnInOutlineColorGreen = BurnInOutlineColor' "GREEN"

pattern BurnInOutlineColorRed :: BurnInOutlineColor
pattern BurnInOutlineColorRed = BurnInOutlineColor' "RED"

pattern BurnInOutlineColorWhite :: BurnInOutlineColor
pattern BurnInOutlineColorWhite = BurnInOutlineColor' "WHITE"

pattern BurnInOutlineColorYellow :: BurnInOutlineColor
pattern BurnInOutlineColorYellow = BurnInOutlineColor' "YELLOW"

{-# COMPLETE 
  BurnInOutlineColorBlack,

  BurnInOutlineColorBlue,

  BurnInOutlineColorGreen,

  BurnInOutlineColorRed,

  BurnInOutlineColorWhite,

  BurnInOutlineColorYellow,
  BurnInOutlineColor'
  #-}
