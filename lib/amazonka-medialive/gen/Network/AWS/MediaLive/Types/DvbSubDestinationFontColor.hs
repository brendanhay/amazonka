{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationFontColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.DvbSubDestinationFontColor
  ( DvbSubDestinationFontColor
    ( DvbSubDestinationFontColor'
    , DvbSubDestinationFontColorBlack
    , DvbSubDestinationFontColorBlue
    , DvbSubDestinationFontColorGreen
    , DvbSubDestinationFontColorRed
    , DvbSubDestinationFontColorWhite
    , DvbSubDestinationFontColorYellow
    , fromDvbSubDestinationFontColor
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Dvb Sub Destination Font Color
newtype DvbSubDestinationFontColor = DvbSubDestinationFontColor'{fromDvbSubDestinationFontColor
                                                                 :: Core.Text}
                                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                       Core.Generic)
                                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                         Core.ToJSONKey, Core.FromJSONKey,
                                                         Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                         Core.FromXML, Core.ToText, Core.FromText,
                                                         Core.ToByteString, Core.ToQuery,
                                                         Core.ToHeader)

pattern DvbSubDestinationFontColorBlack :: DvbSubDestinationFontColor
pattern DvbSubDestinationFontColorBlack = DvbSubDestinationFontColor' "BLACK"

pattern DvbSubDestinationFontColorBlue :: DvbSubDestinationFontColor
pattern DvbSubDestinationFontColorBlue = DvbSubDestinationFontColor' "BLUE"

pattern DvbSubDestinationFontColorGreen :: DvbSubDestinationFontColor
pattern DvbSubDestinationFontColorGreen = DvbSubDestinationFontColor' "GREEN"

pattern DvbSubDestinationFontColorRed :: DvbSubDestinationFontColor
pattern DvbSubDestinationFontColorRed = DvbSubDestinationFontColor' "RED"

pattern DvbSubDestinationFontColorWhite :: DvbSubDestinationFontColor
pattern DvbSubDestinationFontColorWhite = DvbSubDestinationFontColor' "WHITE"

pattern DvbSubDestinationFontColorYellow :: DvbSubDestinationFontColor
pattern DvbSubDestinationFontColorYellow = DvbSubDestinationFontColor' "YELLOW"

{-# COMPLETE 
  DvbSubDestinationFontColorBlack,

  DvbSubDestinationFontColorBlue,

  DvbSubDestinationFontColorGreen,

  DvbSubDestinationFontColorRed,

  DvbSubDestinationFontColorWhite,

  DvbSubDestinationFontColorYellow,
  DvbSubDestinationFontColor'
  #-}
