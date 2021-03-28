{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationOutlineColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.DvbSubDestinationOutlineColor
  ( DvbSubDestinationOutlineColor
    ( DvbSubDestinationOutlineColor'
    , DvbSubDestinationOutlineColorBlack
    , DvbSubDestinationOutlineColorBlue
    , DvbSubDestinationOutlineColorGreen
    , DvbSubDestinationOutlineColorRed
    , DvbSubDestinationOutlineColorWhite
    , DvbSubDestinationOutlineColorYellow
    , fromDvbSubDestinationOutlineColor
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Dvb Sub Destination Outline Color
newtype DvbSubDestinationOutlineColor = DvbSubDestinationOutlineColor'{fromDvbSubDestinationOutlineColor
                                                                       :: Core.Text}
                                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                          Core.Generic)
                                          deriving newtype (Core.IsString, Core.Hashable,
                                                            Core.NFData, Core.ToJSONKey,
                                                            Core.FromJSONKey, Core.ToJSON,
                                                            Core.FromJSON, Core.ToXML, Core.FromXML,
                                                            Core.ToText, Core.FromText,
                                                            Core.ToByteString, Core.ToQuery,
                                                            Core.ToHeader)

pattern DvbSubDestinationOutlineColorBlack :: DvbSubDestinationOutlineColor
pattern DvbSubDestinationOutlineColorBlack = DvbSubDestinationOutlineColor' "BLACK"

pattern DvbSubDestinationOutlineColorBlue :: DvbSubDestinationOutlineColor
pattern DvbSubDestinationOutlineColorBlue = DvbSubDestinationOutlineColor' "BLUE"

pattern DvbSubDestinationOutlineColorGreen :: DvbSubDestinationOutlineColor
pattern DvbSubDestinationOutlineColorGreen = DvbSubDestinationOutlineColor' "GREEN"

pattern DvbSubDestinationOutlineColorRed :: DvbSubDestinationOutlineColor
pattern DvbSubDestinationOutlineColorRed = DvbSubDestinationOutlineColor' "RED"

pattern DvbSubDestinationOutlineColorWhite :: DvbSubDestinationOutlineColor
pattern DvbSubDestinationOutlineColorWhite = DvbSubDestinationOutlineColor' "WHITE"

pattern DvbSubDestinationOutlineColorYellow :: DvbSubDestinationOutlineColor
pattern DvbSubDestinationOutlineColorYellow = DvbSubDestinationOutlineColor' "YELLOW"

{-# COMPLETE 
  DvbSubDestinationOutlineColorBlack,

  DvbSubDestinationOutlineColorBlue,

  DvbSubDestinationOutlineColorGreen,

  DvbSubDestinationOutlineColorRed,

  DvbSubDestinationOutlineColorWhite,

  DvbSubDestinationOutlineColorYellow,
  DvbSubDestinationOutlineColor'
  #-}
