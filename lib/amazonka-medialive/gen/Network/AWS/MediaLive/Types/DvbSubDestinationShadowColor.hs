{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationShadowColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.DvbSubDestinationShadowColor
  ( DvbSubDestinationShadowColor
    ( DvbSubDestinationShadowColor'
    , DvbSubDestinationShadowColorBlack
    , DvbSubDestinationShadowColorNone
    , DvbSubDestinationShadowColorWhite
    , fromDvbSubDestinationShadowColor
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Dvb Sub Destination Shadow Color
newtype DvbSubDestinationShadowColor = DvbSubDestinationShadowColor'{fromDvbSubDestinationShadowColor
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

pattern DvbSubDestinationShadowColorBlack :: DvbSubDestinationShadowColor
pattern DvbSubDestinationShadowColorBlack = DvbSubDestinationShadowColor' "BLACK"

pattern DvbSubDestinationShadowColorNone :: DvbSubDestinationShadowColor
pattern DvbSubDestinationShadowColorNone = DvbSubDestinationShadowColor' "NONE"

pattern DvbSubDestinationShadowColorWhite :: DvbSubDestinationShadowColor
pattern DvbSubDestinationShadowColorWhite = DvbSubDestinationShadowColor' "WHITE"

{-# COMPLETE 
  DvbSubDestinationShadowColorBlack,

  DvbSubDestinationShadowColorNone,

  DvbSubDestinationShadowColorWhite,
  DvbSubDestinationShadowColor'
  #-}
