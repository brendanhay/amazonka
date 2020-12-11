-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationFontColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationFontColor
  ( DvbSubDestinationFontColor
      ( DvbSubDestinationFontColor',
        DSDFCBlack,
        DSDFCBlue,
        DSDFCGreen,
        DSDFCRed,
        DSDFCWhite,
        DSDFCYellow
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Dvb Sub Destination Font Color
newtype DvbSubDestinationFontColor = DvbSubDestinationFontColor' Lude.Text
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

pattern DSDFCBlack :: DvbSubDestinationFontColor
pattern DSDFCBlack = DvbSubDestinationFontColor' "BLACK"

pattern DSDFCBlue :: DvbSubDestinationFontColor
pattern DSDFCBlue = DvbSubDestinationFontColor' "BLUE"

pattern DSDFCGreen :: DvbSubDestinationFontColor
pattern DSDFCGreen = DvbSubDestinationFontColor' "GREEN"

pattern DSDFCRed :: DvbSubDestinationFontColor
pattern DSDFCRed = DvbSubDestinationFontColor' "RED"

pattern DSDFCWhite :: DvbSubDestinationFontColor
pattern DSDFCWhite = DvbSubDestinationFontColor' "WHITE"

pattern DSDFCYellow :: DvbSubDestinationFontColor
pattern DSDFCYellow = DvbSubDestinationFontColor' "YELLOW"

{-# COMPLETE
  DSDFCBlack,
  DSDFCBlue,
  DSDFCGreen,
  DSDFCRed,
  DSDFCWhite,
  DSDFCYellow,
  DvbSubDestinationFontColor'
  #-}
