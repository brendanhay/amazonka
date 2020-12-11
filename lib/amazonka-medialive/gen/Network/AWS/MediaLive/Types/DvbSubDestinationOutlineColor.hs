-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationOutlineColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationOutlineColor
  ( DvbSubDestinationOutlineColor
      ( DvbSubDestinationOutlineColor',
        Black,
        Blue,
        Green,
        Red,
        White,
        Yellow
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Dvb Sub Destination Outline Color
newtype DvbSubDestinationOutlineColor = DvbSubDestinationOutlineColor' Lude.Text
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

pattern Black :: DvbSubDestinationOutlineColor
pattern Black = DvbSubDestinationOutlineColor' "BLACK"

pattern Blue :: DvbSubDestinationOutlineColor
pattern Blue = DvbSubDestinationOutlineColor' "BLUE"

pattern Green :: DvbSubDestinationOutlineColor
pattern Green = DvbSubDestinationOutlineColor' "GREEN"

pattern Red :: DvbSubDestinationOutlineColor
pattern Red = DvbSubDestinationOutlineColor' "RED"

pattern White :: DvbSubDestinationOutlineColor
pattern White = DvbSubDestinationOutlineColor' "WHITE"

pattern Yellow :: DvbSubDestinationOutlineColor
pattern Yellow = DvbSubDestinationOutlineColor' "YELLOW"

{-# COMPLETE
  Black,
  Blue,
  Green,
  Red,
  White,
  Yellow,
  DvbSubDestinationOutlineColor'
  #-}
