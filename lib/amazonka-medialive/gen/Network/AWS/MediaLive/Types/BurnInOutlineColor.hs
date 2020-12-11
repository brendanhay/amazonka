-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInOutlineColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInOutlineColor
  ( BurnInOutlineColor
      ( BurnInOutlineColor',
        BIOCBlack,
        BIOCBlue,
        BIOCGreen,
        BIOCRed,
        BIOCWhite,
        BIOCYellow
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Burn In Outline Color
newtype BurnInOutlineColor = BurnInOutlineColor' Lude.Text
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

pattern BIOCBlack :: BurnInOutlineColor
pattern BIOCBlack = BurnInOutlineColor' "BLACK"

pattern BIOCBlue :: BurnInOutlineColor
pattern BIOCBlue = BurnInOutlineColor' "BLUE"

pattern BIOCGreen :: BurnInOutlineColor
pattern BIOCGreen = BurnInOutlineColor' "GREEN"

pattern BIOCRed :: BurnInOutlineColor
pattern BIOCRed = BurnInOutlineColor' "RED"

pattern BIOCWhite :: BurnInOutlineColor
pattern BIOCWhite = BurnInOutlineColor' "WHITE"

pattern BIOCYellow :: BurnInOutlineColor
pattern BIOCYellow = BurnInOutlineColor' "YELLOW"

{-# COMPLETE
  BIOCBlack,
  BIOCBlue,
  BIOCGreen,
  BIOCRed,
  BIOCWhite,
  BIOCYellow,
  BurnInOutlineColor'
  #-}
