-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInFontColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInFontColor
  ( BurnInFontColor
      ( BurnInFontColor',
        BIFCBlack,
        BIFCBlue,
        BIFCGreen,
        BIFCRed,
        BIFCWhite,
        BIFCYellow
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Burn In Font Color
newtype BurnInFontColor = BurnInFontColor' Lude.Text
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

pattern BIFCBlack :: BurnInFontColor
pattern BIFCBlack = BurnInFontColor' "BLACK"

pattern BIFCBlue :: BurnInFontColor
pattern BIFCBlue = BurnInFontColor' "BLUE"

pattern BIFCGreen :: BurnInFontColor
pattern BIFCGreen = BurnInFontColor' "GREEN"

pattern BIFCRed :: BurnInFontColor
pattern BIFCRed = BurnInFontColor' "RED"

pattern BIFCWhite :: BurnInFontColor
pattern BIFCWhite = BurnInFontColor' "WHITE"

pattern BIFCYellow :: BurnInFontColor
pattern BIFCYellow = BurnInFontColor' "YELLOW"

{-# COMPLETE
  BIFCBlack,
  BIFCBlue,
  BIFCGreen,
  BIFCRed,
  BIFCWhite,
  BIFCYellow,
  BurnInFontColor'
  #-}
