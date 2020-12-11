-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BurninSubtitleBackgroundColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninSubtitleBackgroundColor
  ( BurninSubtitleBackgroundColor
      ( BurninSubtitleBackgroundColor',
        BSBCBlack,
        BSBCNone,
        BSBCWhite
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specifies the color of the rectangle behind the captions.
--
-- All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleBackgroundColor = BurninSubtitleBackgroundColor' Lude.Text
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

pattern BSBCBlack :: BurninSubtitleBackgroundColor
pattern BSBCBlack = BurninSubtitleBackgroundColor' "BLACK"

pattern BSBCNone :: BurninSubtitleBackgroundColor
pattern BSBCNone = BurninSubtitleBackgroundColor' "NONE"

pattern BSBCWhite :: BurninSubtitleBackgroundColor
pattern BSBCWhite = BurninSubtitleBackgroundColor' "WHITE"

{-# COMPLETE
  BSBCBlack,
  BSBCNone,
  BSBCWhite,
  BurninSubtitleBackgroundColor'
  #-}
