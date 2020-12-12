{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DeinterlaceAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DeinterlaceAlgorithm
  ( DeinterlaceAlgorithm
      ( DeinterlaceAlgorithm',
        DABlend,
        DABlendTicker,
        DAInterpolate,
        DAInterpolateTicker
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.
newtype DeinterlaceAlgorithm = DeinterlaceAlgorithm' Lude.Text
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

pattern DABlend :: DeinterlaceAlgorithm
pattern DABlend = DeinterlaceAlgorithm' "BLEND"

pattern DABlendTicker :: DeinterlaceAlgorithm
pattern DABlendTicker = DeinterlaceAlgorithm' "BLEND_TICKER"

pattern DAInterpolate :: DeinterlaceAlgorithm
pattern DAInterpolate = DeinterlaceAlgorithm' "INTERPOLATE"

pattern DAInterpolateTicker :: DeinterlaceAlgorithm
pattern DAInterpolateTicker = DeinterlaceAlgorithm' "INTERPOLATE_TICKER"

{-# COMPLETE
  DABlend,
  DABlendTicker,
  DAInterpolate,
  DAInterpolateTicker,
  DeinterlaceAlgorithm'
  #-}
