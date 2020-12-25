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
        DeinterlaceAlgorithmInterpolate,
        DeinterlaceAlgorithmInterpolateTicker,
        DeinterlaceAlgorithmBlend,
        DeinterlaceAlgorithmBlendTicker,
        fromDeinterlaceAlgorithm
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.
newtype DeinterlaceAlgorithm = DeinterlaceAlgorithm'
  { fromDeinterlaceAlgorithm ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DeinterlaceAlgorithmInterpolate :: DeinterlaceAlgorithm
pattern DeinterlaceAlgorithmInterpolate = DeinterlaceAlgorithm' "INTERPOLATE"

pattern DeinterlaceAlgorithmInterpolateTicker :: DeinterlaceAlgorithm
pattern DeinterlaceAlgorithmInterpolateTicker = DeinterlaceAlgorithm' "INTERPOLATE_TICKER"

pattern DeinterlaceAlgorithmBlend :: DeinterlaceAlgorithm
pattern DeinterlaceAlgorithmBlend = DeinterlaceAlgorithm' "BLEND"

pattern DeinterlaceAlgorithmBlendTicker :: DeinterlaceAlgorithm
pattern DeinterlaceAlgorithmBlendTicker = DeinterlaceAlgorithm' "BLEND_TICKER"

{-# COMPLETE
  DeinterlaceAlgorithmInterpolate,
  DeinterlaceAlgorithmInterpolateTicker,
  DeinterlaceAlgorithmBlend,
  DeinterlaceAlgorithmBlendTicker,
  DeinterlaceAlgorithm'
  #-}
