{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264AdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264AdaptiveQuantization
  ( H264AdaptiveQuantization
      ( H264AdaptiveQuantization',
        HHigh,
        HHigher,
        HLow,
        HMax,
        HMedium,
        HOff
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H264 Adaptive Quantization
newtype H264AdaptiveQuantization = H264AdaptiveQuantization' Lude.Text
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

pattern HHigh :: H264AdaptiveQuantization
pattern HHigh = H264AdaptiveQuantization' "HIGH"

pattern HHigher :: H264AdaptiveQuantization
pattern HHigher = H264AdaptiveQuantization' "HIGHER"

pattern HLow :: H264AdaptiveQuantization
pattern HLow = H264AdaptiveQuantization' "LOW"

pattern HMax :: H264AdaptiveQuantization
pattern HMax = H264AdaptiveQuantization' "MAX"

pattern HMedium :: H264AdaptiveQuantization
pattern HMedium = H264AdaptiveQuantization' "MEDIUM"

pattern HOff :: H264AdaptiveQuantization
pattern HOff = H264AdaptiveQuantization' "OFF"

{-# COMPLETE
  HHigh,
  HHigher,
  HLow,
  HMax,
  HMedium,
  HOff,
  H264AdaptiveQuantization'
  #-}
