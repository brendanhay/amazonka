-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265AdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265AdaptiveQuantization
  ( H265AdaptiveQuantization
      ( H265AdaptiveQuantization',
        HAQHigh,
        HAQHigher,
        HAQLow,
        HAQMax,
        HAQMedium,
        HAQOff
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H265 Adaptive Quantization
newtype H265AdaptiveQuantization = H265AdaptiveQuantization' Lude.Text
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

pattern HAQHigh :: H265AdaptiveQuantization
pattern HAQHigh = H265AdaptiveQuantization' "HIGH"

pattern HAQHigher :: H265AdaptiveQuantization
pattern HAQHigher = H265AdaptiveQuantization' "HIGHER"

pattern HAQLow :: H265AdaptiveQuantization
pattern HAQLow = H265AdaptiveQuantization' "LOW"

pattern HAQMax :: H265AdaptiveQuantization
pattern HAQMax = H265AdaptiveQuantization' "MAX"

pattern HAQMedium :: H265AdaptiveQuantization
pattern HAQMedium = H265AdaptiveQuantization' "MEDIUM"

pattern HAQOff :: H265AdaptiveQuantization
pattern HAQOff = H265AdaptiveQuantization' "OFF"

{-# COMPLETE
  HAQHigh,
  HAQHigher,
  HAQLow,
  HAQMax,
  HAQMedium,
  HAQOff,
  H265AdaptiveQuantization'
  #-}
