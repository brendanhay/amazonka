{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerFilter
  ( NoiseReducerFilter
      ( NoiseReducerFilter',
        Bilateral,
        Mean,
        Gaussian,
        Lanczos,
        Sharpen,
        Conserve,
        Spatial,
        Temporal
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral preserves edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) do convolution filtering. * Conserve does min/max noise reduction. * Spatial does frequency-domain filtering based on JND principles. * Temporal optimizes video quality for complex motion.
newtype NoiseReducerFilter = NoiseReducerFilter' Lude.Text
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

pattern Bilateral :: NoiseReducerFilter
pattern Bilateral = NoiseReducerFilter' "BILATERAL"

pattern Mean :: NoiseReducerFilter
pattern Mean = NoiseReducerFilter' "MEAN"

pattern Gaussian :: NoiseReducerFilter
pattern Gaussian = NoiseReducerFilter' "GAUSSIAN"

pattern Lanczos :: NoiseReducerFilter
pattern Lanczos = NoiseReducerFilter' "LANCZOS"

pattern Sharpen :: NoiseReducerFilter
pattern Sharpen = NoiseReducerFilter' "SHARPEN"

pattern Conserve :: NoiseReducerFilter
pattern Conserve = NoiseReducerFilter' "CONSERVE"

pattern Spatial :: NoiseReducerFilter
pattern Spatial = NoiseReducerFilter' "SPATIAL"

pattern Temporal :: NoiseReducerFilter
pattern Temporal = NoiseReducerFilter' "TEMPORAL"

{-# COMPLETE
  Bilateral,
  Mean,
  Gaussian,
  Lanczos,
  Sharpen,
  Conserve,
  Spatial,
  Temporal,
  NoiseReducerFilter'
  #-}
