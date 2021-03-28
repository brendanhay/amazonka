{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.NoiseReducerFilter
  ( NoiseReducerFilter
    ( NoiseReducerFilter'
    , NoiseReducerFilterBilateral
    , NoiseReducerFilterMean
    , NoiseReducerFilterGaussian
    , NoiseReducerFilterLanczos
    , NoiseReducerFilterSharpen
    , NoiseReducerFilterConserve
    , NoiseReducerFilterSpatial
    , NoiseReducerFilterTemporal
    , fromNoiseReducerFilter
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral preserves edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) do convolution filtering. * Conserve does min/max noise reduction. * Spatial does frequency-domain filtering based on JND principles. * Temporal optimizes video quality for complex motion.
newtype NoiseReducerFilter = NoiseReducerFilter'{fromNoiseReducerFilter
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern NoiseReducerFilterBilateral :: NoiseReducerFilter
pattern NoiseReducerFilterBilateral = NoiseReducerFilter' "BILATERAL"

pattern NoiseReducerFilterMean :: NoiseReducerFilter
pattern NoiseReducerFilterMean = NoiseReducerFilter' "MEAN"

pattern NoiseReducerFilterGaussian :: NoiseReducerFilter
pattern NoiseReducerFilterGaussian = NoiseReducerFilter' "GAUSSIAN"

pattern NoiseReducerFilterLanczos :: NoiseReducerFilter
pattern NoiseReducerFilterLanczos = NoiseReducerFilter' "LANCZOS"

pattern NoiseReducerFilterSharpen :: NoiseReducerFilter
pattern NoiseReducerFilterSharpen = NoiseReducerFilter' "SHARPEN"

pattern NoiseReducerFilterConserve :: NoiseReducerFilter
pattern NoiseReducerFilterConserve = NoiseReducerFilter' "CONSERVE"

pattern NoiseReducerFilterSpatial :: NoiseReducerFilter
pattern NoiseReducerFilterSpatial = NoiseReducerFilter' "SPATIAL"

pattern NoiseReducerFilterTemporal :: NoiseReducerFilter
pattern NoiseReducerFilterTemporal = NoiseReducerFilter' "TEMPORAL"

{-# COMPLETE 
  NoiseReducerFilterBilateral,

  NoiseReducerFilterMean,

  NoiseReducerFilterGaussian,

  NoiseReducerFilterLanczos,

  NoiseReducerFilterSharpen,

  NoiseReducerFilterConserve,

  NoiseReducerFilterSpatial,

  NoiseReducerFilterTemporal,
  NoiseReducerFilter'
  #-}
