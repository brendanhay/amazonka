{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerFilter
  ( NoiseReducerFilter
      ( ..,
        NoiseReducerFilter_BILATERAL,
        NoiseReducerFilter_CONSERVE,
        NoiseReducerFilter_GAUSSIAN,
        NoiseReducerFilter_LANCZOS,
        NoiseReducerFilter_MEAN,
        NoiseReducerFilter_SHARPEN,
        NoiseReducerFilter_SPATIAL,
        NoiseReducerFilter_TEMPORAL
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Use Noise reducer filter (NoiseReducerFilter) to select one of the
-- following spatial image filtering functions. To use this setting, you
-- must also enable Noise reducer (NoiseReducer). * Bilateral preserves
-- edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and
-- Sharpen (sharpest) do convolution filtering. * Conserve does min\/max
-- noise reduction. * Spatial does frequency-domain filtering based on JND
-- principles. * Temporal optimizes video quality for complex motion.
newtype NoiseReducerFilter = NoiseReducerFilter'
  { fromNoiseReducerFilter ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern NoiseReducerFilter_BILATERAL :: NoiseReducerFilter
pattern NoiseReducerFilter_BILATERAL = NoiseReducerFilter' "BILATERAL"

pattern NoiseReducerFilter_CONSERVE :: NoiseReducerFilter
pattern NoiseReducerFilter_CONSERVE = NoiseReducerFilter' "CONSERVE"

pattern NoiseReducerFilter_GAUSSIAN :: NoiseReducerFilter
pattern NoiseReducerFilter_GAUSSIAN = NoiseReducerFilter' "GAUSSIAN"

pattern NoiseReducerFilter_LANCZOS :: NoiseReducerFilter
pattern NoiseReducerFilter_LANCZOS = NoiseReducerFilter' "LANCZOS"

pattern NoiseReducerFilter_MEAN :: NoiseReducerFilter
pattern NoiseReducerFilter_MEAN = NoiseReducerFilter' "MEAN"

pattern NoiseReducerFilter_SHARPEN :: NoiseReducerFilter
pattern NoiseReducerFilter_SHARPEN = NoiseReducerFilter' "SHARPEN"

pattern NoiseReducerFilter_SPATIAL :: NoiseReducerFilter
pattern NoiseReducerFilter_SPATIAL = NoiseReducerFilter' "SPATIAL"

pattern NoiseReducerFilter_TEMPORAL :: NoiseReducerFilter
pattern NoiseReducerFilter_TEMPORAL = NoiseReducerFilter' "TEMPORAL"

{-# COMPLETE
  NoiseReducerFilter_BILATERAL,
  NoiseReducerFilter_CONSERVE,
  NoiseReducerFilter_GAUSSIAN,
  NoiseReducerFilter_LANCZOS,
  NoiseReducerFilter_MEAN,
  NoiseReducerFilter_SHARPEN,
  NoiseReducerFilter_SPATIAL,
  NoiseReducerFilter_TEMPORAL,
  NoiseReducerFilter'
  #-}
