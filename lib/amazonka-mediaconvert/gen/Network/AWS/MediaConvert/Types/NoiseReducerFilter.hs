{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerFilter where

import Network.AWS.Prelude

-- | Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral preserves edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) do convolution filtering. * Conserve does min/max noise reduction. * Spatial does frequency-domain filtering based on JND principles. * Temporal optimizes video quality for complex motion.
data NoiseReducerFilter
  = Bilateral
  | Conserve
  | Gaussian
  | Lanczos
  | Mean
  | Sharpen
  | Spatial
  | Temporal
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText NoiseReducerFilter where
  parser =
    takeLowerText >>= \case
      "bilateral" -> pure Bilateral
      "conserve" -> pure Conserve
      "gaussian" -> pure Gaussian
      "lanczos" -> pure Lanczos
      "mean" -> pure Mean
      "sharpen" -> pure Sharpen
      "spatial" -> pure Spatial
      "temporal" -> pure Temporal
      e ->
        fromTextError $
          "Failure parsing NoiseReducerFilter from value: '" <> e
            <> "'. Accepted values: bilateral, conserve, gaussian, lanczos, mean, sharpen, spatial, temporal"

instance ToText NoiseReducerFilter where
  toText = \case
    Bilateral -> "BILATERAL"
    Conserve -> "CONSERVE"
    Gaussian -> "GAUSSIAN"
    Lanczos -> "LANCZOS"
    Mean -> "MEAN"
    Sharpen -> "SHARPEN"
    Spatial -> "SPATIAL"
    Temporal -> "TEMPORAL"

instance Hashable NoiseReducerFilter

instance NFData NoiseReducerFilter

instance ToByteString NoiseReducerFilter

instance ToQuery NoiseReducerFilter

instance ToHeader NoiseReducerFilter

instance ToJSON NoiseReducerFilter where
  toJSON = toJSONText

instance FromJSON NoiseReducerFilter where
  parseJSON = parseJSONText "NoiseReducerFilter"
