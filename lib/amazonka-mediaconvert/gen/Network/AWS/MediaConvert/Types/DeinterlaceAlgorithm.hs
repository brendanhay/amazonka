{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DeinterlaceAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DeinterlaceAlgorithm where

import Network.AWS.Prelude

-- | Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.
data DeinterlaceAlgorithm
  = DABlend
  | DABlendTicker
  | DAInterpolate
  | DAInterpolateTicker
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

instance FromText DeinterlaceAlgorithm where
  parser =
    takeLowerText >>= \case
      "blend" -> pure DABlend
      "blend_ticker" -> pure DABlendTicker
      "interpolate" -> pure DAInterpolate
      "interpolate_ticker" -> pure DAInterpolateTicker
      e ->
        fromTextError $
          "Failure parsing DeinterlaceAlgorithm from value: '" <> e
            <> "'. Accepted values: blend, blend_ticker, interpolate, interpolate_ticker"

instance ToText DeinterlaceAlgorithm where
  toText = \case
    DABlend -> "BLEND"
    DABlendTicker -> "BLEND_TICKER"
    DAInterpolate -> "INTERPOLATE"
    DAInterpolateTicker -> "INTERPOLATE_TICKER"

instance Hashable DeinterlaceAlgorithm

instance NFData DeinterlaceAlgorithm

instance ToByteString DeinterlaceAlgorithm

instance ToQuery DeinterlaceAlgorithm

instance ToHeader DeinterlaceAlgorithm

instance ToJSON DeinterlaceAlgorithm where
  toJSON = toJSONText

instance FromJSON DeinterlaceAlgorithm where
  parseJSON = parseJSONText "DeinterlaceAlgorithm"
