{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm where

import Network.AWS.Prelude

-- | Audio Normalization Algorithm
data AudioNormalizationAlgorithm
  = Itu17701
  | Itu17702
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

instance FromText AudioNormalizationAlgorithm where
  parser =
    takeLowerText >>= \case
      "itu_1770_1" -> pure Itu17701
      "itu_1770_2" -> pure Itu17702
      e ->
        fromTextError $
          "Failure parsing AudioNormalizationAlgorithm from value: '" <> e
            <> "'. Accepted values: itu_1770_1, itu_1770_2"

instance ToText AudioNormalizationAlgorithm where
  toText = \case
    Itu17701 -> "ITU_1770_1"
    Itu17702 -> "ITU_1770_2"

instance Hashable AudioNormalizationAlgorithm

instance NFData AudioNormalizationAlgorithm

instance ToByteString AudioNormalizationAlgorithm

instance ToQuery AudioNormalizationAlgorithm

instance ToHeader AudioNormalizationAlgorithm

instance ToJSON AudioNormalizationAlgorithm where
  toJSON = toJSONText

instance FromJSON AudioNormalizationAlgorithm where
  parseJSON = parseJSONText "AudioNormalizationAlgorithm"
