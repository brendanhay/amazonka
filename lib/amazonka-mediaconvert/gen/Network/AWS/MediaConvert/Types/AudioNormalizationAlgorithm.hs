{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithm where

import Network.AWS.Prelude

-- | Choose one of the following audio normalization algorithms: ITU-R BS.1770-1: Ungated loudness. A measurement of ungated average loudness for an entire piece of content, suitable for measurement of short-form content under ATSC recommendation A/85. Supports up to 5.1 audio channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated average loudness compliant with the requirements of EBU-R128. Supports up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same loudness measurement algorithm as 1770-2, with an updated true peak measurement. ITU-R BS.1770-4: Higher channel count. Allows for more audio channels than the other algorithms, including configurations such as 7.1.
data AudioNormalizationAlgorithm
  = ItuBs17701
  | ItuBs17702
  | ItuBs17703
  | ItuBs17704
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
      "itu_bs_1770_1" -> pure ItuBs17701
      "itu_bs_1770_2" -> pure ItuBs17702
      "itu_bs_1770_3" -> pure ItuBs17703
      "itu_bs_1770_4" -> pure ItuBs17704
      e ->
        fromTextError $
          "Failure parsing AudioNormalizationAlgorithm from value: '" <> e
            <> "'. Accepted values: itu_bs_1770_1, itu_bs_1770_2, itu_bs_1770_3, itu_bs_1770_4"

instance ToText AudioNormalizationAlgorithm where
  toText = \case
    ItuBs17701 -> "ITU_BS_1770_1"
    ItuBs17702 -> "ITU_BS_1770_2"
    ItuBs17703 -> "ITU_BS_1770_3"
    ItuBs17704 -> "ITU_BS_1770_4"

instance Hashable AudioNormalizationAlgorithm

instance NFData AudioNormalizationAlgorithm

instance ToByteString AudioNormalizationAlgorithm

instance ToQuery AudioNormalizationAlgorithm

instance ToHeader AudioNormalizationAlgorithm

instance ToJSON AudioNormalizationAlgorithm where
  toJSON = toJSONText

instance FromJSON AudioNormalizationAlgorithm where
  parseJSON = parseJSONText "AudioNormalizationAlgorithm"
