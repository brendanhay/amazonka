{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Ac3CodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3CodingMode where

import Network.AWS.Prelude

-- | Dolby Digital coding mode. Determines number of channels.
data Ac3CodingMode
  = CodingMode10
  | CodingMode11
  | CodingMode20
  | CodingMode32Lfe
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

instance FromText Ac3CodingMode where
  parser =
    takeLowerText >>= \case
      "coding_mode_1_0" -> pure CodingMode10
      "coding_mode_1_1" -> pure CodingMode11
      "coding_mode_2_0" -> pure CodingMode20
      "coding_mode_3_2_lfe" -> pure CodingMode32Lfe
      e ->
        fromTextError $
          "Failure parsing Ac3CodingMode from value: '" <> e
            <> "'. Accepted values: coding_mode_1_0, coding_mode_1_1, coding_mode_2_0, coding_mode_3_2_lfe"

instance ToText Ac3CodingMode where
  toText = \case
    CodingMode10 -> "CODING_MODE_1_0"
    CodingMode11 -> "CODING_MODE_1_1"
    CodingMode20 -> "CODING_MODE_2_0"
    CodingMode32Lfe -> "CODING_MODE_3_2_LFE"

instance Hashable Ac3CodingMode

instance NFData Ac3CodingMode

instance ToByteString Ac3CodingMode

instance ToQuery Ac3CodingMode

instance ToHeader Ac3CodingMode

instance ToJSON Ac3CodingMode where
  toJSON = toJSONText

instance FromJSON Ac3CodingMode where
  parseJSON = parseJSONText "Ac3CodingMode"
