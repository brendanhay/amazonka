{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TechnicalCueType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TechnicalCueType where

import Network.AWS.Prelude

data TechnicalCueType
  = BlackFrames
  | ColorBars
  | EndCredits
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

instance FromText TechnicalCueType where
  parser =
    takeLowerText >>= \case
      "blackframes" -> pure BlackFrames
      "colorbars" -> pure ColorBars
      "endcredits" -> pure EndCredits
      e ->
        fromTextError $
          "Failure parsing TechnicalCueType from value: '" <> e
            <> "'. Accepted values: blackframes, colorbars, endcredits"

instance ToText TechnicalCueType where
  toText = \case
    BlackFrames -> "BlackFrames"
    ColorBars -> "ColorBars"
    EndCredits -> "EndCredits"

instance Hashable TechnicalCueType

instance NFData TechnicalCueType

instance ToByteString TechnicalCueType

instance ToQuery TechnicalCueType

instance ToHeader TechnicalCueType

instance FromJSON TechnicalCueType where
  parseJSON = parseJSONText "TechnicalCueType"
