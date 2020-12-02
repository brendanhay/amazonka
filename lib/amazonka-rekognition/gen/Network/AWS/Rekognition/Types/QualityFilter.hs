{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.QualityFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.QualityFilter where

import Network.AWS.Prelude

data QualityFilter
  = Auto
  | High
  | Low
  | Medium
  | None
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

instance FromText QualityFilter where
  parser =
    takeLowerText >>= \case
      "auto" -> pure Auto
      "high" -> pure High
      "low" -> pure Low
      "medium" -> pure Medium
      "none" -> pure None
      e ->
        fromTextError $
          "Failure parsing QualityFilter from value: '" <> e
            <> "'. Accepted values: auto, high, low, medium, none"

instance ToText QualityFilter where
  toText = \case
    Auto -> "AUTO"
    High -> "HIGH"
    Low -> "LOW"
    Medium -> "MEDIUM"
    None -> "NONE"

instance Hashable QualityFilter

instance NFData QualityFilter

instance ToByteString QualityFilter

instance ToQuery QualityFilter

instance ToHeader QualityFilter

instance ToJSON QualityFilter where
  toJSON = toJSONText
