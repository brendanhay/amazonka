{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.DetailsAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.DetailsAttributes where

import Network.AWS.Prelude

-- | Contains the key values of @DetailsMap@ : @PredictiveModelType@ - Indicates the type of the @MLModel@ . @Algorithm@ - Indicates the algorithm that was used for the @MLModel@ .
data DetailsAttributes
  = Algorithm
  | PredictiveModelType
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

instance FromText DetailsAttributes where
  parser =
    takeLowerText >>= \case
      "algorithm" -> pure Algorithm
      "predictivemodeltype" -> pure PredictiveModelType
      e ->
        fromTextError $
          "Failure parsing DetailsAttributes from value: '" <> e
            <> "'. Accepted values: algorithm, predictivemodeltype"

instance ToText DetailsAttributes where
  toText = \case
    Algorithm -> "Algorithm"
    PredictiveModelType -> "PredictiveModelType"

instance Hashable DetailsAttributes

instance NFData DetailsAttributes

instance ToByteString DetailsAttributes

instance ToQuery DetailsAttributes

instance ToHeader DetailsAttributes

instance FromJSON DetailsAttributes where
  parseJSON = parseJSONText "DetailsAttributes"
