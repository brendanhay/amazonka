{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ParameterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ParameterType where

import Network.AWS.Prelude

data ParameterType
  = PTCategorical
  | PTContinuous
  | PTFreeText
  | PTInteger
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

instance FromText ParameterType where
  parser =
    takeLowerText >>= \case
      "categorical" -> pure PTCategorical
      "continuous" -> pure PTContinuous
      "freetext" -> pure PTFreeText
      "integer" -> pure PTInteger
      e ->
        fromTextError $
          "Failure parsing ParameterType from value: '" <> e
            <> "'. Accepted values: categorical, continuous, freetext, integer"

instance ToText ParameterType where
  toText = \case
    PTCategorical -> "Categorical"
    PTContinuous -> "Continuous"
    PTFreeText -> "FreeText"
    PTInteger -> "Integer"

instance Hashable ParameterType

instance NFData ParameterType

instance ToByteString ParameterType

instance ToQuery ParameterType

instance ToHeader ParameterType

instance ToJSON ParameterType where
  toJSON = toJSONText

instance FromJSON ParameterType where
  parseJSON = parseJSONText "ParameterType"
