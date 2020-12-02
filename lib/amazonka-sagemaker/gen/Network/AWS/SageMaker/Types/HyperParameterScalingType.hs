{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterScalingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterScalingType where

import Network.AWS.Prelude

data HyperParameterScalingType
  = Auto
  | Linear
  | Logarithmic
  | ReverseLogarithmic
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

instance FromText HyperParameterScalingType where
  parser =
    takeLowerText >>= \case
      "auto" -> pure Auto
      "linear" -> pure Linear
      "logarithmic" -> pure Logarithmic
      "reverselogarithmic" -> pure ReverseLogarithmic
      e ->
        fromTextError $
          "Failure parsing HyperParameterScalingType from value: '" <> e
            <> "'. Accepted values: auto, linear, logarithmic, reverselogarithmic"

instance ToText HyperParameterScalingType where
  toText = \case
    Auto -> "Auto"
    Linear -> "Linear"
    Logarithmic -> "Logarithmic"
    ReverseLogarithmic -> "ReverseLogarithmic"

instance Hashable HyperParameterScalingType

instance NFData HyperParameterScalingType

instance ToByteString HyperParameterScalingType

instance ToQuery HyperParameterScalingType

instance ToHeader HyperParameterScalingType

instance ToJSON HyperParameterScalingType where
  toJSON = toJSONText

instance FromJSON HyperParameterScalingType where
  parseJSON = parseJSONText "HyperParameterScalingType"
