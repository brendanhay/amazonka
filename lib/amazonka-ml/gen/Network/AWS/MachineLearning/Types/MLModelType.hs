{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.MLModelType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.MLModelType where

import Network.AWS.Prelude

data MLModelType
  = Binary
  | Multiclass
  | Regression
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

instance FromText MLModelType where
  parser =
    takeLowerText >>= \case
      "binary" -> pure Binary
      "multiclass" -> pure Multiclass
      "regression" -> pure Regression
      e ->
        fromTextError $
          "Failure parsing MLModelType from value: '" <> e
            <> "'. Accepted values: binary, multiclass, regression"

instance ToText MLModelType where
  toText = \case
    Binary -> "BINARY"
    Multiclass -> "MULTICLASS"
    Regression -> "REGRESSION"

instance Hashable MLModelType

instance NFData MLModelType

instance ToByteString MLModelType

instance ToQuery MLModelType

instance ToHeader MLModelType

instance ToJSON MLModelType where
  toJSON = toJSONText

instance FromJSON MLModelType where
  parseJSON = parseJSONText "MLModelType"
