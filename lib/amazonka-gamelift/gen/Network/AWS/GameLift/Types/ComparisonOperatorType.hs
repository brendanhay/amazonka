{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ComparisonOperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ComparisonOperatorType where

import Network.AWS.Prelude

data ComparisonOperatorType
  = GreaterThanOrEqualToThreshold
  | GreaterThanThreshold
  | LessThanOrEqualToThreshold
  | LessThanThreshold
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

instance FromText ComparisonOperatorType where
  parser =
    takeLowerText >>= \case
      "greaterthanorequaltothreshold" -> pure GreaterThanOrEqualToThreshold
      "greaterthanthreshold" -> pure GreaterThanThreshold
      "lessthanorequaltothreshold" -> pure LessThanOrEqualToThreshold
      "lessthanthreshold" -> pure LessThanThreshold
      e ->
        fromTextError $
          "Failure parsing ComparisonOperatorType from value: '" <> e
            <> "'. Accepted values: greaterthanorequaltothreshold, greaterthanthreshold, lessthanorequaltothreshold, lessthanthreshold"

instance ToText ComparisonOperatorType where
  toText = \case
    GreaterThanOrEqualToThreshold -> "GreaterThanOrEqualToThreshold"
    GreaterThanThreshold -> "GreaterThanThreshold"
    LessThanOrEqualToThreshold -> "LessThanOrEqualToThreshold"
    LessThanThreshold -> "LessThanThreshold"

instance Hashable ComparisonOperatorType

instance NFData ComparisonOperatorType

instance ToByteString ComparisonOperatorType

instance ToQuery ComparisonOperatorType

instance ToHeader ComparisonOperatorType

instance ToJSON ComparisonOperatorType where
  toJSON = toJSONText

instance FromJSON ComparisonOperatorType where
  parseJSON = parseJSONText "ComparisonOperatorType"
