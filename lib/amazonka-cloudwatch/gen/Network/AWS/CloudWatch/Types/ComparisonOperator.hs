{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.ComparisonOperator where

import Network.AWS.Prelude

data ComparisonOperator
  = GreaterThanOrEqualToThreshold
  | GreaterThanThreshold
  | GreaterThanUpperThreshold
  | LessThanLowerOrGreaterThanUpperThreshold
  | LessThanLowerThreshold
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

instance FromText ComparisonOperator where
  parser =
    takeLowerText >>= \case
      "greaterthanorequaltothreshold" -> pure GreaterThanOrEqualToThreshold
      "greaterthanthreshold" -> pure GreaterThanThreshold
      "greaterthanupperthreshold" -> pure GreaterThanUpperThreshold
      "lessthanlowerorgreaterthanupperthreshold" -> pure LessThanLowerOrGreaterThanUpperThreshold
      "lessthanlowerthreshold" -> pure LessThanLowerThreshold
      "lessthanorequaltothreshold" -> pure LessThanOrEqualToThreshold
      "lessthanthreshold" -> pure LessThanThreshold
      e ->
        fromTextError $
          "Failure parsing ComparisonOperator from value: '" <> e
            <> "'. Accepted values: greaterthanorequaltothreshold, greaterthanthreshold, greaterthanupperthreshold, lessthanlowerorgreaterthanupperthreshold, lessthanlowerthreshold, lessthanorequaltothreshold, lessthanthreshold"

instance ToText ComparisonOperator where
  toText = \case
    GreaterThanOrEqualToThreshold -> "GreaterThanOrEqualToThreshold"
    GreaterThanThreshold -> "GreaterThanThreshold"
    GreaterThanUpperThreshold -> "GreaterThanUpperThreshold"
    LessThanLowerOrGreaterThanUpperThreshold -> "LessThanLowerOrGreaterThanUpperThreshold"
    LessThanLowerThreshold -> "LessThanLowerThreshold"
    LessThanOrEqualToThreshold -> "LessThanOrEqualToThreshold"
    LessThanThreshold -> "LessThanThreshold"

instance Hashable ComparisonOperator

instance NFData ComparisonOperator

instance ToByteString ComparisonOperator

instance ToQuery ComparisonOperator

instance ToHeader ComparisonOperator

instance FromXML ComparisonOperator where
  parseXML = parseXMLText "ComparisonOperator"
