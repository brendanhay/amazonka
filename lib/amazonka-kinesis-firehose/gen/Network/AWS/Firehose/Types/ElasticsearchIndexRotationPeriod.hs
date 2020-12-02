{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod where

import Network.AWS.Prelude

data ElasticsearchIndexRotationPeriod
  = NoRotation
  | OneDay
  | OneHour
  | OneMonth
  | OneWeek
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

instance FromText ElasticsearchIndexRotationPeriod where
  parser =
    takeLowerText >>= \case
      "norotation" -> pure NoRotation
      "oneday" -> pure OneDay
      "onehour" -> pure OneHour
      "onemonth" -> pure OneMonth
      "oneweek" -> pure OneWeek
      e ->
        fromTextError $
          "Failure parsing ElasticsearchIndexRotationPeriod from value: '" <> e
            <> "'. Accepted values: norotation, oneday, onehour, onemonth, oneweek"

instance ToText ElasticsearchIndexRotationPeriod where
  toText = \case
    NoRotation -> "NoRotation"
    OneDay -> "OneDay"
    OneHour -> "OneHour"
    OneMonth -> "OneMonth"
    OneWeek -> "OneWeek"

instance Hashable ElasticsearchIndexRotationPeriod

instance NFData ElasticsearchIndexRotationPeriod

instance ToByteString ElasticsearchIndexRotationPeriod

instance ToQuery ElasticsearchIndexRotationPeriod

instance ToHeader ElasticsearchIndexRotationPeriod

instance ToJSON ElasticsearchIndexRotationPeriod where
  toJSON = toJSONText

instance FromJSON ElasticsearchIndexRotationPeriod where
  parseJSON = parseJSONText "ElasticsearchIndexRotationPeriod"
