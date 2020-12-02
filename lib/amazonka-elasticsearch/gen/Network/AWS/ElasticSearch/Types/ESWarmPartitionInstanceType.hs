{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType where

import Network.AWS.Prelude

data ESWarmPartitionInstanceType
  = ESWPITULTRAWARM1_Large_Elasticsearch
  | ESWPITULTRAWARM1_Medium_Elasticsearch
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

instance FromText ESWarmPartitionInstanceType where
  parser =
    takeLowerText >>= \case
      "ultrawarm1.large.elasticsearch" -> pure ESWPITULTRAWARM1_Large_Elasticsearch
      "ultrawarm1.medium.elasticsearch" -> pure ESWPITULTRAWARM1_Medium_Elasticsearch
      e ->
        fromTextError $
          "Failure parsing ESWarmPartitionInstanceType from value: '" <> e
            <> "'. Accepted values: ultrawarm1.large.elasticsearch, ultrawarm1.medium.elasticsearch"

instance ToText ESWarmPartitionInstanceType where
  toText = \case
    ESWPITULTRAWARM1_Large_Elasticsearch -> "ultrawarm1.large.elasticsearch"
    ESWPITULTRAWARM1_Medium_Elasticsearch -> "ultrawarm1.medium.elasticsearch"

instance Hashable ESWarmPartitionInstanceType

instance NFData ESWarmPartitionInstanceType

instance ToByteString ESWarmPartitionInstanceType

instance ToQuery ESWarmPartitionInstanceType

instance ToHeader ESWarmPartitionInstanceType

instance ToJSON ESWarmPartitionInstanceType where
  toJSON = toJSONText

instance FromJSON ESWarmPartitionInstanceType where
  parseJSON = parseJSONText "ESWarmPartitionInstanceType"
