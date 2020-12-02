{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DataSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.DataSourceType where

import Network.AWS.Prelude

data DataSourceType
  = AWSLambda
  | AmazonDynamodb
  | AmazonElasticsearch
  | HTTP
  | None
  | RelationalDatabase
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

instance FromText DataSourceType where
  parser =
    takeLowerText >>= \case
      "aws_lambda" -> pure AWSLambda
      "amazon_dynamodb" -> pure AmazonDynamodb
      "amazon_elasticsearch" -> pure AmazonElasticsearch
      "http" -> pure HTTP
      "none" -> pure None
      "relational_database" -> pure RelationalDatabase
      e ->
        fromTextError $
          "Failure parsing DataSourceType from value: '" <> e
            <> "'. Accepted values: aws_lambda, amazon_dynamodb, amazon_elasticsearch, http, none, relational_database"

instance ToText DataSourceType where
  toText = \case
    AWSLambda -> "AWS_LAMBDA"
    AmazonDynamodb -> "AMAZON_DYNAMODB"
    AmazonElasticsearch -> "AMAZON_ELASTICSEARCH"
    HTTP -> "HTTP"
    None -> "NONE"
    RelationalDatabase -> "RELATIONAL_DATABASE"

instance Hashable DataSourceType

instance NFData DataSourceType

instance ToByteString DataSourceType

instance ToQuery DataSourceType

instance ToHeader DataSourceType

instance ToJSON DataSourceType where
  toJSON = toJSONText

instance FromJSON DataSourceType where
  parseJSON = parseJSONText "DataSourceType"
