{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace where

import Network.AWS.Prelude

data ServiceNamespace
  = Appstream
  | Cassandra
  | Comprehend
  | CustomResource
  | Dynamodb
  | EC2
  | Ecs
  | Elasticmapreduce
  | Kafka
  | Lambda
  | RDS
  | Sagemaker
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

instance FromText ServiceNamespace where
  parser =
    takeLowerText >>= \case
      "appstream" -> pure Appstream
      "cassandra" -> pure Cassandra
      "comprehend" -> pure Comprehend
      "custom-resource" -> pure CustomResource
      "dynamodb" -> pure Dynamodb
      "ec2" -> pure EC2
      "ecs" -> pure Ecs
      "elasticmapreduce" -> pure Elasticmapreduce
      "kafka" -> pure Kafka
      "lambda" -> pure Lambda
      "rds" -> pure RDS
      "sagemaker" -> pure Sagemaker
      e ->
        fromTextError $
          "Failure parsing ServiceNamespace from value: '" <> e
            <> "'. Accepted values: appstream, cassandra, comprehend, custom-resource, dynamodb, ec2, ecs, elasticmapreduce, kafka, lambda, rds, sagemaker"

instance ToText ServiceNamespace where
  toText = \case
    Appstream -> "appstream"
    Cassandra -> "cassandra"
    Comprehend -> "comprehend"
    CustomResource -> "custom-resource"
    Dynamodb -> "dynamodb"
    EC2 -> "ec2"
    Ecs -> "ecs"
    Elasticmapreduce -> "elasticmapreduce"
    Kafka -> "kafka"
    Lambda -> "lambda"
    RDS -> "rds"
    Sagemaker -> "sagemaker"

instance Hashable ServiceNamespace

instance NFData ServiceNamespace

instance ToByteString ServiceNamespace

instance ToQuery ServiceNamespace

instance ToHeader ServiceNamespace

instance ToJSON ServiceNamespace where
  toJSON = toJSONText

instance FromJSON ServiceNamespace where
  parseJSON = parseJSONText "ServiceNamespace"
