{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ServiceNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ServiceNamespace where

import Network.AWS.Prelude

data ServiceNamespace
  = Autoscaling
  | Dynamodb
  | EC2
  | Ecs
  | RDS
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
      "autoscaling" -> pure Autoscaling
      "dynamodb" -> pure Dynamodb
      "ec2" -> pure EC2
      "ecs" -> pure Ecs
      "rds" -> pure RDS
      e ->
        fromTextError $
          "Failure parsing ServiceNamespace from value: '" <> e
            <> "'. Accepted values: autoscaling, dynamodb, ec2, ecs, rds"

instance ToText ServiceNamespace where
  toText = \case
    Autoscaling -> "autoscaling"
    Dynamodb -> "dynamodb"
    EC2 -> "ec2"
    Ecs -> "ecs"
    RDS -> "rds"

instance Hashable ServiceNamespace

instance NFData ServiceNamespace

instance ToByteString ServiceNamespace

instance ToQuery ServiceNamespace

instance ToHeader ServiceNamespace

instance ToJSON ServiceNamespace where
  toJSON = toJSONText

instance FromJSON ServiceNamespace where
  parseJSON = parseJSONText "ServiceNamespace"
