{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LayerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.LayerType where

import Network.AWS.Prelude

data LayerType
  = AWSFlowRuby
  | Custom
  | DBMaster
  | EcsCluster
  | JavaApp
  | LB
  | Memcached
  | MonitoringMaster
  | NodejsApp
  | PHPApp
  | RailsApp
  | Web
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

instance FromText LayerType where
  parser =
    takeLowerText >>= \case
      "aws-flow-ruby" -> pure AWSFlowRuby
      "custom" -> pure Custom
      "db-master" -> pure DBMaster
      "ecs-cluster" -> pure EcsCluster
      "java-app" -> pure JavaApp
      "lb" -> pure LB
      "memcached" -> pure Memcached
      "monitoring-master" -> pure MonitoringMaster
      "nodejs-app" -> pure NodejsApp
      "php-app" -> pure PHPApp
      "rails-app" -> pure RailsApp
      "web" -> pure Web
      e ->
        fromTextError $
          "Failure parsing LayerType from value: '" <> e
            <> "'. Accepted values: aws-flow-ruby, custom, db-master, ecs-cluster, java-app, lb, memcached, monitoring-master, nodejs-app, php-app, rails-app, web"

instance ToText LayerType where
  toText = \case
    AWSFlowRuby -> "aws-flow-ruby"
    Custom -> "custom"
    DBMaster -> "db-master"
    EcsCluster -> "ecs-cluster"
    JavaApp -> "java-app"
    LB -> "lb"
    Memcached -> "memcached"
    MonitoringMaster -> "monitoring-master"
    NodejsApp -> "nodejs-app"
    PHPApp -> "php-app"
    RailsApp -> "rails-app"
    Web -> "web"

instance Hashable LayerType

instance NFData LayerType

instance ToByteString LayerType

instance ToQuery LayerType

instance ToHeader LayerType

instance ToJSON LayerType where
  toJSON = toJSONText

instance FromJSON LayerType where
  parseJSON = parseJSONText "LayerType"
