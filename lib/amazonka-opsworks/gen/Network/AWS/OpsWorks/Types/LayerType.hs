{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LayerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.LayerType
  ( LayerType
      ( LayerType',
        AWSFlowRuby,
        EcsCluster,
        JavaApp,
        LB,
        Web,
        PHPApp,
        RailsApp,
        NodejsApp,
        Memcached,
        DBMaster,
        MonitoringMaster,
        Custom
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LayerType = LayerType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AWSFlowRuby :: LayerType
pattern AWSFlowRuby = LayerType' "aws-flow-ruby"

pattern EcsCluster :: LayerType
pattern EcsCluster = LayerType' "ecs-cluster"

pattern JavaApp :: LayerType
pattern JavaApp = LayerType' "java-app"

pattern LB :: LayerType
pattern LB = LayerType' "lb"

pattern Web :: LayerType
pattern Web = LayerType' "web"

pattern PHPApp :: LayerType
pattern PHPApp = LayerType' "php-app"

pattern RailsApp :: LayerType
pattern RailsApp = LayerType' "rails-app"

pattern NodejsApp :: LayerType
pattern NodejsApp = LayerType' "nodejs-app"

pattern Memcached :: LayerType
pattern Memcached = LayerType' "memcached"

pattern DBMaster :: LayerType
pattern DBMaster = LayerType' "db-master"

pattern MonitoringMaster :: LayerType
pattern MonitoringMaster = LayerType' "monitoring-master"

pattern Custom :: LayerType
pattern Custom = LayerType' "custom"

{-# COMPLETE
  AWSFlowRuby,
  EcsCluster,
  JavaApp,
  LB,
  Web,
  PHPApp,
  RailsApp,
  NodejsApp,
  Memcached,
  DBMaster,
  MonitoringMaster,
  Custom,
  LayerType'
  #-}
