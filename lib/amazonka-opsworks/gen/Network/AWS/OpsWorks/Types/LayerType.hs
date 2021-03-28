{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LayerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.LayerType
  ( LayerType
    ( LayerType'
    , LayerTypeAwsFlowRuby
    , LayerTypeEcsCluster
    , LayerTypeJavaApp
    , LayerTypeLB
    , LayerTypeWeb
    , LayerTypePhpApp
    , LayerTypeRailsApp
    , LayerTypeNodejsApp
    , LayerTypeMemcached
    , LayerTypeDbMaster
    , LayerTypeMonitoringMaster
    , LayerTypeCustom
    , fromLayerType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LayerType = LayerType'{fromLayerType :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern LayerTypeAwsFlowRuby :: LayerType
pattern LayerTypeAwsFlowRuby = LayerType' "aws-flow-ruby"

pattern LayerTypeEcsCluster :: LayerType
pattern LayerTypeEcsCluster = LayerType' "ecs-cluster"

pattern LayerTypeJavaApp :: LayerType
pattern LayerTypeJavaApp = LayerType' "java-app"

pattern LayerTypeLB :: LayerType
pattern LayerTypeLB = LayerType' "lb"

pattern LayerTypeWeb :: LayerType
pattern LayerTypeWeb = LayerType' "web"

pattern LayerTypePhpApp :: LayerType
pattern LayerTypePhpApp = LayerType' "php-app"

pattern LayerTypeRailsApp :: LayerType
pattern LayerTypeRailsApp = LayerType' "rails-app"

pattern LayerTypeNodejsApp :: LayerType
pattern LayerTypeNodejsApp = LayerType' "nodejs-app"

pattern LayerTypeMemcached :: LayerType
pattern LayerTypeMemcached = LayerType' "memcached"

pattern LayerTypeDbMaster :: LayerType
pattern LayerTypeDbMaster = LayerType' "db-master"

pattern LayerTypeMonitoringMaster :: LayerType
pattern LayerTypeMonitoringMaster = LayerType' "monitoring-master"

pattern LayerTypeCustom :: LayerType
pattern LayerTypeCustom = LayerType' "custom"

{-# COMPLETE 
  LayerTypeAwsFlowRuby,

  LayerTypeEcsCluster,

  LayerTypeJavaApp,

  LayerTypeLB,

  LayerTypeWeb,

  LayerTypePhpApp,

  LayerTypeRailsApp,

  LayerTypeNodejsApp,

  LayerTypeMemcached,

  LayerTypeDbMaster,

  LayerTypeMonitoringMaster,

  LayerTypeCustom,
  LayerType'
  #-}
