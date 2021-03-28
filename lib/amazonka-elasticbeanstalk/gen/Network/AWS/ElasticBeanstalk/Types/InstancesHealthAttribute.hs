{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.InstancesHealthAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.InstancesHealthAttribute
  ( InstancesHealthAttribute
    ( InstancesHealthAttribute'
    , InstancesHealthAttributeHealthStatus
    , InstancesHealthAttributeColor
    , InstancesHealthAttributeCauses
    , InstancesHealthAttributeApplicationMetrics
    , InstancesHealthAttributeRefreshedAt
    , InstancesHealthAttributeLaunchedAt
    , InstancesHealthAttributeSystem
    , InstancesHealthAttributeDeployment
    , InstancesHealthAttributeAvailabilityZone
    , InstancesHealthAttributeInstanceType
    , InstancesHealthAttributeAll
    , fromInstancesHealthAttribute
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype InstancesHealthAttribute = InstancesHealthAttribute'{fromInstancesHealthAttribute
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern InstancesHealthAttributeHealthStatus :: InstancesHealthAttribute
pattern InstancesHealthAttributeHealthStatus = InstancesHealthAttribute' "HealthStatus"

pattern InstancesHealthAttributeColor :: InstancesHealthAttribute
pattern InstancesHealthAttributeColor = InstancesHealthAttribute' "Color"

pattern InstancesHealthAttributeCauses :: InstancesHealthAttribute
pattern InstancesHealthAttributeCauses = InstancesHealthAttribute' "Causes"

pattern InstancesHealthAttributeApplicationMetrics :: InstancesHealthAttribute
pattern InstancesHealthAttributeApplicationMetrics = InstancesHealthAttribute' "ApplicationMetrics"

pattern InstancesHealthAttributeRefreshedAt :: InstancesHealthAttribute
pattern InstancesHealthAttributeRefreshedAt = InstancesHealthAttribute' "RefreshedAt"

pattern InstancesHealthAttributeLaunchedAt :: InstancesHealthAttribute
pattern InstancesHealthAttributeLaunchedAt = InstancesHealthAttribute' "LaunchedAt"

pattern InstancesHealthAttributeSystem :: InstancesHealthAttribute
pattern InstancesHealthAttributeSystem = InstancesHealthAttribute' "System"

pattern InstancesHealthAttributeDeployment :: InstancesHealthAttribute
pattern InstancesHealthAttributeDeployment = InstancesHealthAttribute' "Deployment"

pattern InstancesHealthAttributeAvailabilityZone :: InstancesHealthAttribute
pattern InstancesHealthAttributeAvailabilityZone = InstancesHealthAttribute' "AvailabilityZone"

pattern InstancesHealthAttributeInstanceType :: InstancesHealthAttribute
pattern InstancesHealthAttributeInstanceType = InstancesHealthAttribute' "InstanceType"

pattern InstancesHealthAttributeAll :: InstancesHealthAttribute
pattern InstancesHealthAttributeAll = InstancesHealthAttribute' "All"

{-# COMPLETE 
  InstancesHealthAttributeHealthStatus,

  InstancesHealthAttributeColor,

  InstancesHealthAttributeCauses,

  InstancesHealthAttributeApplicationMetrics,

  InstancesHealthAttributeRefreshedAt,

  InstancesHealthAttributeLaunchedAt,

  InstancesHealthAttributeSystem,

  InstancesHealthAttributeDeployment,

  InstancesHealthAttributeAvailabilityZone,

  InstancesHealthAttributeInstanceType,

  InstancesHealthAttributeAll,
  InstancesHealthAttribute'
  #-}
