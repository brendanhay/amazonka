{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.InstancesHealthAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.InstancesHealthAttribute
  ( InstancesHealthAttribute
      ( InstancesHealthAttribute',
        All,
        ApplicationMetrics,
        AvailabilityZone,
        Causes,
        Color,
        Deployment,
        HealthStatus,
        InstanceType,
        LaunchedAt,
        RefreshedAt,
        System
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstancesHealthAttribute = InstancesHealthAttribute' Lude.Text
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

pattern All :: InstancesHealthAttribute
pattern All = InstancesHealthAttribute' "All"

pattern ApplicationMetrics :: InstancesHealthAttribute
pattern ApplicationMetrics = InstancesHealthAttribute' "ApplicationMetrics"

pattern AvailabilityZone :: InstancesHealthAttribute
pattern AvailabilityZone = InstancesHealthAttribute' "AvailabilityZone"

pattern Causes :: InstancesHealthAttribute
pattern Causes = InstancesHealthAttribute' "Causes"

pattern Color :: InstancesHealthAttribute
pattern Color = InstancesHealthAttribute' "Color"

pattern Deployment :: InstancesHealthAttribute
pattern Deployment = InstancesHealthAttribute' "Deployment"

pattern HealthStatus :: InstancesHealthAttribute
pattern HealthStatus = InstancesHealthAttribute' "HealthStatus"

pattern InstanceType :: InstancesHealthAttribute
pattern InstanceType = InstancesHealthAttribute' "InstanceType"

pattern LaunchedAt :: InstancesHealthAttribute
pattern LaunchedAt = InstancesHealthAttribute' "LaunchedAt"

pattern RefreshedAt :: InstancesHealthAttribute
pattern RefreshedAt = InstancesHealthAttribute' "RefreshedAt"

pattern System :: InstancesHealthAttribute
pattern System = InstancesHealthAttribute' "System"

{-# COMPLETE
  All,
  ApplicationMetrics,
  AvailabilityZone,
  Causes,
  Color,
  Deployment,
  HealthStatus,
  InstanceType,
  LaunchedAt,
  RefreshedAt,
  System,
  InstancesHealthAttribute'
  #-}
