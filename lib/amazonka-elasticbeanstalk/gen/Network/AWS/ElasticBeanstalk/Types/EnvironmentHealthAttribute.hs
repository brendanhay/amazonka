-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthAttribute
  ( EnvironmentHealthAttribute
      ( EnvironmentHealthAttribute',
        EHAAll,
        EHAApplicationMetrics,
        EHACauses,
        EHAColor,
        EHAHealthStatus,
        EHAInstancesHealth,
        EHARefreshedAt,
        EHAStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EnvironmentHealthAttribute = EnvironmentHealthAttribute' Lude.Text
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

pattern EHAAll :: EnvironmentHealthAttribute
pattern EHAAll = EnvironmentHealthAttribute' "All"

pattern EHAApplicationMetrics :: EnvironmentHealthAttribute
pattern EHAApplicationMetrics = EnvironmentHealthAttribute' "ApplicationMetrics"

pattern EHACauses :: EnvironmentHealthAttribute
pattern EHACauses = EnvironmentHealthAttribute' "Causes"

pattern EHAColor :: EnvironmentHealthAttribute
pattern EHAColor = EnvironmentHealthAttribute' "Color"

pattern EHAHealthStatus :: EnvironmentHealthAttribute
pattern EHAHealthStatus = EnvironmentHealthAttribute' "HealthStatus"

pattern EHAInstancesHealth :: EnvironmentHealthAttribute
pattern EHAInstancesHealth = EnvironmentHealthAttribute' "InstancesHealth"

pattern EHARefreshedAt :: EnvironmentHealthAttribute
pattern EHARefreshedAt = EnvironmentHealthAttribute' "RefreshedAt"

pattern EHAStatus :: EnvironmentHealthAttribute
pattern EHAStatus = EnvironmentHealthAttribute' "Status"

{-# COMPLETE
  EHAAll,
  EHAApplicationMetrics,
  EHACauses,
  EHAColor,
  EHAHealthStatus,
  EHAInstancesHealth,
  EHARefreshedAt,
  EHAStatus,
  EnvironmentHealthAttribute'
  #-}
