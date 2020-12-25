{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        EnvironmentHealthAttributeStatus,
        EnvironmentHealthAttributeColor,
        EnvironmentHealthAttributeCauses,
        EnvironmentHealthAttributeApplicationMetrics,
        EnvironmentHealthAttributeInstancesHealth,
        EnvironmentHealthAttributeAll,
        EnvironmentHealthAttributeHealthStatus,
        EnvironmentHealthAttributeRefreshedAt,
        fromEnvironmentHealthAttribute
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype EnvironmentHealthAttribute = EnvironmentHealthAttribute'
  { fromEnvironmentHealthAttribute ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern EnvironmentHealthAttributeStatus :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttributeStatus = EnvironmentHealthAttribute' "Status"

pattern EnvironmentHealthAttributeColor :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttributeColor = EnvironmentHealthAttribute' "Color"

pattern EnvironmentHealthAttributeCauses :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttributeCauses = EnvironmentHealthAttribute' "Causes"

pattern EnvironmentHealthAttributeApplicationMetrics :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttributeApplicationMetrics = EnvironmentHealthAttribute' "ApplicationMetrics"

pattern EnvironmentHealthAttributeInstancesHealth :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttributeInstancesHealth = EnvironmentHealthAttribute' "InstancesHealth"

pattern EnvironmentHealthAttributeAll :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttributeAll = EnvironmentHealthAttribute' "All"

pattern EnvironmentHealthAttributeHealthStatus :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttributeHealthStatus = EnvironmentHealthAttribute' "HealthStatus"

pattern EnvironmentHealthAttributeRefreshedAt :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttributeRefreshedAt = EnvironmentHealthAttribute' "RefreshedAt"

{-# COMPLETE
  EnvironmentHealthAttributeStatus,
  EnvironmentHealthAttributeColor,
  EnvironmentHealthAttributeCauses,
  EnvironmentHealthAttributeApplicationMetrics,
  EnvironmentHealthAttributeInstancesHealth,
  EnvironmentHealthAttributeAll,
  EnvironmentHealthAttributeHealthStatus,
  EnvironmentHealthAttributeRefreshedAt,
  EnvironmentHealthAttribute'
  #-}
