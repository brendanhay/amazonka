{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectedResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectedResourceType
  ( ProtectedResourceType
      ( ProtectedResourceType',
        ProtectedResourceTypeCloudfrontDistribution,
        ProtectedResourceTypeRoute53HostedZone,
        ProtectedResourceTypeElasticIpAllocation,
        ProtectedResourceTypeClassicLoadBalancer,
        ProtectedResourceTypeApplicationLoadBalancer,
        ProtectedResourceTypeGlobalAccelerator,
        fromProtectedResourceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ProtectedResourceType = ProtectedResourceType'
  { fromProtectedResourceType ::
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

pattern ProtectedResourceTypeCloudfrontDistribution :: ProtectedResourceType
pattern ProtectedResourceTypeCloudfrontDistribution = ProtectedResourceType' "CLOUDFRONT_DISTRIBUTION"

pattern ProtectedResourceTypeRoute53HostedZone :: ProtectedResourceType
pattern ProtectedResourceTypeRoute53HostedZone = ProtectedResourceType' "ROUTE_53_HOSTED_ZONE"

pattern ProtectedResourceTypeElasticIpAllocation :: ProtectedResourceType
pattern ProtectedResourceTypeElasticIpAllocation = ProtectedResourceType' "ELASTIC_IP_ALLOCATION"

pattern ProtectedResourceTypeClassicLoadBalancer :: ProtectedResourceType
pattern ProtectedResourceTypeClassicLoadBalancer = ProtectedResourceType' "CLASSIC_LOAD_BALANCER"

pattern ProtectedResourceTypeApplicationLoadBalancer :: ProtectedResourceType
pattern ProtectedResourceTypeApplicationLoadBalancer = ProtectedResourceType' "APPLICATION_LOAD_BALANCER"

pattern ProtectedResourceTypeGlobalAccelerator :: ProtectedResourceType
pattern ProtectedResourceTypeGlobalAccelerator = ProtectedResourceType' "GLOBAL_ACCELERATOR"

{-# COMPLETE
  ProtectedResourceTypeCloudfrontDistribution,
  ProtectedResourceTypeRoute53HostedZone,
  ProtectedResourceTypeElasticIpAllocation,
  ProtectedResourceTypeClassicLoadBalancer,
  ProtectedResourceTypeApplicationLoadBalancer,
  ProtectedResourceTypeGlobalAccelerator,
  ProtectedResourceType'
  #-}
