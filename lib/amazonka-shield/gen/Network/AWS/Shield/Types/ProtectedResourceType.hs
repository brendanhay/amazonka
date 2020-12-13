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
        CloudfrontDistribution,
        Route53HostedZone,
        ElasticIPAllocation,
        ClassicLoadBalancer,
        ApplicationLoadBalancer,
        GlobalAccelerator
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProtectedResourceType = ProtectedResourceType' Lude.Text
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

pattern CloudfrontDistribution :: ProtectedResourceType
pattern CloudfrontDistribution = ProtectedResourceType' "CLOUDFRONT_DISTRIBUTION"

pattern Route53HostedZone :: ProtectedResourceType
pattern Route53HostedZone = ProtectedResourceType' "ROUTE_53_HOSTED_ZONE"

pattern ElasticIPAllocation :: ProtectedResourceType
pattern ElasticIPAllocation = ProtectedResourceType' "ELASTIC_IP_ALLOCATION"

pattern ClassicLoadBalancer :: ProtectedResourceType
pattern ClassicLoadBalancer = ProtectedResourceType' "CLASSIC_LOAD_BALANCER"

pattern ApplicationLoadBalancer :: ProtectedResourceType
pattern ApplicationLoadBalancer = ProtectedResourceType' "APPLICATION_LOAD_BALANCER"

pattern GlobalAccelerator :: ProtectedResourceType
pattern GlobalAccelerator = ProtectedResourceType' "GLOBAL_ACCELERATOR"

{-# COMPLETE
  CloudfrontDistribution,
  Route53HostedZone,
  ElasticIPAllocation,
  ClassicLoadBalancer,
  ApplicationLoadBalancer,
  GlobalAccelerator,
  ProtectedResourceType'
  #-}
