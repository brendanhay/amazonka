{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectedResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectedResourceType where

import Network.AWS.Prelude

data ProtectedResourceType
  = ApplicationLoadBalancer
  | ClassicLoadBalancer
  | CloudfrontDistribution
  | ElasticIPAllocation
  | GlobalAccelerator
  | Route53HostedZone
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

instance FromText ProtectedResourceType where
  parser =
    takeLowerText >>= \case
      "application_load_balancer" -> pure ApplicationLoadBalancer
      "classic_load_balancer" -> pure ClassicLoadBalancer
      "cloudfront_distribution" -> pure CloudfrontDistribution
      "elastic_ip_allocation" -> pure ElasticIPAllocation
      "global_accelerator" -> pure GlobalAccelerator
      "route_53_hosted_zone" -> pure Route53HostedZone
      e ->
        fromTextError $
          "Failure parsing ProtectedResourceType from value: '" <> e
            <> "'. Accepted values: application_load_balancer, classic_load_balancer, cloudfront_distribution, elastic_ip_allocation, global_accelerator, route_53_hosted_zone"

instance ToText ProtectedResourceType where
  toText = \case
    ApplicationLoadBalancer -> "APPLICATION_LOAD_BALANCER"
    ClassicLoadBalancer -> "CLASSIC_LOAD_BALANCER"
    CloudfrontDistribution -> "CLOUDFRONT_DISTRIBUTION"
    ElasticIPAllocation -> "ELASTIC_IP_ALLOCATION"
    GlobalAccelerator -> "GLOBAL_ACCELERATOR"
    Route53HostedZone -> "ROUTE_53_HOSTED_ZONE"

instance Hashable ProtectedResourceType

instance NFData ProtectedResourceType

instance ToByteString ProtectedResourceType

instance ToQuery ProtectedResourceType

instance ToHeader ProtectedResourceType

instance ToJSON ProtectedResourceType where
  toJSON = toJSONText

instance FromJSON ProtectedResourceType where
  parseJSON = parseJSONText "ProtectedResourceType"
