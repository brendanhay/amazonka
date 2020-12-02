{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerMetricName where

import Network.AWS.Prelude

data LoadBalancerMetricName
  = ClientTLSNegotiationErrorCount
  | HTTPCodeInstance2XXCount
  | HTTPCodeInstance3XXCount
  | HTTPCodeInstance4XXCount
  | HTTPCodeInstance5XXCount
  | HTTPCodeLb4XXCount
  | HTTPCodeLb5XXCount
  | HealthyHostCount
  | InstanceResponseTime
  | RejectedConnectionCount
  | RequestCount
  | UnhealthyHostCount
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

instance FromText LoadBalancerMetricName where
  parser =
    takeLowerText >>= \case
      "clienttlsnegotiationerrorcount" -> pure ClientTLSNegotiationErrorCount
      "httpcode_instance_2xx_count" -> pure HTTPCodeInstance2XXCount
      "httpcode_instance_3xx_count" -> pure HTTPCodeInstance3XXCount
      "httpcode_instance_4xx_count" -> pure HTTPCodeInstance4XXCount
      "httpcode_instance_5xx_count" -> pure HTTPCodeInstance5XXCount
      "httpcode_lb_4xx_count" -> pure HTTPCodeLb4XXCount
      "httpcode_lb_5xx_count" -> pure HTTPCodeLb5XXCount
      "healthyhostcount" -> pure HealthyHostCount
      "instanceresponsetime" -> pure InstanceResponseTime
      "rejectedconnectioncount" -> pure RejectedConnectionCount
      "requestcount" -> pure RequestCount
      "unhealthyhostcount" -> pure UnhealthyHostCount
      e ->
        fromTextError $
          "Failure parsing LoadBalancerMetricName from value: '" <> e
            <> "'. Accepted values: clienttlsnegotiationerrorcount, httpcode_instance_2xx_count, httpcode_instance_3xx_count, httpcode_instance_4xx_count, httpcode_instance_5xx_count, httpcode_lb_4xx_count, httpcode_lb_5xx_count, healthyhostcount, instanceresponsetime, rejectedconnectioncount, requestcount, unhealthyhostcount"

instance ToText LoadBalancerMetricName where
  toText = \case
    ClientTLSNegotiationErrorCount -> "ClientTLSNegotiationErrorCount"
    HTTPCodeInstance2XXCount -> "HTTPCode_Instance_2XX_Count"
    HTTPCodeInstance3XXCount -> "HTTPCode_Instance_3XX_Count"
    HTTPCodeInstance4XXCount -> "HTTPCode_Instance_4XX_Count"
    HTTPCodeInstance5XXCount -> "HTTPCode_Instance_5XX_Count"
    HTTPCodeLb4XXCount -> "HTTPCode_LB_4XX_Count"
    HTTPCodeLb5XXCount -> "HTTPCode_LB_5XX_Count"
    HealthyHostCount -> "HealthyHostCount"
    InstanceResponseTime -> "InstanceResponseTime"
    RejectedConnectionCount -> "RejectedConnectionCount"
    RequestCount -> "RequestCount"
    UnhealthyHostCount -> "UnhealthyHostCount"

instance Hashable LoadBalancerMetricName

instance NFData LoadBalancerMetricName

instance ToByteString LoadBalancerMetricName

instance ToQuery LoadBalancerMetricName

instance ToHeader LoadBalancerMetricName

instance ToJSON LoadBalancerMetricName where
  toJSON = toJSONText

instance FromJSON LoadBalancerMetricName where
  parseJSON = parseJSONText "LoadBalancerMetricName"
