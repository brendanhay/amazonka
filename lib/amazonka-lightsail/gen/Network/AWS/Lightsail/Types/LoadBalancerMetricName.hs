{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerMetricName
  ( LoadBalancerMetricName
      ( LoadBalancerMetricName',
        ClientTLSNegotiationErrorCount,
        HTTPCodeInstance2XXCount,
        HTTPCodeInstance3XXCount,
        HTTPCodeInstance4XXCount,
        HTTPCodeInstance5XXCount,
        HTTPCodeLb4XXCount,
        HTTPCodeLb5XXCount,
        HealthyHostCount,
        InstanceResponseTime,
        RejectedConnectionCount,
        RequestCount,
        UnhealthyHostCount
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LoadBalancerMetricName = LoadBalancerMetricName' Lude.Text
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

pattern ClientTLSNegotiationErrorCount :: LoadBalancerMetricName
pattern ClientTLSNegotiationErrorCount = LoadBalancerMetricName' "ClientTLSNegotiationErrorCount"

pattern HTTPCodeInstance2XXCount :: LoadBalancerMetricName
pattern HTTPCodeInstance2XXCount = LoadBalancerMetricName' "HTTPCode_Instance_2XX_Count"

pattern HTTPCodeInstance3XXCount :: LoadBalancerMetricName
pattern HTTPCodeInstance3XXCount = LoadBalancerMetricName' "HTTPCode_Instance_3XX_Count"

pattern HTTPCodeInstance4XXCount :: LoadBalancerMetricName
pattern HTTPCodeInstance4XXCount = LoadBalancerMetricName' "HTTPCode_Instance_4XX_Count"

pattern HTTPCodeInstance5XXCount :: LoadBalancerMetricName
pattern HTTPCodeInstance5XXCount = LoadBalancerMetricName' "HTTPCode_Instance_5XX_Count"

pattern HTTPCodeLb4XXCount :: LoadBalancerMetricName
pattern HTTPCodeLb4XXCount = LoadBalancerMetricName' "HTTPCode_LB_4XX_Count"

pattern HTTPCodeLb5XXCount :: LoadBalancerMetricName
pattern HTTPCodeLb5XXCount = LoadBalancerMetricName' "HTTPCode_LB_5XX_Count"

pattern HealthyHostCount :: LoadBalancerMetricName
pattern HealthyHostCount = LoadBalancerMetricName' "HealthyHostCount"

pattern InstanceResponseTime :: LoadBalancerMetricName
pattern InstanceResponseTime = LoadBalancerMetricName' "InstanceResponseTime"

pattern RejectedConnectionCount :: LoadBalancerMetricName
pattern RejectedConnectionCount = LoadBalancerMetricName' "RejectedConnectionCount"

pattern RequestCount :: LoadBalancerMetricName
pattern RequestCount = LoadBalancerMetricName' "RequestCount"

pattern UnhealthyHostCount :: LoadBalancerMetricName
pattern UnhealthyHostCount = LoadBalancerMetricName' "UnhealthyHostCount"

{-# COMPLETE
  ClientTLSNegotiationErrorCount,
  HTTPCodeInstance2XXCount,
  HTTPCodeInstance3XXCount,
  HTTPCodeInstance4XXCount,
  HTTPCodeInstance5XXCount,
  HTTPCodeLb4XXCount,
  HTTPCodeLb5XXCount,
  HealthyHostCount,
  InstanceResponseTime,
  RejectedConnectionCount,
  RequestCount,
  UnhealthyHostCount,
  LoadBalancerMetricName'
  #-}
