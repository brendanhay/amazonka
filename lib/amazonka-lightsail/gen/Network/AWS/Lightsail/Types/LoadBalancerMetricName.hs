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
        LoadBalancerMetricNameClientTLSNegotiationErrorCount,
        LoadBalancerMetricNameHealthyHostCount,
        LoadBalancerMetricNameUnhealthyHostCount,
        LoadBalancerMetricNameHTTPCodeLb4XXCount,
        LoadBalancerMetricNameHTTPCodeLb5XXCount,
        LoadBalancerMetricNameHTTPCodeInstance2XXCount,
        LoadBalancerMetricNameHTTPCodeInstance3XXCount,
        LoadBalancerMetricNameHTTPCodeInstance4XXCount,
        LoadBalancerMetricNameHTTPCodeInstance5XXCount,
        LoadBalancerMetricNameInstanceResponseTime,
        LoadBalancerMetricNameRejectedConnectionCount,
        LoadBalancerMetricNameRequestCount,
        fromLoadBalancerMetricName
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype LoadBalancerMetricName = LoadBalancerMetricName'
  { fromLoadBalancerMetricName ::
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

pattern LoadBalancerMetricNameClientTLSNegotiationErrorCount :: LoadBalancerMetricName
pattern LoadBalancerMetricNameClientTLSNegotiationErrorCount = LoadBalancerMetricName' "ClientTLSNegotiationErrorCount"

pattern LoadBalancerMetricNameHealthyHostCount :: LoadBalancerMetricName
pattern LoadBalancerMetricNameHealthyHostCount = LoadBalancerMetricName' "HealthyHostCount"

pattern LoadBalancerMetricNameUnhealthyHostCount :: LoadBalancerMetricName
pattern LoadBalancerMetricNameUnhealthyHostCount = LoadBalancerMetricName' "UnhealthyHostCount"

pattern LoadBalancerMetricNameHTTPCodeLb4XXCount :: LoadBalancerMetricName
pattern LoadBalancerMetricNameHTTPCodeLb4XXCount = LoadBalancerMetricName' "HTTPCode_LB_4XX_Count"

pattern LoadBalancerMetricNameHTTPCodeLb5XXCount :: LoadBalancerMetricName
pattern LoadBalancerMetricNameHTTPCodeLb5XXCount = LoadBalancerMetricName' "HTTPCode_LB_5XX_Count"

pattern LoadBalancerMetricNameHTTPCodeInstance2XXCount :: LoadBalancerMetricName
pattern LoadBalancerMetricNameHTTPCodeInstance2XXCount = LoadBalancerMetricName' "HTTPCode_Instance_2XX_Count"

pattern LoadBalancerMetricNameHTTPCodeInstance3XXCount :: LoadBalancerMetricName
pattern LoadBalancerMetricNameHTTPCodeInstance3XXCount = LoadBalancerMetricName' "HTTPCode_Instance_3XX_Count"

pattern LoadBalancerMetricNameHTTPCodeInstance4XXCount :: LoadBalancerMetricName
pattern LoadBalancerMetricNameHTTPCodeInstance4XXCount = LoadBalancerMetricName' "HTTPCode_Instance_4XX_Count"

pattern LoadBalancerMetricNameHTTPCodeInstance5XXCount :: LoadBalancerMetricName
pattern LoadBalancerMetricNameHTTPCodeInstance5XXCount = LoadBalancerMetricName' "HTTPCode_Instance_5XX_Count"

pattern LoadBalancerMetricNameInstanceResponseTime :: LoadBalancerMetricName
pattern LoadBalancerMetricNameInstanceResponseTime = LoadBalancerMetricName' "InstanceResponseTime"

pattern LoadBalancerMetricNameRejectedConnectionCount :: LoadBalancerMetricName
pattern LoadBalancerMetricNameRejectedConnectionCount = LoadBalancerMetricName' "RejectedConnectionCount"

pattern LoadBalancerMetricNameRequestCount :: LoadBalancerMetricName
pattern LoadBalancerMetricNameRequestCount = LoadBalancerMetricName' "RequestCount"

{-# COMPLETE
  LoadBalancerMetricNameClientTLSNegotiationErrorCount,
  LoadBalancerMetricNameHealthyHostCount,
  LoadBalancerMetricNameUnhealthyHostCount,
  LoadBalancerMetricNameHTTPCodeLb4XXCount,
  LoadBalancerMetricNameHTTPCodeLb5XXCount,
  LoadBalancerMetricNameHTTPCodeInstance2XXCount,
  LoadBalancerMetricNameHTTPCodeInstance3XXCount,
  LoadBalancerMetricNameHTTPCodeInstance4XXCount,
  LoadBalancerMetricNameHTTPCodeInstance5XXCount,
  LoadBalancerMetricNameInstanceResponseTime,
  LoadBalancerMetricNameRejectedConnectionCount,
  LoadBalancerMetricNameRequestCount,
  LoadBalancerMetricName'
  #-}
