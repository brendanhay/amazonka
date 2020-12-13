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
        LBMNClientTLSNegotiationErrorCount,
        LBMNHealthyHostCount,
        LBMNUnhealthyHostCount,
        LBMNHTTPCodeLb4XXCount,
        LBMNHTTPCodeLb5XXCount,
        LBMNHTTPCodeInstance2XXCount,
        LBMNHTTPCodeInstance3XXCount,
        LBMNHTTPCodeInstance4XXCount,
        LBMNHTTPCodeInstance5XXCount,
        LBMNInstanceResponseTime,
        LBMNRejectedConnectionCount,
        LBMNRequestCount
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

pattern LBMNClientTLSNegotiationErrorCount :: LoadBalancerMetricName
pattern LBMNClientTLSNegotiationErrorCount = LoadBalancerMetricName' "ClientTLSNegotiationErrorCount"

pattern LBMNHealthyHostCount :: LoadBalancerMetricName
pattern LBMNHealthyHostCount = LoadBalancerMetricName' "HealthyHostCount"

pattern LBMNUnhealthyHostCount :: LoadBalancerMetricName
pattern LBMNUnhealthyHostCount = LoadBalancerMetricName' "UnhealthyHostCount"

pattern LBMNHTTPCodeLb4XXCount :: LoadBalancerMetricName
pattern LBMNHTTPCodeLb4XXCount = LoadBalancerMetricName' "HTTPCode_LB_4XX_Count"

pattern LBMNHTTPCodeLb5XXCount :: LoadBalancerMetricName
pattern LBMNHTTPCodeLb5XXCount = LoadBalancerMetricName' "HTTPCode_LB_5XX_Count"

pattern LBMNHTTPCodeInstance2XXCount :: LoadBalancerMetricName
pattern LBMNHTTPCodeInstance2XXCount = LoadBalancerMetricName' "HTTPCode_Instance_2XX_Count"

pattern LBMNHTTPCodeInstance3XXCount :: LoadBalancerMetricName
pattern LBMNHTTPCodeInstance3XXCount = LoadBalancerMetricName' "HTTPCode_Instance_3XX_Count"

pattern LBMNHTTPCodeInstance4XXCount :: LoadBalancerMetricName
pattern LBMNHTTPCodeInstance4XXCount = LoadBalancerMetricName' "HTTPCode_Instance_4XX_Count"

pattern LBMNHTTPCodeInstance5XXCount :: LoadBalancerMetricName
pattern LBMNHTTPCodeInstance5XXCount = LoadBalancerMetricName' "HTTPCode_Instance_5XX_Count"

pattern LBMNInstanceResponseTime :: LoadBalancerMetricName
pattern LBMNInstanceResponseTime = LoadBalancerMetricName' "InstanceResponseTime"

pattern LBMNRejectedConnectionCount :: LoadBalancerMetricName
pattern LBMNRejectedConnectionCount = LoadBalancerMetricName' "RejectedConnectionCount"

pattern LBMNRequestCount :: LoadBalancerMetricName
pattern LBMNRequestCount = LoadBalancerMetricName' "RequestCount"

{-# COMPLETE
  LBMNClientTLSNegotiationErrorCount,
  LBMNHealthyHostCount,
  LBMNUnhealthyHostCount,
  LBMNHTTPCodeLb4XXCount,
  LBMNHTTPCodeLb5XXCount,
  LBMNHTTPCodeInstance2XXCount,
  LBMNHTTPCodeInstance3XXCount,
  LBMNHTTPCodeInstance4XXCount,
  LBMNHTTPCodeInstance5XXCount,
  LBMNInstanceResponseTime,
  LBMNRejectedConnectionCount,
  LBMNRequestCount,
  LoadBalancerMetricName'
  #-}
