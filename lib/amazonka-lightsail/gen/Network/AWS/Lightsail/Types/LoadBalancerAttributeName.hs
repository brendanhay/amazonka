-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerAttributeName
  ( LoadBalancerAttributeName
      ( LoadBalancerAttributeName',
        HealthCheckPath,
        SessionStickinessEnabled,
        SessionStickinessLbCookieDurationSeconds
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LoadBalancerAttributeName = LoadBalancerAttributeName' Lude.Text
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

pattern HealthCheckPath :: LoadBalancerAttributeName
pattern HealthCheckPath = LoadBalancerAttributeName' "HealthCheckPath"

pattern SessionStickinessEnabled :: LoadBalancerAttributeName
pattern SessionStickinessEnabled = LoadBalancerAttributeName' "SessionStickinessEnabled"

pattern SessionStickinessLbCookieDurationSeconds :: LoadBalancerAttributeName
pattern SessionStickinessLbCookieDurationSeconds = LoadBalancerAttributeName' "SessionStickiness_LB_CookieDurationSeconds"

{-# COMPLETE
  HealthCheckPath,
  SessionStickinessEnabled,
  SessionStickinessLbCookieDurationSeconds,
  LoadBalancerAttributeName'
  #-}
