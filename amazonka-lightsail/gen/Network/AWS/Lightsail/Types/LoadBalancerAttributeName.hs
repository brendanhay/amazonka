{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerAttributeName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerAttributeName
  ( LoadBalancerAttributeName
      ( ..,
        LoadBalancerAttributeName_HealthCheckPath,
        LoadBalancerAttributeName_SessionStickinessEnabled,
        LoadBalancerAttributeName_SessionStickiness_LB_CookieDurationSeconds
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LoadBalancerAttributeName = LoadBalancerAttributeName'
  { fromLoadBalancerAttributeName ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern LoadBalancerAttributeName_HealthCheckPath :: LoadBalancerAttributeName
pattern LoadBalancerAttributeName_HealthCheckPath = LoadBalancerAttributeName' "HealthCheckPath"

pattern LoadBalancerAttributeName_SessionStickinessEnabled :: LoadBalancerAttributeName
pattern LoadBalancerAttributeName_SessionStickinessEnabled = LoadBalancerAttributeName' "SessionStickinessEnabled"

pattern LoadBalancerAttributeName_SessionStickiness_LB_CookieDurationSeconds :: LoadBalancerAttributeName
pattern LoadBalancerAttributeName_SessionStickiness_LB_CookieDurationSeconds = LoadBalancerAttributeName' "SessionStickiness_LB_CookieDurationSeconds"

{-# COMPLETE
  LoadBalancerAttributeName_HealthCheckPath,
  LoadBalancerAttributeName_SessionStickinessEnabled,
  LoadBalancerAttributeName_SessionStickiness_LB_CookieDurationSeconds,
  LoadBalancerAttributeName'
  #-}
