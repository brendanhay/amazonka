{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype LoadBalancerAttributeName = LoadBalancerAttributeName'
  { fromLoadBalancerAttributeName ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
