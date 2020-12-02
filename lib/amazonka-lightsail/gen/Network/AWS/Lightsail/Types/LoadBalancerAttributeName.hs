{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerAttributeName where

import Network.AWS.Prelude

data LoadBalancerAttributeName
  = HealthCheckPath
  | SessionStickinessEnabled
  | SessionStickinessLbCookieDurationSeconds
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

instance FromText LoadBalancerAttributeName where
  parser =
    takeLowerText >>= \case
      "healthcheckpath" -> pure HealthCheckPath
      "sessionstickinessenabled" -> pure SessionStickinessEnabled
      "sessionstickiness_lb_cookiedurationseconds" -> pure SessionStickinessLbCookieDurationSeconds
      e ->
        fromTextError $
          "Failure parsing LoadBalancerAttributeName from value: '" <> e
            <> "'. Accepted values: healthcheckpath, sessionstickinessenabled, sessionstickiness_lb_cookiedurationseconds"

instance ToText LoadBalancerAttributeName where
  toText = \case
    HealthCheckPath -> "HealthCheckPath"
    SessionStickinessEnabled -> "SessionStickinessEnabled"
    SessionStickinessLbCookieDurationSeconds -> "SessionStickiness_LB_CookieDurationSeconds"

instance Hashable LoadBalancerAttributeName

instance NFData LoadBalancerAttributeName

instance ToByteString LoadBalancerAttributeName

instance ToQuery LoadBalancerAttributeName

instance ToHeader LoadBalancerAttributeName

instance ToJSON LoadBalancerAttributeName where
  toJSON = toJSONText

instance FromJSON LoadBalancerAttributeName where
  parseJSON = parseJSONText "LoadBalancerAttributeName"
