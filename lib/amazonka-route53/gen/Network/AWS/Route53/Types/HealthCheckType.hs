{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HealthCheckType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HealthCheckType where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data HealthCheckType
  = Calculated
  | CloudwatchMetric
  | HTTP
  | HTTPS
  | HTTPSStrMatch
  | HTTPStrMatch
  | TCP
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

instance FromText HealthCheckType where
  parser =
    takeLowerText >>= \case
      "calculated" -> pure Calculated
      "cloudwatch_metric" -> pure CloudwatchMetric
      "http" -> pure HTTP
      "https" -> pure HTTPS
      "https_str_match" -> pure HTTPSStrMatch
      "http_str_match" -> pure HTTPStrMatch
      "tcp" -> pure TCP
      e ->
        fromTextError $
          "Failure parsing HealthCheckType from value: '" <> e
            <> "'. Accepted values: calculated, cloudwatch_metric, http, https, https_str_match, http_str_match, tcp"

instance ToText HealthCheckType where
  toText = \case
    Calculated -> "CALCULATED"
    CloudwatchMetric -> "CLOUDWATCH_METRIC"
    HTTP -> "HTTP"
    HTTPS -> "HTTPS"
    HTTPSStrMatch -> "HTTPS_STR_MATCH"
    HTTPStrMatch -> "HTTP_STR_MATCH"
    TCP -> "TCP"

instance Hashable HealthCheckType

instance NFData HealthCheckType

instance ToByteString HealthCheckType

instance ToQuery HealthCheckType

instance ToHeader HealthCheckType

instance FromXML HealthCheckType where
  parseXML = parseXMLText "HealthCheckType"

instance ToXML HealthCheckType where
  toXML = toXMLText
