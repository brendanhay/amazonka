{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HealthCheckType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HealthCheckType where

import Network.AWS.Prelude

data HealthCheckType
  = HTTP
  | HTTPS
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
      "http" -> pure HTTP
      "https" -> pure HTTPS
      "tcp" -> pure TCP
      e ->
        fromTextError $
          "Failure parsing HealthCheckType from value: '" <> e
            <> "'. Accepted values: http, https, tcp"

instance ToText HealthCheckType where
  toText = \case
    HTTP -> "HTTP"
    HTTPS -> "HTTPS"
    TCP -> "TCP"

instance Hashable HealthCheckType

instance NFData HealthCheckType

instance ToByteString HealthCheckType

instance ToQuery HealthCheckType

instance ToHeader HealthCheckType

instance ToJSON HealthCheckType where
  toJSON = toJSONText

instance FromJSON HealthCheckType where
  parseJSON = parseJSONText "HealthCheckType"
