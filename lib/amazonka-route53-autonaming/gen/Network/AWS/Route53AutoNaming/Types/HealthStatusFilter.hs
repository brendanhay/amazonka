{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HealthStatusFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HealthStatusFilter where

import Network.AWS.Prelude

data HealthStatusFilter
  = HSFAll
  | HSFHealthy
  | HSFUnhealthy
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

instance FromText HealthStatusFilter where
  parser =
    takeLowerText >>= \case
      "all" -> pure HSFAll
      "healthy" -> pure HSFHealthy
      "unhealthy" -> pure HSFUnhealthy
      e ->
        fromTextError $
          "Failure parsing HealthStatusFilter from value: '" <> e
            <> "'. Accepted values: all, healthy, unhealthy"

instance ToText HealthStatusFilter where
  toText = \case
    HSFAll -> "ALL"
    HSFHealthy -> "HEALTHY"
    HSFUnhealthy -> "UNHEALTHY"

instance Hashable HealthStatusFilter

instance NFData HealthStatusFilter

instance ToByteString HealthStatusFilter

instance ToQuery HealthStatusFilter

instance ToHeader HealthStatusFilter

instance ToJSON HealthStatusFilter where
  toJSON = toJSONText
