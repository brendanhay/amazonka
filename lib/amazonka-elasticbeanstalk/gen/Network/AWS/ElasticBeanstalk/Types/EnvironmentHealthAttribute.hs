{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthAttribute where

import Network.AWS.Prelude

data EnvironmentHealthAttribute
  = EHAAll
  | EHAApplicationMetrics
  | EHACauses
  | EHAColor
  | EHAHealthStatus
  | EHAInstancesHealth
  | EHARefreshedAt
  | EHAStatus
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

instance FromText EnvironmentHealthAttribute where
  parser =
    takeLowerText >>= \case
      "all" -> pure EHAAll
      "applicationmetrics" -> pure EHAApplicationMetrics
      "causes" -> pure EHACauses
      "color" -> pure EHAColor
      "healthstatus" -> pure EHAHealthStatus
      "instanceshealth" -> pure EHAInstancesHealth
      "refreshedat" -> pure EHARefreshedAt
      "status" -> pure EHAStatus
      e ->
        fromTextError $
          "Failure parsing EnvironmentHealthAttribute from value: '" <> e
            <> "'. Accepted values: all, applicationmetrics, causes, color, healthstatus, instanceshealth, refreshedat, status"

instance ToText EnvironmentHealthAttribute where
  toText = \case
    EHAAll -> "All"
    EHAApplicationMetrics -> "ApplicationMetrics"
    EHACauses -> "Causes"
    EHAColor -> "Color"
    EHAHealthStatus -> "HealthStatus"
    EHAInstancesHealth -> "InstancesHealth"
    EHARefreshedAt -> "RefreshedAt"
    EHAStatus -> "Status"

instance Hashable EnvironmentHealthAttribute

instance NFData EnvironmentHealthAttribute

instance ToByteString EnvironmentHealthAttribute

instance ToQuery EnvironmentHealthAttribute

instance ToHeader EnvironmentHealthAttribute
