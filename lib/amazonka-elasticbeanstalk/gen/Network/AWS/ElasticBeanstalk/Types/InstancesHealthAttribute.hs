{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.InstancesHealthAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.InstancesHealthAttribute where

import Network.AWS.Prelude

data InstancesHealthAttribute
  = All
  | ApplicationMetrics
  | AvailabilityZone
  | Causes
  | Color
  | Deployment
  | HealthStatus
  | InstanceType
  | LaunchedAt
  | RefreshedAt
  | System
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

instance FromText InstancesHealthAttribute where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "applicationmetrics" -> pure ApplicationMetrics
      "availabilityzone" -> pure AvailabilityZone
      "causes" -> pure Causes
      "color" -> pure Color
      "deployment" -> pure Deployment
      "healthstatus" -> pure HealthStatus
      "instancetype" -> pure InstanceType
      "launchedat" -> pure LaunchedAt
      "refreshedat" -> pure RefreshedAt
      "system" -> pure System
      e ->
        fromTextError $
          "Failure parsing InstancesHealthAttribute from value: '" <> e
            <> "'. Accepted values: all, applicationmetrics, availabilityzone, causes, color, deployment, healthstatus, instancetype, launchedat, refreshedat, system"

instance ToText InstancesHealthAttribute where
  toText = \case
    All -> "All"
    ApplicationMetrics -> "ApplicationMetrics"
    AvailabilityZone -> "AvailabilityZone"
    Causes -> "Causes"
    Color -> "Color"
    Deployment -> "Deployment"
    HealthStatus -> "HealthStatus"
    InstanceType -> "InstanceType"
    LaunchedAt -> "LaunchedAt"
    RefreshedAt -> "RefreshedAt"
    System -> "System"

instance Hashable InstancesHealthAttribute

instance NFData InstancesHealthAttribute

instance ToByteString InstancesHealthAttribute

instance ToQuery InstancesHealthAttribute

instance ToHeader InstancesHealthAttribute
