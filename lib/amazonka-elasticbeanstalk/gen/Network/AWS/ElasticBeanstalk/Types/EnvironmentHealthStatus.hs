{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthStatus where

import Network.AWS.Prelude

data EnvironmentHealthStatus
  = EHSDegraded
  | EHSInfo
  | EHSNoData
  | EHSOK
  | EHSPending
  | EHSSevere
  | EHSSuspended
  | EHSUnknown
  | EHSWarning
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

instance FromText EnvironmentHealthStatus where
  parser =
    takeLowerText >>= \case
      "degraded" -> pure EHSDegraded
      "info" -> pure EHSInfo
      "nodata" -> pure EHSNoData
      "ok" -> pure EHSOK
      "pending" -> pure EHSPending
      "severe" -> pure EHSSevere
      "suspended" -> pure EHSSuspended
      "unknown" -> pure EHSUnknown
      "warning" -> pure EHSWarning
      e ->
        fromTextError $
          "Failure parsing EnvironmentHealthStatus from value: '" <> e
            <> "'. Accepted values: degraded, info, nodata, ok, pending, severe, suspended, unknown, warning"

instance ToText EnvironmentHealthStatus where
  toText = \case
    EHSDegraded -> "Degraded"
    EHSInfo -> "Info"
    EHSNoData -> "NoData"
    EHSOK -> "Ok"
    EHSPending -> "Pending"
    EHSSevere -> "Severe"
    EHSSuspended -> "Suspended"
    EHSUnknown -> "Unknown"
    EHSWarning -> "Warning"

instance Hashable EnvironmentHealthStatus

instance NFData EnvironmentHealthStatus

instance ToByteString EnvironmentHealthStatus

instance ToQuery EnvironmentHealthStatus

instance ToHeader EnvironmentHealthStatus

instance FromXML EnvironmentHealthStatus where
  parseXML = parseXMLText "EnvironmentHealthStatus"
