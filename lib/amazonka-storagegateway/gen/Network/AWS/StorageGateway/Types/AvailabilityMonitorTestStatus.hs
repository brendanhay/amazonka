{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.AvailabilityMonitorTestStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.AvailabilityMonitorTestStatus where

import Network.AWS.Prelude

data AvailabilityMonitorTestStatus
  = Complete
  | Failed
  | Pending
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

instance FromText AvailabilityMonitorTestStatus where
  parser =
    takeLowerText >>= \case
      "complete" -> pure Complete
      "failed" -> pure Failed
      "pending" -> pure Pending
      e ->
        fromTextError $
          "Failure parsing AvailabilityMonitorTestStatus from value: '" <> e
            <> "'. Accepted values: complete, failed, pending"

instance ToText AvailabilityMonitorTestStatus where
  toText = \case
    Complete -> "COMPLETE"
    Failed -> "FAILED"
    Pending -> "PENDING"

instance Hashable AvailabilityMonitorTestStatus

instance NFData AvailabilityMonitorTestStatus

instance ToByteString AvailabilityMonitorTestStatus

instance ToQuery AvailabilityMonitorTestStatus

instance ToHeader AvailabilityMonitorTestStatus

instance FromJSON AvailabilityMonitorTestStatus where
  parseJSON = parseJSONText "AvailabilityMonitorTestStatus"
