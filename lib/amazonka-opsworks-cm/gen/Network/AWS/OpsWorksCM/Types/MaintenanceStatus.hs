{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.MaintenanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.MaintenanceStatus where

import Network.AWS.Prelude

data MaintenanceStatus
  = MSFailed
  | MSSuccess
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

instance FromText MaintenanceStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure MSFailed
      "success" -> pure MSSuccess
      e ->
        fromTextError $
          "Failure parsing MaintenanceStatus from value: '" <> e
            <> "'. Accepted values: failed, success"

instance ToText MaintenanceStatus where
  toText = \case
    MSFailed -> "FAILED"
    MSSuccess -> "SUCCESS"

instance Hashable MaintenanceStatus

instance NFData MaintenanceStatus

instance ToByteString MaintenanceStatus

instance ToQuery MaintenanceStatus

instance ToHeader MaintenanceStatus

instance FromJSON MaintenanceStatus where
  parseJSON = parseJSONText "MaintenanceStatus"
