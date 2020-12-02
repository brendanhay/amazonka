{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetStateCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FleetStateCode
  = FSCActive
  | FSCDeleted
  | FSCDeletedRunning
  | FSCDeletedTerminating
  | FSCFailed
  | FSCModifying
  | FSCSubmitted
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

instance FromText FleetStateCode where
  parser =
    takeLowerText >>= \case
      "active" -> pure FSCActive
      "deleted" -> pure FSCDeleted
      "deleted_running" -> pure FSCDeletedRunning
      "deleted_terminating" -> pure FSCDeletedTerminating
      "failed" -> pure FSCFailed
      "modifying" -> pure FSCModifying
      "submitted" -> pure FSCSubmitted
      e ->
        fromTextError $
          "Failure parsing FleetStateCode from value: '" <> e
            <> "'. Accepted values: active, deleted, deleted_running, deleted_terminating, failed, modifying, submitted"

instance ToText FleetStateCode where
  toText = \case
    FSCActive -> "active"
    FSCDeleted -> "deleted"
    FSCDeletedRunning -> "deleted_running"
    FSCDeletedTerminating -> "deleted_terminating"
    FSCFailed -> "failed"
    FSCModifying -> "modifying"
    FSCSubmitted -> "submitted"

instance Hashable FleetStateCode

instance NFData FleetStateCode

instance ToByteString FleetStateCode

instance ToQuery FleetStateCode

instance ToHeader FleetStateCode

instance FromXML FleetStateCode where
  parseXML = parseXMLText "FleetStateCode"
