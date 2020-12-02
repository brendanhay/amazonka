{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStateName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStateName where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceStateName
  = ISNPending
  | ISNRunning
  | ISNShuttingDown
  | ISNStopped
  | ISNStopping
  | ISNTerminated
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

instance FromText InstanceStateName where
  parser =
    takeLowerText >>= \case
      "pending" -> pure ISNPending
      "running" -> pure ISNRunning
      "shutting-down" -> pure ISNShuttingDown
      "stopped" -> pure ISNStopped
      "stopping" -> pure ISNStopping
      "terminated" -> pure ISNTerminated
      e ->
        fromTextError $
          "Failure parsing InstanceStateName from value: '" <> e
            <> "'. Accepted values: pending, running, shutting-down, stopped, stopping, terminated"

instance ToText InstanceStateName where
  toText = \case
    ISNPending -> "pending"
    ISNRunning -> "running"
    ISNShuttingDown -> "shutting-down"
    ISNStopped -> "stopped"
    ISNStopping -> "stopping"
    ISNTerminated -> "terminated"

instance Hashable InstanceStateName

instance NFData InstanceStateName

instance ToByteString InstanceStateName

instance ToQuery InstanceStateName

instance ToHeader InstanceStateName

instance FromXML InstanceStateName where
  parseXML = parseXMLText "InstanceStateName"
