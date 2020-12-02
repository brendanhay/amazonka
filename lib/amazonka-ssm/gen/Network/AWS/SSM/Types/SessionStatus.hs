{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SessionStatus where

import Network.AWS.Prelude

data SessionStatus
  = SSConnected
  | SSConnecting
  | SSDisconnected
  | SSFailed
  | SSTerminated
  | SSTerminating
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

instance FromText SessionStatus where
  parser =
    takeLowerText >>= \case
      "connected" -> pure SSConnected
      "connecting" -> pure SSConnecting
      "disconnected" -> pure SSDisconnected
      "failed" -> pure SSFailed
      "terminated" -> pure SSTerminated
      "terminating" -> pure SSTerminating
      e ->
        fromTextError $
          "Failure parsing SessionStatus from value: '" <> e
            <> "'. Accepted values: connected, connecting, disconnected, failed, terminated, terminating"

instance ToText SessionStatus where
  toText = \case
    SSConnected -> "Connected"
    SSConnecting -> "Connecting"
    SSDisconnected -> "Disconnected"
    SSFailed -> "Failed"
    SSTerminated -> "Terminated"
    SSTerminating -> "Terminating"

instance Hashable SessionStatus

instance NFData SessionStatus

instance ToByteString SessionStatus

instance ToQuery SessionStatus

instance ToHeader SessionStatus

instance FromJSON SessionStatus where
  parseJSON = parseJSONText "SessionStatus"
