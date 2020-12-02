{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BatchState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BatchState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data BatchState
  = BSActive
  | BSCancelled
  | BSCancelledRunning
  | BSCancelledTerminating
  | BSFailed
  | BSModifying
  | BSSubmitted
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

instance FromText BatchState where
  parser =
    takeLowerText >>= \case
      "active" -> pure BSActive
      "cancelled" -> pure BSCancelled
      "cancelled_running" -> pure BSCancelledRunning
      "cancelled_terminating" -> pure BSCancelledTerminating
      "failed" -> pure BSFailed
      "modifying" -> pure BSModifying
      "submitted" -> pure BSSubmitted
      e ->
        fromTextError $
          "Failure parsing BatchState from value: '" <> e
            <> "'. Accepted values: active, cancelled, cancelled_running, cancelled_terminating, failed, modifying, submitted"

instance ToText BatchState where
  toText = \case
    BSActive -> "active"
    BSCancelled -> "cancelled"
    BSCancelledRunning -> "cancelled_running"
    BSCancelledTerminating -> "cancelled_terminating"
    BSFailed -> "failed"
    BSModifying -> "modifying"
    BSSubmitted -> "submitted"

instance Hashable BatchState

instance NFData BatchState

instance ToByteString BatchState

instance ToQuery BatchState

instance ToHeader BatchState

instance FromXML BatchState where
  parseXML = parseXMLText "BatchState"
