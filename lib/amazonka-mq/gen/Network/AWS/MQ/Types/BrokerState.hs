{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerState where

import Network.AWS.Prelude

-- | The status of the broker.
data BrokerState
  = CreationFailed
  | CreationInProgress
  | DeletionInProgress
  | RebootInProgress
  | Running
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

instance FromText BrokerState where
  parser =
    takeLowerText >>= \case
      "creation_failed" -> pure CreationFailed
      "creation_in_progress" -> pure CreationInProgress
      "deletion_in_progress" -> pure DeletionInProgress
      "reboot_in_progress" -> pure RebootInProgress
      "running" -> pure Running
      e ->
        fromTextError $
          "Failure parsing BrokerState from value: '" <> e
            <> "'. Accepted values: creation_failed, creation_in_progress, deletion_in_progress, reboot_in_progress, running"

instance ToText BrokerState where
  toText = \case
    CreationFailed -> "CREATION_FAILED"
    CreationInProgress -> "CREATION_IN_PROGRESS"
    DeletionInProgress -> "DELETION_IN_PROGRESS"
    RebootInProgress -> "REBOOT_IN_PROGRESS"
    Running -> "RUNNING"

instance Hashable BrokerState

instance NFData BrokerState

instance ToByteString BrokerState

instance ToQuery BrokerState

instance ToHeader BrokerState

instance FromJSON BrokerState where
  parseJSON = parseJSONText "BrokerState"
