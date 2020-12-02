{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeUpdateStatus where

import Network.AWS.Prelude

data NodeUpdateStatus
  = NUSComplete
  | NUSInProgress
  | NUSNotApplied
  | NUSStopped
  | NUSStopping
  | NUSWaitingToStart
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

instance FromText NodeUpdateStatus where
  parser =
    takeLowerText >>= \case
      "complete" -> pure NUSComplete
      "in-progress" -> pure NUSInProgress
      "not-applied" -> pure NUSNotApplied
      "stopped" -> pure NUSStopped
      "stopping" -> pure NUSStopping
      "waiting-to-start" -> pure NUSWaitingToStart
      e ->
        fromTextError $
          "Failure parsing NodeUpdateStatus from value: '" <> e
            <> "'. Accepted values: complete, in-progress, not-applied, stopped, stopping, waiting-to-start"

instance ToText NodeUpdateStatus where
  toText = \case
    NUSComplete -> "complete"
    NUSInProgress -> "in-progress"
    NUSNotApplied -> "not-applied"
    NUSStopped -> "stopped"
    NUSStopping -> "stopping"
    NUSWaitingToStart -> "waiting-to-start"

instance Hashable NodeUpdateStatus

instance NFData NodeUpdateStatus

instance ToByteString NodeUpdateStatus

instance ToQuery NodeUpdateStatus

instance ToHeader NodeUpdateStatus

instance FromXML NodeUpdateStatus where
  parseXML = parseXMLText "NodeUpdateStatus"
