{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SnapshotState
  = SSCompleted
  | SSError'
  | SSPending
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

instance FromText SnapshotState where
  parser =
    takeLowerText >>= \case
      "completed" -> pure SSCompleted
      "error" -> pure SSError'
      "pending" -> pure SSPending
      e ->
        fromTextError $
          "Failure parsing SnapshotState from value: '" <> e
            <> "'. Accepted values: completed, error, pending"

instance ToText SnapshotState where
  toText = \case
    SSCompleted -> "completed"
    SSError' -> "error"
    SSPending -> "pending"

instance Hashable SnapshotState

instance NFData SnapshotState

instance ToByteString SnapshotState

instance ToQuery SnapshotState

instance ToHeader SnapshotState

instance FromXML SnapshotState where
  parseXML = parseXMLText "SnapshotState"
