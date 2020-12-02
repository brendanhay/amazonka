{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ReplicationStatus
  = Completed
  | Failed
  | Pending
  | Replica
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

instance FromText ReplicationStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure Completed
      "failed" -> pure Failed
      "pending" -> pure Pending
      "replica" -> pure Replica
      e ->
        fromTextError $
          "Failure parsing ReplicationStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, pending, replica"

instance ToText ReplicationStatus where
  toText = \case
    Completed -> "COMPLETED"
    Failed -> "FAILED"
    Pending -> "PENDING"
    Replica -> "REPLICA"

instance Hashable ReplicationStatus

instance NFData ReplicationStatus

instance ToByteString ReplicationStatus

instance ToQuery ReplicationStatus

instance ToHeader ReplicationStatus

instance FromXML ReplicationStatus where
  parseXML = parseXMLText "ReplicationStatus"
