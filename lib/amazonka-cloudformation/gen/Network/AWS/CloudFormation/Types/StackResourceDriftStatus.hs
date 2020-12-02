{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDriftStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDriftStatus where

import Network.AWS.Prelude

data StackResourceDriftStatus
  = SRDSDeleted
  | SRDSInSync
  | SRDSModified
  | SRDSNotChecked
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

instance FromText StackResourceDriftStatus where
  parser =
    takeLowerText >>= \case
      "deleted" -> pure SRDSDeleted
      "in_sync" -> pure SRDSInSync
      "modified" -> pure SRDSModified
      "not_checked" -> pure SRDSNotChecked
      e ->
        fromTextError $
          "Failure parsing StackResourceDriftStatus from value: '" <> e
            <> "'. Accepted values: deleted, in_sync, modified, not_checked"

instance ToText StackResourceDriftStatus where
  toText = \case
    SRDSDeleted -> "DELETED"
    SRDSInSync -> "IN_SYNC"
    SRDSModified -> "MODIFIED"
    SRDSNotChecked -> "NOT_CHECKED"

instance Hashable StackResourceDriftStatus

instance NFData StackResourceDriftStatus

instance ToByteString StackResourceDriftStatus

instance ToQuery StackResourceDriftStatus

instance ToHeader StackResourceDriftStatus

instance FromXML StackResourceDriftStatus where
  parseXML = parseXMLText "StackResourceDriftStatus"
