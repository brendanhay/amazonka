{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.LastResourceDataSyncStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.LastResourceDataSyncStatus where

import Network.AWS.Prelude

data LastResourceDataSyncStatus
  = LRDSSFailed
  | LRDSSInProgress
  | LRDSSSuccessful
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

instance FromText LastResourceDataSyncStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure LRDSSFailed
      "inprogress" -> pure LRDSSInProgress
      "successful" -> pure LRDSSSuccessful
      e ->
        fromTextError $
          "Failure parsing LastResourceDataSyncStatus from value: '" <> e
            <> "'. Accepted values: failed, inprogress, successful"

instance ToText LastResourceDataSyncStatus where
  toText = \case
    LRDSSFailed -> "Failed"
    LRDSSInProgress -> "InProgress"
    LRDSSSuccessful -> "Successful"

instance Hashable LastResourceDataSyncStatus

instance NFData LastResourceDataSyncStatus

instance ToByteString LastResourceDataSyncStatus

instance ToQuery LastResourceDataSyncStatus

instance ToHeader LastResourceDataSyncStatus

instance FromJSON LastResourceDataSyncStatus where
  parseJSON = parseJSONText "LastResourceDataSyncStatus"
