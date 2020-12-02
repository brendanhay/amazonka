{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryDeletionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryDeletionStatus where

import Network.AWS.Prelude

data InventoryDeletionStatus
  = IDSComplete
  | IDSInProgress
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

instance FromText InventoryDeletionStatus where
  parser =
    takeLowerText >>= \case
      "complete" -> pure IDSComplete
      "inprogress" -> pure IDSInProgress
      e ->
        fromTextError $
          "Failure parsing InventoryDeletionStatus from value: '" <> e
            <> "'. Accepted values: complete, inprogress"

instance ToText InventoryDeletionStatus where
  toText = \case
    IDSComplete -> "Complete"
    IDSInProgress -> "InProgress"

instance Hashable InventoryDeletionStatus

instance NFData InventoryDeletionStatus

instance ToByteString InventoryDeletionStatus

instance ToQuery InventoryDeletionStatus

instance ToHeader InventoryDeletionStatus

instance FromJSON InventoryDeletionStatus where
  parseJSON = parseJSONText "InventoryDeletionStatus"
