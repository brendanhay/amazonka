{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.EntityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.EntityStatus where

import Network.AWS.Prelude

-- | Object status with the following possible values:
--
--
--     * @PENDING@     * @INPROGRESS@     * @FAILED@     * @COMPLETED@     * @DELETED@
data EntityStatus
  = ESCompleted
  | ESDeleted
  | ESFailed
  | ESInprogress
  | ESPending
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

instance FromText EntityStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure ESCompleted
      "deleted" -> pure ESDeleted
      "failed" -> pure ESFailed
      "inprogress" -> pure ESInprogress
      "pending" -> pure ESPending
      e ->
        fromTextError $
          "Failure parsing EntityStatus from value: '" <> e
            <> "'. Accepted values: completed, deleted, failed, inprogress, pending"

instance ToText EntityStatus where
  toText = \case
    ESCompleted -> "COMPLETED"
    ESDeleted -> "DELETED"
    ESFailed -> "FAILED"
    ESInprogress -> "INPROGRESS"
    ESPending -> "PENDING"

instance Hashable EntityStatus

instance NFData EntityStatus

instance ToByteString EntityStatus

instance ToQuery EntityStatus

instance ToHeader EntityStatus

instance FromJSON EntityStatus where
  parseJSON = parseJSONText "EntityStatus"
