{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.IPSetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.IPSetStatus where

import Network.AWS.Prelude

data IPSetStatus
  = ISSActivating
  | ISSActive
  | ISSDeactivating
  | ISSDeletePending
  | ISSDeleted
  | ISSError'
  | ISSInactive
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

instance FromText IPSetStatus where
  parser =
    takeLowerText >>= \case
      "activating" -> pure ISSActivating
      "active" -> pure ISSActive
      "deactivating" -> pure ISSDeactivating
      "delete_pending" -> pure ISSDeletePending
      "deleted" -> pure ISSDeleted
      "error" -> pure ISSError'
      "inactive" -> pure ISSInactive
      e ->
        fromTextError $
          "Failure parsing IPSetStatus from value: '" <> e
            <> "'. Accepted values: activating, active, deactivating, delete_pending, deleted, error, inactive"

instance ToText IPSetStatus where
  toText = \case
    ISSActivating -> "ACTIVATING"
    ISSActive -> "ACTIVE"
    ISSDeactivating -> "DEACTIVATING"
    ISSDeletePending -> "DELETE_PENDING"
    ISSDeleted -> "DELETED"
    ISSError' -> "ERROR"
    ISSInactive -> "INACTIVE"

instance Hashable IPSetStatus

instance NFData IPSetStatus

instance ToByteString IPSetStatus

instance ToQuery IPSetStatus

instance ToHeader IPSetStatus

instance FromJSON IPSetStatus where
  parseJSON = parseJSONText "IPSetStatus"
