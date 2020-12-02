{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.PoolStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.PoolStatus where

import Network.AWS.Prelude

data PoolStatus
  = Active
  | Deleted
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

instance FromText PoolStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "deleted" -> pure Deleted
      e ->
        fromTextError $
          "Failure parsing PoolStatus from value: '" <> e
            <> "'. Accepted values: active, deleted"

instance ToText PoolStatus where
  toText = \case
    Active -> "ACTIVE"
    Deleted -> "DELETED"

instance Hashable PoolStatus

instance NFData PoolStatus

instance ToByteString PoolStatus

instance ToQuery PoolStatus

instance ToHeader PoolStatus

instance FromJSON PoolStatus where
  parseJSON = parseJSONText "PoolStatus"
