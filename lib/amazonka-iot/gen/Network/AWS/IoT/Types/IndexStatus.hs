{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.IndexStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.IndexStatus where

import Network.AWS.Prelude

data IndexStatus
  = ISActive
  | ISBuilding
  | ISRebuilding
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

instance FromText IndexStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure ISActive
      "building" -> pure ISBuilding
      "rebuilding" -> pure ISRebuilding
      e ->
        fromTextError $
          "Failure parsing IndexStatus from value: '" <> e
            <> "'. Accepted values: active, building, rebuilding"

instance ToText IndexStatus where
  toText = \case
    ISActive -> "ACTIVE"
    ISBuilding -> "BUILDING"
    ISRebuilding -> "REBUILDING"

instance Hashable IndexStatus

instance NFData IndexStatus

instance ToByteString IndexStatus

instance ToQuery IndexStatus

instance ToHeader IndexStatus

instance FromJSON IndexStatus where
  parseJSON = parseJSONText "IndexStatus"
