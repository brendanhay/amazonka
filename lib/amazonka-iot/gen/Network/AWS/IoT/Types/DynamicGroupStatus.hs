{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DynamicGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DynamicGroupStatus where

import Network.AWS.Prelude

data DynamicGroupStatus
  = DGSActive
  | DGSBuilding
  | DGSRebuilding
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

instance FromText DynamicGroupStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure DGSActive
      "building" -> pure DGSBuilding
      "rebuilding" -> pure DGSRebuilding
      e ->
        fromTextError $
          "Failure parsing DynamicGroupStatus from value: '" <> e
            <> "'. Accepted values: active, building, rebuilding"

instance ToText DynamicGroupStatus where
  toText = \case
    DGSActive -> "ACTIVE"
    DGSBuilding -> "BUILDING"
    DGSRebuilding -> "REBUILDING"

instance Hashable DynamicGroupStatus

instance NFData DynamicGroupStatus

instance ToByteString DynamicGroupStatus

instance ToQuery DynamicGroupStatus

instance ToHeader DynamicGroupStatus

instance FromJSON DynamicGroupStatus where
  parseJSON = parseJSONText "DynamicGroupStatus"
