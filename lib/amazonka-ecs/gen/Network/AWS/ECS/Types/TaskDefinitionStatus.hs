{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskDefinitionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskDefinitionStatus where

import Network.AWS.Prelude

data TaskDefinitionStatus
  = TDSActive
  | TDSInactive
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

instance FromText TaskDefinitionStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure TDSActive
      "inactive" -> pure TDSInactive
      e ->
        fromTextError $
          "Failure parsing TaskDefinitionStatus from value: '" <> e
            <> "'. Accepted values: active, inactive"

instance ToText TaskDefinitionStatus where
  toText = \case
    TDSActive -> "ACTIVE"
    TDSInactive -> "INACTIVE"

instance Hashable TaskDefinitionStatus

instance NFData TaskDefinitionStatus

instance ToByteString TaskDefinitionStatus

instance ToQuery TaskDefinitionStatus

instance ToHeader TaskDefinitionStatus

instance ToJSON TaskDefinitionStatus where
  toJSON = toJSONText

instance FromJSON TaskDefinitionStatus where
  parseJSON = parseJSONText "TaskDefinitionStatus"
