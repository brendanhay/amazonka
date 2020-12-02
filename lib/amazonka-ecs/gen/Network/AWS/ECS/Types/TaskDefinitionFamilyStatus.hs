{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskDefinitionFamilyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskDefinitionFamilyStatus where

import Network.AWS.Prelude

data TaskDefinitionFamilyStatus
  = TDFSActive
  | TDFSAll
  | TDFSInactive
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

instance FromText TaskDefinitionFamilyStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure TDFSActive
      "all" -> pure TDFSAll
      "inactive" -> pure TDFSInactive
      e ->
        fromTextError $
          "Failure parsing TaskDefinitionFamilyStatus from value: '" <> e
            <> "'. Accepted values: active, all, inactive"

instance ToText TaskDefinitionFamilyStatus where
  toText = \case
    TDFSActive -> "ACTIVE"
    TDFSAll -> "ALL"
    TDFSInactive -> "INACTIVE"

instance Hashable TaskDefinitionFamilyStatus

instance NFData TaskDefinitionFamilyStatus

instance ToByteString TaskDefinitionFamilyStatus

instance ToQuery TaskDefinitionFamilyStatus

instance ToHeader TaskDefinitionFamilyStatus

instance ToJSON TaskDefinitionFamilyStatus where
  toJSON = toJSONText
