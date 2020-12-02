{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.GroupByAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.GroupByAttribute where

import Network.AWS.Prelude

data GroupByAttribute
  = Region
  | ResourceType
  | TargetId
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

instance FromText GroupByAttribute where
  parser =
    takeLowerText >>= \case
      "region" -> pure Region
      "resource_type" -> pure ResourceType
      "target_id" -> pure TargetId
      e ->
        fromTextError $
          "Failure parsing GroupByAttribute from value: '" <> e
            <> "'. Accepted values: region, resource_type, target_id"

instance ToText GroupByAttribute where
  toText = \case
    Region -> "REGION"
    ResourceType -> "RESOURCE_TYPE"
    TargetId -> "TARGET_ID"

instance Hashable GroupByAttribute

instance NFData GroupByAttribute

instance ToByteString GroupByAttribute

instance ToQuery GroupByAttribute

instance ToHeader GroupByAttribute

instance ToJSON GroupByAttribute where
  toJSON = toJSONText
