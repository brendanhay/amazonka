{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskDefinitionPlacementConstraintType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskDefinitionPlacementConstraintType where

import Network.AWS.Prelude

data TaskDefinitionPlacementConstraintType = MemberOf
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

instance FromText TaskDefinitionPlacementConstraintType where
  parser =
    takeLowerText >>= \case
      "memberof" -> pure MemberOf
      e ->
        fromTextError $
          "Failure parsing TaskDefinitionPlacementConstraintType from value: '" <> e
            <> "'. Accepted values: memberof"

instance ToText TaskDefinitionPlacementConstraintType where
  toText = \case
    MemberOf -> "memberOf"

instance Hashable TaskDefinitionPlacementConstraintType

instance NFData TaskDefinitionPlacementConstraintType

instance ToByteString TaskDefinitionPlacementConstraintType

instance ToQuery TaskDefinitionPlacementConstraintType

instance ToHeader TaskDefinitionPlacementConstraintType

instance ToJSON TaskDefinitionPlacementConstraintType where
  toJSON = toJSONText

instance FromJSON TaskDefinitionPlacementConstraintType where
  parseJSON = parseJSONText "TaskDefinitionPlacementConstraintType"
