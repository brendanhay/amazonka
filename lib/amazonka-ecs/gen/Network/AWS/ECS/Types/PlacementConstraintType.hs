{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlacementConstraintType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PlacementConstraintType where

import Network.AWS.Prelude

data PlacementConstraintType
  = PCTDistinctInstance
  | PCTMemberOf
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

instance FromText PlacementConstraintType where
  parser =
    takeLowerText >>= \case
      "distinctinstance" -> pure PCTDistinctInstance
      "memberof" -> pure PCTMemberOf
      e ->
        fromTextError $
          "Failure parsing PlacementConstraintType from value: '" <> e
            <> "'. Accepted values: distinctinstance, memberof"

instance ToText PlacementConstraintType where
  toText = \case
    PCTDistinctInstance -> "distinctInstance"
    PCTMemberOf -> "memberOf"

instance Hashable PlacementConstraintType

instance NFData PlacementConstraintType

instance ToByteString PlacementConstraintType

instance ToQuery PlacementConstraintType

instance ToHeader PlacementConstraintType

instance ToJSON PlacementConstraintType where
  toJSON = toJSONText

instance FromJSON PlacementConstraintType where
  parseJSON = parseJSONText "PlacementConstraintType"
