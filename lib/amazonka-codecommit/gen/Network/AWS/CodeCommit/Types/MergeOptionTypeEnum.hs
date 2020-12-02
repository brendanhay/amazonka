{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.MergeOptionTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeOptionTypeEnum where

import Network.AWS.Prelude

data MergeOptionTypeEnum
  = FastForwardMerge
  | SquashMerge
  | ThreeWayMerge
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

instance FromText MergeOptionTypeEnum where
  parser =
    takeLowerText >>= \case
      "fast_forward_merge" -> pure FastForwardMerge
      "squash_merge" -> pure SquashMerge
      "three_way_merge" -> pure ThreeWayMerge
      e ->
        fromTextError $
          "Failure parsing MergeOptionTypeEnum from value: '" <> e
            <> "'. Accepted values: fast_forward_merge, squash_merge, three_way_merge"

instance ToText MergeOptionTypeEnum where
  toText = \case
    FastForwardMerge -> "FAST_FORWARD_MERGE"
    SquashMerge -> "SQUASH_MERGE"
    ThreeWayMerge -> "THREE_WAY_MERGE"

instance Hashable MergeOptionTypeEnum

instance NFData MergeOptionTypeEnum

instance ToByteString MergeOptionTypeEnum

instance ToQuery MergeOptionTypeEnum

instance ToHeader MergeOptionTypeEnum

instance ToJSON MergeOptionTypeEnum where
  toJSON = toJSONText

instance FromJSON MergeOptionTypeEnum where
  parseJSON = parseJSONText "MergeOptionTypeEnum"
