{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskSetField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskSetField where

import Network.AWS.Prelude

data TaskSetField = TSFTags
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

instance FromText TaskSetField where
  parser =
    takeLowerText >>= \case
      "tags" -> pure TSFTags
      e ->
        fromTextError $
          "Failure parsing TaskSetField from value: '" <> e
            <> "'. Accepted values: tags"

instance ToText TaskSetField where
  toText = \case
    TSFTags -> "TAGS"

instance Hashable TaskSetField

instance NFData TaskSetField

instance ToByteString TaskSetField

instance ToQuery TaskSetField

instance ToHeader TaskSetField

instance ToJSON TaskSetField where
  toJSON = toJSONText
