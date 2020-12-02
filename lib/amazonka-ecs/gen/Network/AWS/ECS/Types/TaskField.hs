{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskField where

import Network.AWS.Prelude

data TaskField = TFTags
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

instance FromText TaskField where
  parser =
    takeLowerText >>= \case
      "tags" -> pure TFTags
      e ->
        fromTextError $
          "Failure parsing TaskField from value: '" <> e
            <> "'. Accepted values: tags"

instance ToText TaskField where
  toText = \case
    TFTags -> "TAGS"

instance Hashable TaskField

instance NFData TaskField

instance ToByteString TaskField

instance ToQuery TaskField

instance ToHeader TaskField

instance ToJSON TaskField where
  toJSON = toJSONText
