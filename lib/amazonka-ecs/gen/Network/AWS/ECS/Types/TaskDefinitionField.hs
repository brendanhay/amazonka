{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskDefinitionField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskDefinitionField where

import Network.AWS.Prelude

data TaskDefinitionField = TDFTags
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

instance FromText TaskDefinitionField where
  parser =
    takeLowerText >>= \case
      "tags" -> pure TDFTags
      e ->
        fromTextError $
          "Failure parsing TaskDefinitionField from value: '" <> e
            <> "'. Accepted values: tags"

instance ToText TaskDefinitionField where
  toText = \case
    TDFTags -> "TAGS"

instance Hashable TaskDefinitionField

instance NFData TaskDefinitionField

instance ToByteString TaskDefinitionField

instance ToQuery TaskDefinitionField

instance ToHeader TaskDefinitionField

instance ToJSON TaskDefinitionField where
  toJSON = toJSONText
