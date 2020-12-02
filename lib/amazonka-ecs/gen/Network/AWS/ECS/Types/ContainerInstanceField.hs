{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerInstanceField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerInstanceField where

import Network.AWS.Prelude

data ContainerInstanceField = CIFTags
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

instance FromText ContainerInstanceField where
  parser =
    takeLowerText >>= \case
      "tags" -> pure CIFTags
      e ->
        fromTextError $
          "Failure parsing ContainerInstanceField from value: '" <> e
            <> "'. Accepted values: tags"

instance ToText ContainerInstanceField where
  toText = \case
    CIFTags -> "TAGS"

instance Hashable ContainerInstanceField

instance NFData ContainerInstanceField

instance ToByteString ContainerInstanceField

instance ToQuery ContainerInstanceField

instance ToHeader ContainerInstanceField

instance ToJSON ContainerInstanceField where
  toJSON = toJSONText
