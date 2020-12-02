{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformSortColumnType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformSortColumnType where

import Network.AWS.Prelude

data TransformSortColumnType
  = Created
  | LastModified
  | Name
  | Status
  | TransformType
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

instance FromText TransformSortColumnType where
  parser =
    takeLowerText >>= \case
      "created" -> pure Created
      "last_modified" -> pure LastModified
      "name" -> pure Name
      "status" -> pure Status
      "transform_type" -> pure TransformType
      e ->
        fromTextError $
          "Failure parsing TransformSortColumnType from value: '" <> e
            <> "'. Accepted values: created, last_modified, name, status, transform_type"

instance ToText TransformSortColumnType where
  toText = \case
    Created -> "CREATED"
    LastModified -> "LAST_MODIFIED"
    Name -> "NAME"
    Status -> "STATUS"
    TransformType -> "TRANSFORM_TYPE"

instance Hashable TransformSortColumnType

instance NFData TransformSortColumnType

instance ToByteString TransformSortColumnType

instance ToQuery TransformSortColumnType

instance ToHeader TransformSortColumnType

instance ToJSON TransformSortColumnType where
  toJSON = toJSONText
