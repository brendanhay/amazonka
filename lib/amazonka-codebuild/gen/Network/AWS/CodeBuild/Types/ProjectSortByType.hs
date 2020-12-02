{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectSortByType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectSortByType where

import Network.AWS.Prelude

data ProjectSortByType
  = CreatedTime
  | LastModifiedTime
  | Name
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

instance FromText ProjectSortByType where
  parser =
    takeLowerText >>= \case
      "created_time" -> pure CreatedTime
      "last_modified_time" -> pure LastModifiedTime
      "name" -> pure Name
      e ->
        fromTextError $
          "Failure parsing ProjectSortByType from value: '" <> e
            <> "'. Accepted values: created_time, last_modified_time, name"

instance ToText ProjectSortByType where
  toText = \case
    CreatedTime -> "CREATED_TIME"
    LastModifiedTime -> "LAST_MODIFIED_TIME"
    Name -> "NAME"

instance Hashable ProjectSortByType

instance NFData ProjectSortByType

instance ToByteString ProjectSortByType

instance ToQuery ProjectSortByType

instance ToHeader ProjectSortByType

instance ToJSON ProjectSortByType where
  toJSON = toJSONText
