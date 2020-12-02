{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroupSortByType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroupSortByType where

import Network.AWS.Prelude

data ReportGroupSortByType
  = RGSBTCreatedTime
  | RGSBTLastModifiedTime
  | RGSBTName
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

instance FromText ReportGroupSortByType where
  parser =
    takeLowerText >>= \case
      "created_time" -> pure RGSBTCreatedTime
      "last_modified_time" -> pure RGSBTLastModifiedTime
      "name" -> pure RGSBTName
      e ->
        fromTextError $
          "Failure parsing ReportGroupSortByType from value: '" <> e
            <> "'. Accepted values: created_time, last_modified_time, name"

instance ToText ReportGroupSortByType where
  toText = \case
    RGSBTCreatedTime -> "CREATED_TIME"
    RGSBTLastModifiedTime -> "LAST_MODIFIED_TIME"
    RGSBTName -> "NAME"

instance Hashable ReportGroupSortByType

instance NFData ReportGroupSortByType

instance ToByteString ReportGroupSortByType

instance ToQuery ReportGroupSortByType

instance ToHeader ReportGroupSortByType

instance ToJSON ReportGroupSortByType where
  toJSON = toJSONText
