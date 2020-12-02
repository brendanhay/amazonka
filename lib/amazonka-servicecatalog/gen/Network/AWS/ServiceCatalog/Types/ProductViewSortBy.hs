{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProductViewSortBy where

import Network.AWS.Prelude

data ProductViewSortBy
  = CreationDate
  | Title
  | VersionCount
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

instance FromText ProductViewSortBy where
  parser =
    takeLowerText >>= \case
      "creationdate" -> pure CreationDate
      "title" -> pure Title
      "versioncount" -> pure VersionCount
      e ->
        fromTextError $
          "Failure parsing ProductViewSortBy from value: '" <> e
            <> "'. Accepted values: creationdate, title, versioncount"

instance ToText ProductViewSortBy where
  toText = \case
    CreationDate -> "CreationDate"
    Title -> "Title"
    VersionCount -> "VersionCount"

instance Hashable ProductViewSortBy

instance NFData ProductViewSortBy

instance ToByteString ProductViewSortBy

instance ToQuery ProductViewSortBy

instance ToHeader ProductViewSortBy

instance ToJSON ProductViewSortBy where
  toJSON = toJSONText
