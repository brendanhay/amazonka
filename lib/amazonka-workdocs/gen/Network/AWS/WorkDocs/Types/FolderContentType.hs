{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.FolderContentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.FolderContentType where

import Network.AWS.Prelude

data FolderContentType
  = FCTAll
  | FCTDocument
  | FCTFolder
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

instance FromText FolderContentType where
  parser =
    takeLowerText >>= \case
      "all" -> pure FCTAll
      "document" -> pure FCTDocument
      "folder" -> pure FCTFolder
      e ->
        fromTextError $
          "Failure parsing FolderContentType from value: '" <> e
            <> "'. Accepted values: all, document, folder"

instance ToText FolderContentType where
  toText = \case
    FCTAll -> "ALL"
    FCTDocument -> "DOCUMENT"
    FCTFolder -> "FOLDER"

instance Hashable FolderContentType

instance NFData FolderContentType

instance ToByteString FolderContentType

instance ToQuery FolderContentType

instance ToHeader FolderContentType

instance ToJSON FolderContentType where
  toJSON = toJSONText
