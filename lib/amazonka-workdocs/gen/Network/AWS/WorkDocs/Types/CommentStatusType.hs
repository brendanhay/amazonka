{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.CommentStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.CommentStatusType where

import Network.AWS.Prelude

data CommentStatusType
  = Deleted
  | Draft
  | Published
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

instance FromText CommentStatusType where
  parser =
    takeLowerText >>= \case
      "deleted" -> pure Deleted
      "draft" -> pure Draft
      "published" -> pure Published
      e ->
        fromTextError $
          "Failure parsing CommentStatusType from value: '" <> e
            <> "'. Accepted values: deleted, draft, published"

instance ToText CommentStatusType where
  toText = \case
    Deleted -> "DELETED"
    Draft -> "DRAFT"
    Published -> "PUBLISHED"

instance Hashable CommentStatusType

instance NFData CommentStatusType

instance ToByteString CommentStatusType

instance ToQuery CommentStatusType

instance ToHeader CommentStatusType

instance FromJSON CommentStatusType where
  parseJSON = parseJSONText "CommentStatusType"
