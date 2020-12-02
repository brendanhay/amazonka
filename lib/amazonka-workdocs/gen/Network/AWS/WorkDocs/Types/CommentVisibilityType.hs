{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.CommentVisibilityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.CommentVisibilityType where

import Network.AWS.Prelude

data CommentVisibilityType
  = Private
  | Public
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

instance FromText CommentVisibilityType where
  parser =
    takeLowerText >>= \case
      "private" -> pure Private
      "public" -> pure Public
      e ->
        fromTextError $
          "Failure parsing CommentVisibilityType from value: '" <> e
            <> "'. Accepted values: private, public"

instance ToText CommentVisibilityType where
  toText = \case
    Private -> "PRIVATE"
    Public -> "PUBLIC"

instance Hashable CommentVisibilityType

instance NFData CommentVisibilityType

instance ToByteString CommentVisibilityType

instance ToQuery CommentVisibilityType

instance ToHeader CommentVisibilityType

instance ToJSON CommentVisibilityType where
  toJSON = toJSONText

instance FromJSON CommentVisibilityType where
  parseJSON = parseJSONText "CommentVisibilityType"
