{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentThumbnailType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.DocumentThumbnailType where

import Network.AWS.Prelude

data DocumentThumbnailType
  = Large
  | Small
  | SmallHq
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

instance FromText DocumentThumbnailType where
  parser =
    takeLowerText >>= \case
      "large" -> pure Large
      "small" -> pure Small
      "small_hq" -> pure SmallHq
      e ->
        fromTextError $
          "Failure parsing DocumentThumbnailType from value: '" <> e
            <> "'. Accepted values: large, small, small_hq"

instance ToText DocumentThumbnailType where
  toText = \case
    Large -> "LARGE"
    Small -> "SMALL"
    SmallHq -> "SMALL_HQ"

instance Hashable DocumentThumbnailType

instance NFData DocumentThumbnailType

instance ToByteString DocumentThumbnailType

instance ToQuery DocumentThumbnailType

instance ToHeader DocumentThumbnailType

instance FromJSON DocumentThumbnailType where
  parseJSON = parseJSONText "DocumentThumbnailType"
