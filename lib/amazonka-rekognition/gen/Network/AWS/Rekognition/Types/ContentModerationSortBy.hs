{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ContentModerationSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ContentModerationSortBy where

import Network.AWS.Prelude

data ContentModerationSortBy
  = CMSBName
  | CMSBTimestamp
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

instance FromText ContentModerationSortBy where
  parser =
    takeLowerText >>= \case
      "name" -> pure CMSBName
      "timestamp" -> pure CMSBTimestamp
      e ->
        fromTextError $
          "Failure parsing ContentModerationSortBy from value: '" <> e
            <> "'. Accepted values: name, timestamp"

instance ToText ContentModerationSortBy where
  toText = \case
    CMSBName -> "NAME"
    CMSBTimestamp -> "TIMESTAMP"

instance Hashable ContentModerationSortBy

instance NFData ContentModerationSortBy

instance ToByteString ContentModerationSortBy

instance ToQuery ContentModerationSortBy

instance ToHeader ContentModerationSortBy

instance ToJSON ContentModerationSortBy where
  toJSON = toJSONText
