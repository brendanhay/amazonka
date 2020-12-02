{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceSearchSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceSearchSortBy where

import Network.AWS.Prelude

data FaceSearchSortBy
  = FSSBIndex
  | FSSBTimestamp
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

instance FromText FaceSearchSortBy where
  parser =
    takeLowerText >>= \case
      "index" -> pure FSSBIndex
      "timestamp" -> pure FSSBTimestamp
      e ->
        fromTextError $
          "Failure parsing FaceSearchSortBy from value: '" <> e
            <> "'. Accepted values: index, timestamp"

instance ToText FaceSearchSortBy where
  toText = \case
    FSSBIndex -> "INDEX"
    FSSBTimestamp -> "TIMESTAMP"

instance Hashable FaceSearchSortBy

instance NFData FaceSearchSortBy

instance ToByteString FaceSearchSortBy

instance ToQuery FaceSearchSortBy

instance ToHeader FaceSearchSortBy

instance ToJSON FaceSearchSortBy where
  toJSON = toJSONText
