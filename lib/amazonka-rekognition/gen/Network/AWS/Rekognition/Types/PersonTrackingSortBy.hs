{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.PersonTrackingSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.PersonTrackingSortBy where

import Network.AWS.Prelude

data PersonTrackingSortBy
  = Index
  | Timestamp
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

instance FromText PersonTrackingSortBy where
  parser =
    takeLowerText >>= \case
      "index" -> pure Index
      "timestamp" -> pure Timestamp
      e ->
        fromTextError $
          "Failure parsing PersonTrackingSortBy from value: '" <> e
            <> "'. Accepted values: index, timestamp"

instance ToText PersonTrackingSortBy where
  toText = \case
    Index -> "INDEX"
    Timestamp -> "TIMESTAMP"

instance Hashable PersonTrackingSortBy

instance NFData PersonTrackingSortBy

instance ToByteString PersonTrackingSortBy

instance ToQuery PersonTrackingSortBy

instance ToHeader PersonTrackingSortBy

instance ToJSON PersonTrackingSortBy where
  toJSON = toJSONText
