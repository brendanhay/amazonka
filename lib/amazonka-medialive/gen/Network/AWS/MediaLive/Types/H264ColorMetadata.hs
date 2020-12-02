{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ColorMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264ColorMetadata where

import Network.AWS.Prelude

-- | H264 Color Metadata
data H264ColorMetadata
  = HIgnore
  | HInsert
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

instance FromText H264ColorMetadata where
  parser =
    takeLowerText >>= \case
      "ignore" -> pure HIgnore
      "insert" -> pure HInsert
      e ->
        fromTextError $
          "Failure parsing H264ColorMetadata from value: '" <> e
            <> "'. Accepted values: ignore, insert"

instance ToText H264ColorMetadata where
  toText = \case
    HIgnore -> "IGNORE"
    HInsert -> "INSERT"

instance Hashable H264ColorMetadata

instance NFData H264ColorMetadata

instance ToByteString H264ColorMetadata

instance ToQuery H264ColorMetadata

instance ToHeader H264ColorMetadata

instance ToJSON H264ColorMetadata where
  toJSON = toJSONText

instance FromJSON H264ColorMetadata where
  parseJSON = parseJSONText "H264ColorMetadata"
