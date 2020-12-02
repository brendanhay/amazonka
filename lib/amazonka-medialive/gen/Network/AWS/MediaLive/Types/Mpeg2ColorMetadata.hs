{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2ColorMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2ColorMetadata where

import Network.AWS.Prelude

-- | Mpeg2 Color Metadata
data Mpeg2ColorMetadata
  = MCMIgnore
  | MCMInsert
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

instance FromText Mpeg2ColorMetadata where
  parser =
    takeLowerText >>= \case
      "ignore" -> pure MCMIgnore
      "insert" -> pure MCMInsert
      e ->
        fromTextError $
          "Failure parsing Mpeg2ColorMetadata from value: '" <> e
            <> "'. Accepted values: ignore, insert"

instance ToText Mpeg2ColorMetadata where
  toText = \case
    MCMIgnore -> "IGNORE"
    MCMInsert -> "INSERT"

instance Hashable Mpeg2ColorMetadata

instance NFData Mpeg2ColorMetadata

instance ToByteString Mpeg2ColorMetadata

instance ToQuery Mpeg2ColorMetadata

instance ToHeader Mpeg2ColorMetadata

instance ToJSON Mpeg2ColorMetadata where
  toJSON = toJSONText

instance FromJSON Mpeg2ColorMetadata where
  parseJSON = parseJSONText "Mpeg2ColorMetadata"
