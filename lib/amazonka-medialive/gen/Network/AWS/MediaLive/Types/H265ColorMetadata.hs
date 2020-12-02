{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265ColorMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265ColorMetadata where

import Network.AWS.Prelude

-- | H265 Color Metadata
data H265ColorMetadata
  = HCMIgnore
  | HCMInsert
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

instance FromText H265ColorMetadata where
  parser =
    takeLowerText >>= \case
      "ignore" -> pure HCMIgnore
      "insert" -> pure HCMInsert
      e ->
        fromTextError $
          "Failure parsing H265ColorMetadata from value: '" <> e
            <> "'. Accepted values: ignore, insert"

instance ToText H265ColorMetadata where
  toText = \case
    HCMIgnore -> "IGNORE"
    HCMInsert -> "INSERT"

instance Hashable H265ColorMetadata

instance NFData H265ColorMetadata

instance ToByteString H265ColorMetadata

instance ToQuery H265ColorMetadata

instance ToHeader H265ColorMetadata

instance ToJSON H265ColorMetadata where
  toJSON = toJSONText

instance FromJSON H265ColorMetadata where
  parseJSON = parseJSONText "H265ColorMetadata"
