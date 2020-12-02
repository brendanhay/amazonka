{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.StreamViewType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.StreamViewType where

import Network.AWS.Prelude

data StreamViewType
  = KeysOnly
  | NewAndOldImages
  | NewImage
  | OldImage
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

instance FromText StreamViewType where
  parser =
    takeLowerText >>= \case
      "keys_only" -> pure KeysOnly
      "new_and_old_images" -> pure NewAndOldImages
      "new_image" -> pure NewImage
      "old_image" -> pure OldImage
      e ->
        fromTextError $
          "Failure parsing StreamViewType from value: '" <> e
            <> "'. Accepted values: keys_only, new_and_old_images, new_image, old_image"

instance ToText StreamViewType where
  toText = \case
    KeysOnly -> "KEYS_ONLY"
    NewAndOldImages -> "NEW_AND_OLD_IMAGES"
    NewImage -> "NEW_IMAGE"
    OldImage -> "OLD_IMAGE"

instance Hashable StreamViewType

instance NFData StreamViewType

instance ToByteString StreamViewType

instance ToQuery StreamViewType

instance ToHeader StreamViewType

instance FromJSON StreamViewType where
  parseJSON = parseJSONText "StreamViewType"
