{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageSortBy where

import Network.AWS.Prelude

data ImageSortBy
  = ISBCreationTime
  | ISBImageName
  | ISBLastModifiedTime
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

instance FromText ImageSortBy where
  parser =
    takeLowerText >>= \case
      "creation_time" -> pure ISBCreationTime
      "image_name" -> pure ISBImageName
      "last_modified_time" -> pure ISBLastModifiedTime
      e ->
        fromTextError $
          "Failure parsing ImageSortBy from value: '" <> e
            <> "'. Accepted values: creation_time, image_name, last_modified_time"

instance ToText ImageSortBy where
  toText = \case
    ISBCreationTime -> "CREATION_TIME"
    ISBImageName -> "IMAGE_NAME"
    ISBLastModifiedTime -> "LAST_MODIFIED_TIME"

instance Hashable ImageSortBy

instance NFData ImageSortBy

instance ToByteString ImageSortBy

instance ToQuery ImageSortBy

instance ToHeader ImageSortBy

instance ToJSON ImageSortBy where
  toJSON = toJSONText
