{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ContentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ContentType where

import Network.AWS.Prelude

-- | Specifies the media type of the thumbnail.
data ContentType = ImageJpeg
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

instance FromText ContentType where
  parser =
    takeLowerText >>= \case
      "image/jpeg" -> pure ImageJpeg
      e ->
        fromTextError $
          "Failure parsing ContentType from value: '" <> e
            <> "'. Accepted values: image/jpeg"

instance ToText ContentType where
  toText = \case
    ImageJpeg -> "image/jpeg"

instance Hashable ContentType

instance NFData ContentType

instance ToByteString ContentType

instance ToQuery ContentType

instance ToHeader ContentType

instance FromJSON ContentType where
  parseJSON = parseJSONText "ContentType"
