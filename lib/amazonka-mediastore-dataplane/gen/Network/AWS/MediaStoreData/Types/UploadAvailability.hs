{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.Types.UploadAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStoreData.Types.UploadAvailability where

import Network.AWS.Prelude

data UploadAvailability
  = Standard
  | Streaming
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

instance FromText UploadAvailability where
  parser =
    takeLowerText >>= \case
      "standard" -> pure Standard
      "streaming" -> pure Streaming
      e ->
        fromTextError $
          "Failure parsing UploadAvailability from value: '" <> e
            <> "'. Accepted values: standard, streaming"

instance ToText UploadAvailability where
  toText = \case
    Standard -> "STANDARD"
    Streaming -> "STREAMING"

instance Hashable UploadAvailability

instance NFData UploadAvailability

instance ToByteString UploadAvailability

instance ToQuery UploadAvailability

instance ToHeader UploadAvailability

instance ToJSON UploadAvailability where
  toJSON = toJSONText
