{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothManifestEncoding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothManifestEncoding where

import Network.AWS.Prelude

-- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding format for the server and client manifest. Valid options are utf8 and utf16.
data MsSmoothManifestEncoding
  = UTF16
  | UTF8
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

instance FromText MsSmoothManifestEncoding where
  parser =
    takeLowerText >>= \case
      "utf16" -> pure UTF16
      "utf8" -> pure UTF8
      e ->
        fromTextError $
          "Failure parsing MsSmoothManifestEncoding from value: '" <> e
            <> "'. Accepted values: utf16, utf8"

instance ToText MsSmoothManifestEncoding where
  toText = \case
    UTF16 -> "UTF16"
    UTF8 -> "UTF8"

instance Hashable MsSmoothManifestEncoding

instance NFData MsSmoothManifestEncoding

instance ToByteString MsSmoothManifestEncoding

instance ToQuery MsSmoothManifestEncoding

instance ToHeader MsSmoothManifestEncoding

instance ToJSON MsSmoothManifestEncoding where
  toJSON = toJSONText

instance FromJSON MsSmoothManifestEncoding where
  parseJSON = parseJSONText "MsSmoothManifestEncoding"
