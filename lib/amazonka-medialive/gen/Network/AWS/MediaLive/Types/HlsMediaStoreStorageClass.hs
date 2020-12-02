{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsMediaStoreStorageClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsMediaStoreStorageClass where

import Network.AWS.Prelude

-- | Hls Media Store Storage Class
data HlsMediaStoreStorageClass = Temporal
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

instance FromText HlsMediaStoreStorageClass where
  parser =
    takeLowerText >>= \case
      "temporal" -> pure Temporal
      e ->
        fromTextError $
          "Failure parsing HlsMediaStoreStorageClass from value: '" <> e
            <> "'. Accepted values: temporal"

instance ToText HlsMediaStoreStorageClass where
  toText = \case
    Temporal -> "TEMPORAL"

instance Hashable HlsMediaStoreStorageClass

instance NFData HlsMediaStoreStorageClass

instance ToByteString HlsMediaStoreStorageClass

instance ToQuery HlsMediaStoreStorageClass

instance ToHeader HlsMediaStoreStorageClass

instance ToJSON HlsMediaStoreStorageClass where
  toJSON = toJSONText

instance FromJSON HlsMediaStoreStorageClass where
  parseJSON = parseJSONText "HlsMediaStoreStorageClass"
