{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.TapeStorageClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.TapeStorageClass where

import Network.AWS.Prelude

data TapeStorageClass
  = DeepArchive
  | Glacier
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

instance FromText TapeStorageClass where
  parser =
    takeLowerText >>= \case
      "deep_archive" -> pure DeepArchive
      "glacier" -> pure Glacier
      e ->
        fromTextError $
          "Failure parsing TapeStorageClass from value: '" <> e
            <> "'. Accepted values: deep_archive, glacier"

instance ToText TapeStorageClass where
  toText = \case
    DeepArchive -> "DEEP_ARCHIVE"
    Glacier -> "GLACIER"

instance Hashable TapeStorageClass

instance NFData TapeStorageClass

instance ToByteString TapeStorageClass

instance ToQuery TapeStorageClass

instance ToHeader TapeStorageClass

instance ToJSON TapeStorageClass where
  toJSON = toJSONText

instance FromJSON TapeStorageClass where
  parseJSON = parseJSONText "TapeStorageClass"
