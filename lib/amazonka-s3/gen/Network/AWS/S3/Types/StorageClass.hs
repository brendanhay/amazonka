{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.StorageClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.StorageClass where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data StorageClass
  = DeepArchive
  | Glacier
  | IntelligentTiering
  | OnezoneIA
  | Outposts
  | ReducedRedundancy
  | Standard
  | StandardIA
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

instance FromText StorageClass where
  parser =
    takeLowerText >>= \case
      "deep_archive" -> pure DeepArchive
      "glacier" -> pure Glacier
      "intelligent_tiering" -> pure IntelligentTiering
      "onezone_ia" -> pure OnezoneIA
      "outposts" -> pure Outposts
      "reduced_redundancy" -> pure ReducedRedundancy
      "standard" -> pure Standard
      "standard_ia" -> pure StandardIA
      e ->
        fromTextError $
          "Failure parsing StorageClass from value: '" <> e
            <> "'. Accepted values: deep_archive, glacier, intelligent_tiering, onezone_ia, outposts, reduced_redundancy, standard, standard_ia"

instance ToText StorageClass where
  toText = \case
    DeepArchive -> "DEEP_ARCHIVE"
    Glacier -> "GLACIER"
    IntelligentTiering -> "INTELLIGENT_TIERING"
    OnezoneIA -> "ONEZONE_IA"
    Outposts -> "OUTPOSTS"
    ReducedRedundancy -> "REDUCED_REDUNDANCY"
    Standard -> "STANDARD"
    StandardIA -> "STANDARD_IA"

instance Hashable StorageClass

instance NFData StorageClass

instance ToByteString StorageClass

instance ToQuery StorageClass

instance ToHeader StorageClass

instance FromXML StorageClass where
  parseXML = parseXMLText "StorageClass"

instance ToXML StorageClass where
  toXML = toXMLText
