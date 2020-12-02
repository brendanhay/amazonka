{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierDataFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierDataFormat where

import Network.AWS.Prelude

data DocumentClassifierDataFormat
  = DCDFAugmentedManifest
  | DCDFComprehendCSV
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

instance FromText DocumentClassifierDataFormat where
  parser =
    takeLowerText >>= \case
      "augmented_manifest" -> pure DCDFAugmentedManifest
      "comprehend_csv" -> pure DCDFComprehendCSV
      e ->
        fromTextError $
          "Failure parsing DocumentClassifierDataFormat from value: '" <> e
            <> "'. Accepted values: augmented_manifest, comprehend_csv"

instance ToText DocumentClassifierDataFormat where
  toText = \case
    DCDFAugmentedManifest -> "AUGMENTED_MANIFEST"
    DCDFComprehendCSV -> "COMPREHEND_CSV"

instance Hashable DocumentClassifierDataFormat

instance NFData DocumentClassifierDataFormat

instance ToByteString DocumentClassifierDataFormat

instance ToQuery DocumentClassifierDataFormat

instance ToHeader DocumentClassifierDataFormat

instance ToJSON DocumentClassifierDataFormat where
  toJSON = toJSONText

instance FromJSON DocumentClassifierDataFormat where
  parseJSON = parseJSONText "DocumentClassifierDataFormat"
