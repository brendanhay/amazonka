{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerDataFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerDataFormat where

import Network.AWS.Prelude

data EntityRecognizerDataFormat
  = AugmentedManifest
  | ComprehendCSV
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

instance FromText EntityRecognizerDataFormat where
  parser =
    takeLowerText >>= \case
      "augmented_manifest" -> pure AugmentedManifest
      "comprehend_csv" -> pure ComprehendCSV
      e ->
        fromTextError $
          "Failure parsing EntityRecognizerDataFormat from value: '" <> e
            <> "'. Accepted values: augmented_manifest, comprehend_csv"

instance ToText EntityRecognizerDataFormat where
  toText = \case
    AugmentedManifest -> "AUGMENTED_MANIFEST"
    ComprehendCSV -> "COMPREHEND_CSV"

instance Hashable EntityRecognizerDataFormat

instance NFData EntityRecognizerDataFormat

instance ToByteString EntityRecognizerDataFormat

instance ToQuery EntityRecognizerDataFormat

instance ToHeader EntityRecognizerDataFormat

instance ToJSON EntityRecognizerDataFormat where
  toJSON = toJSONText

instance FromJSON EntityRecognizerDataFormat where
  parseJSON = parseJSONText "EntityRecognizerDataFormat"
