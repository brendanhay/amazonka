{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ContentClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ContentClassifier where

import Network.AWS.Prelude

data ContentClassifier
  = FreeOfAdultContent
  | FreeOfPersonallyIdentifiableInformation
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

instance FromText ContentClassifier where
  parser =
    takeLowerText >>= \case
      "freeofadultcontent" -> pure FreeOfAdultContent
      "freeofpersonallyidentifiableinformation" -> pure FreeOfPersonallyIdentifiableInformation
      e ->
        fromTextError $
          "Failure parsing ContentClassifier from value: '" <> e
            <> "'. Accepted values: freeofadultcontent, freeofpersonallyidentifiableinformation"

instance ToText ContentClassifier where
  toText = \case
    FreeOfAdultContent -> "FreeOfAdultContent"
    FreeOfPersonallyIdentifiableInformation -> "FreeOfPersonallyIdentifiableInformation"

instance Hashable ContentClassifier

instance NFData ContentClassifier

instance ToByteString ContentClassifier

instance ToQuery ContentClassifier

instance ToHeader ContentClassifier

instance ToJSON ContentClassifier where
  toJSON = toJSONText
