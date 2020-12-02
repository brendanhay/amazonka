{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.LabelDetectionSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.LabelDetectionSortBy where

import Network.AWS.Prelude

data LabelDetectionSortBy
  = LDSBName
  | LDSBTimestamp
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

instance FromText LabelDetectionSortBy where
  parser =
    takeLowerText >>= \case
      "name" -> pure LDSBName
      "timestamp" -> pure LDSBTimestamp
      e ->
        fromTextError $
          "Failure parsing LabelDetectionSortBy from value: '" <> e
            <> "'. Accepted values: name, timestamp"

instance ToText LabelDetectionSortBy where
  toText = \case
    LDSBName -> "NAME"
    LDSBTimestamp -> "TIMESTAMP"

instance Hashable LabelDetectionSortBy

instance NFData LabelDetectionSortBy

instance ToByteString LabelDetectionSortBy

instance ToQuery LabelDetectionSortBy

instance ToHeader LabelDetectionSortBy

instance ToJSON LabelDetectionSortBy where
  toJSON = toJSONText
