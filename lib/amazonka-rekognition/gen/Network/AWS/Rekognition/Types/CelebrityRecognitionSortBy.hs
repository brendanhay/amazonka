{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CelebrityRecognitionSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CelebrityRecognitionSortBy where

import Network.AWS.Prelude

data CelebrityRecognitionSortBy
  = CRSBId
  | CRSBTimestamp
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

instance FromText CelebrityRecognitionSortBy where
  parser =
    takeLowerText >>= \case
      "id" -> pure CRSBId
      "timestamp" -> pure CRSBTimestamp
      e ->
        fromTextError $
          "Failure parsing CelebrityRecognitionSortBy from value: '" <> e
            <> "'. Accepted values: id, timestamp"

instance ToText CelebrityRecognitionSortBy where
  toText = \case
    CRSBId -> "ID"
    CRSBTimestamp -> "TIMESTAMP"

instance Hashable CelebrityRecognitionSortBy

instance NFData CelebrityRecognitionSortBy

instance ToByteString CelebrityRecognitionSortBy

instance ToQuery CelebrityRecognitionSortBy

instance ToHeader CelebrityRecognitionSortBy

instance ToJSON CelebrityRecognitionSortBy where
  toJSON = toJSONText
