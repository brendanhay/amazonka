{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchS3BackupMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchS3BackupMode where

import Network.AWS.Prelude

data ElasticsearchS3BackupMode
  = AllDocuments
  | FailedDocumentsOnly
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

instance FromText ElasticsearchS3BackupMode where
  parser =
    takeLowerText >>= \case
      "alldocuments" -> pure AllDocuments
      "faileddocumentsonly" -> pure FailedDocumentsOnly
      e ->
        fromTextError $
          "Failure parsing ElasticsearchS3BackupMode from value: '" <> e
            <> "'. Accepted values: alldocuments, faileddocumentsonly"

instance ToText ElasticsearchS3BackupMode where
  toText = \case
    AllDocuments -> "AllDocuments"
    FailedDocumentsOnly -> "FailedDocumentsOnly"

instance Hashable ElasticsearchS3BackupMode

instance NFData ElasticsearchS3BackupMode

instance ToByteString ElasticsearchS3BackupMode

instance ToQuery ElasticsearchS3BackupMode

instance ToHeader ElasticsearchS3BackupMode

instance ToJSON ElasticsearchS3BackupMode where
  toJSON = toJSONText

instance FromJSON ElasticsearchS3BackupMode where
  parseJSON = parseJSONText "ElasticsearchS3BackupMode"
