{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.StorageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.StorageType where

import Network.AWS.Prelude

data StorageType
  = KinesisFirehose
  | KinesisStream
  | KinesisVideoStream
  | S3
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

instance FromText StorageType where
  parser =
    takeLowerText >>= \case
      "kinesis_firehose" -> pure KinesisFirehose
      "kinesis_stream" -> pure KinesisStream
      "kinesis_video_stream" -> pure KinesisVideoStream
      "s3" -> pure S3
      e ->
        fromTextError $
          "Failure parsing StorageType from value: '" <> e
            <> "'. Accepted values: kinesis_firehose, kinesis_stream, kinesis_video_stream, s3"

instance ToText StorageType where
  toText = \case
    KinesisFirehose -> "KINESIS_FIREHOSE"
    KinesisStream -> "KINESIS_STREAM"
    KinesisVideoStream -> "KINESIS_VIDEO_STREAM"
    S3 -> "S3"

instance Hashable StorageType

instance NFData StorageType

instance ToByteString StorageType

instance ToQuery StorageType

instance ToHeader StorageType

instance ToJSON StorageType where
  toJSON = toJSONText

instance FromJSON StorageType where
  parseJSON = parseJSONText "StorageType"
