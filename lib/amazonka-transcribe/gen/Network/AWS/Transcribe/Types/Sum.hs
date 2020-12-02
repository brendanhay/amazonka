{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.Sum where

import Network.AWS.Prelude

data LanguageCode
  = EnUs
  | EsUs
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LanguageCode where
    parser = takeLowerText >>= \case
        "en-us" -> pure EnUs
        "es-us" -> pure EsUs
        e -> fromTextError $ "Failure parsing LanguageCode from value: '" <> e
           <> "'. Accepted values: en-us, es-us"

instance ToText LanguageCode where
    toText = \case
        EnUs -> "en-US"
        EsUs -> "es-US"

instance Hashable     LanguageCode
instance NFData       LanguageCode
instance ToByteString LanguageCode
instance ToQuery      LanguageCode
instance ToHeader     LanguageCode

instance ToJSON LanguageCode where
    toJSON = toJSONText

instance FromJSON LanguageCode where
    parseJSON = parseJSONText "LanguageCode"

data MediaFormat
  = Flac
  | MP3
  | MP4
  | Wav
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MediaFormat where
    parser = takeLowerText >>= \case
        "flac" -> pure Flac
        "mp3" -> pure MP3
        "mp4" -> pure MP4
        "wav" -> pure Wav
        e -> fromTextError $ "Failure parsing MediaFormat from value: '" <> e
           <> "'. Accepted values: flac, mp3, mp4, wav"

instance ToText MediaFormat where
    toText = \case
        Flac -> "flac"
        MP3 -> "mp3"
        MP4 -> "mp4"
        Wav -> "wav"

instance Hashable     MediaFormat
instance NFData       MediaFormat
instance ToByteString MediaFormat
instance ToQuery      MediaFormat
instance ToHeader     MediaFormat

instance ToJSON MediaFormat where
    toJSON = toJSONText

instance FromJSON MediaFormat where
    parseJSON = parseJSONText "MediaFormat"

data TranscriptionJobStatus
  = TJSCompleted
  | TJSFailed
  | TJSInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TranscriptionJobStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure TJSCompleted
        "failed" -> pure TJSFailed
        "in_progress" -> pure TJSInProgress
        e -> fromTextError $ "Failure parsing TranscriptionJobStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, in_progress"

instance ToText TranscriptionJobStatus where
    toText = \case
        TJSCompleted -> "COMPLETED"
        TJSFailed -> "FAILED"
        TJSInProgress -> "IN_PROGRESS"

instance Hashable     TranscriptionJobStatus
instance NFData       TranscriptionJobStatus
instance ToByteString TranscriptionJobStatus
instance ToQuery      TranscriptionJobStatus
instance ToHeader     TranscriptionJobStatus

instance ToJSON TranscriptionJobStatus where
    toJSON = toJSONText

instance FromJSON TranscriptionJobStatus where
    parseJSON = parseJSONText "TranscriptionJobStatus"

data VocabularyState
  = Failed
  | Pending
  | Ready
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VocabularyState where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "pending" -> pure Pending
        "ready" -> pure Ready
        e -> fromTextError $ "Failure parsing VocabularyState from value: '" <> e
           <> "'. Accepted values: failed, pending, ready"

instance ToText VocabularyState where
    toText = \case
        Failed -> "FAILED"
        Pending -> "PENDING"
        Ready -> "READY"

instance Hashable     VocabularyState
instance NFData       VocabularyState
instance ToByteString VocabularyState
instance ToQuery      VocabularyState
instance ToHeader     VocabularyState

instance ToJSON VocabularyState where
    toJSON = toJSONText

instance FromJSON VocabularyState where
    parseJSON = parseJSONText "VocabularyState"
