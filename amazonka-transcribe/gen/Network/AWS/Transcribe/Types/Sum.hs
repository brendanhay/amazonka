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
  = DeDe
  | EnAu
  | EnGb
  | EnUs
  | EsUs
  | FrCa
  | FrFr
  | ItIt
  | KoKr
  | PtBr
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LanguageCode where
    parser = takeLowerText >>= \case
        "de-de" -> pure DeDe
        "en-au" -> pure EnAu
        "en-gb" -> pure EnGb
        "en-us" -> pure EnUs
        "es-us" -> pure EsUs
        "fr-ca" -> pure FrCa
        "fr-fr" -> pure FrFr
        "it-it" -> pure ItIt
        "ko-kr" -> pure KoKr
        "pt-br" -> pure PtBr
        e -> fromTextError $ "Failure parsing LanguageCode from value: '" <> e
           <> "'. Accepted values: de-de, en-au, en-gb, en-us, es-us, fr-ca, fr-fr, it-it, ko-kr, pt-br"

instance ToText LanguageCode where
    toText = \case
        DeDe -> "de-DE"
        EnAu -> "en-AU"
        EnGb -> "en-GB"
        EnUs -> "en-US"
        EsUs -> "es-US"
        FrCa -> "fr-CA"
        FrFr -> "fr-FR"
        ItIt -> "it-IT"
        KoKr -> "ko-KR"
        PtBr -> "pt-BR"

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

data OutputLocationType
  = CustomerBucket
  | ServiceBucket
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OutputLocationType where
    parser = takeLowerText >>= \case
        "customer_bucket" -> pure CustomerBucket
        "service_bucket" -> pure ServiceBucket
        e -> fromTextError $ "Failure parsing OutputLocationType from value: '" <> e
           <> "'. Accepted values: customer_bucket, service_bucket"

instance ToText OutputLocationType where
    toText = \case
        CustomerBucket -> "CUSTOMER_BUCKET"
        ServiceBucket -> "SERVICE_BUCKET"

instance Hashable     OutputLocationType
instance NFData       OutputLocationType
instance ToByteString OutputLocationType
instance ToQuery      OutputLocationType
instance ToHeader     OutputLocationType

instance FromJSON OutputLocationType where
    parseJSON = parseJSONText "OutputLocationType"

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
