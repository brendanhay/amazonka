{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Sum where

import Network.AWS.Prelude

data Attribute
  = All
  | Default
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Attribute where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "default" -> pure Default
        e -> fromTextError $ "Failure parsing Attribute from value: '" <> e
           <> "'. Accepted values: all, default"

instance ToText Attribute where
    toText = \case
        All -> "ALL"
        Default -> "DEFAULT"

instance Hashable     Attribute
instance NFData       Attribute
instance ToByteString Attribute
instance ToQuery      Attribute
instance ToHeader     Attribute

instance ToJSON Attribute where
    toJSON = toJSONText

data CelebrityRecognitionSortBy
  = CRSBId
  | CRSBTimestamp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CelebrityRecognitionSortBy where
    parser = takeLowerText >>= \case
        "id" -> pure CRSBId
        "timestamp" -> pure CRSBTimestamp
        e -> fromTextError $ "Failure parsing CelebrityRecognitionSortBy from value: '" <> e
           <> "'. Accepted values: id, timestamp"

instance ToText CelebrityRecognitionSortBy where
    toText = \case
        CRSBId -> "ID"
        CRSBTimestamp -> "TIMESTAMP"

instance Hashable     CelebrityRecognitionSortBy
instance NFData       CelebrityRecognitionSortBy
instance ToByteString CelebrityRecognitionSortBy
instance ToQuery      CelebrityRecognitionSortBy
instance ToHeader     CelebrityRecognitionSortBy

instance ToJSON CelebrityRecognitionSortBy where
    toJSON = toJSONText

data ContentModerationSortBy
  = CMSBName
  | CMSBTimestamp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContentModerationSortBy where
    parser = takeLowerText >>= \case
        "name" -> pure CMSBName
        "timestamp" -> pure CMSBTimestamp
        e -> fromTextError $ "Failure parsing ContentModerationSortBy from value: '" <> e
           <> "'. Accepted values: name, timestamp"

instance ToText ContentModerationSortBy where
    toText = \case
        CMSBName -> "NAME"
        CMSBTimestamp -> "TIMESTAMP"

instance Hashable     ContentModerationSortBy
instance NFData       ContentModerationSortBy
instance ToByteString ContentModerationSortBy
instance ToQuery      ContentModerationSortBy
instance ToHeader     ContentModerationSortBy

instance ToJSON ContentModerationSortBy where
    toJSON = toJSONText

data EmotionName
  = Angry
  | Calm
  | Confused
  | Disgusted
  | Happy
  | Sad
  | Surprised
  | Unknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EmotionName where
    parser = takeLowerText >>= \case
        "angry" -> pure Angry
        "calm" -> pure Calm
        "confused" -> pure Confused
        "disgusted" -> pure Disgusted
        "happy" -> pure Happy
        "sad" -> pure Sad
        "surprised" -> pure Surprised
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing EmotionName from value: '" <> e
           <> "'. Accepted values: angry, calm, confused, disgusted, happy, sad, surprised, unknown"

instance ToText EmotionName where
    toText = \case
        Angry -> "ANGRY"
        Calm -> "CALM"
        Confused -> "CONFUSED"
        Disgusted -> "DISGUSTED"
        Happy -> "HAPPY"
        Sad -> "SAD"
        Surprised -> "SURPRISED"
        Unknown -> "UNKNOWN"

instance Hashable     EmotionName
instance NFData       EmotionName
instance ToByteString EmotionName
instance ToQuery      EmotionName
instance ToHeader     EmotionName

instance FromJSON EmotionName where
    parseJSON = parseJSONText "EmotionName"

data FaceAttributes
  = FAAll
  | FADefault
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FaceAttributes where
    parser = takeLowerText >>= \case
        "all" -> pure FAAll
        "default" -> pure FADefault
        e -> fromTextError $ "Failure parsing FaceAttributes from value: '" <> e
           <> "'. Accepted values: all, default"

instance ToText FaceAttributes where
    toText = \case
        FAAll -> "ALL"
        FADefault -> "DEFAULT"

instance Hashable     FaceAttributes
instance NFData       FaceAttributes
instance ToByteString FaceAttributes
instance ToQuery      FaceAttributes
instance ToHeader     FaceAttributes

instance ToJSON FaceAttributes where
    toJSON = toJSONText

data FaceSearchSortBy
  = FSSBIndex
  | FSSBTimestamp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FaceSearchSortBy where
    parser = takeLowerText >>= \case
        "index" -> pure FSSBIndex
        "timestamp" -> pure FSSBTimestamp
        e -> fromTextError $ "Failure parsing FaceSearchSortBy from value: '" <> e
           <> "'. Accepted values: index, timestamp"

instance ToText FaceSearchSortBy where
    toText = \case
        FSSBIndex -> "INDEX"
        FSSBTimestamp -> "TIMESTAMP"

instance Hashable     FaceSearchSortBy
instance NFData       FaceSearchSortBy
instance ToByteString FaceSearchSortBy
instance ToQuery      FaceSearchSortBy
instance ToHeader     FaceSearchSortBy

instance ToJSON FaceSearchSortBy where
    toJSON = toJSONText

data GenderType
  = Female
  | Male
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GenderType where
    parser = takeLowerText >>= \case
        "female" -> pure Female
        "male" -> pure Male
        e -> fromTextError $ "Failure parsing GenderType from value: '" <> e
           <> "'. Accepted values: female, male"

instance ToText GenderType where
    toText = \case
        Female -> "Female"
        Male -> "Male"

instance Hashable     GenderType
instance NFData       GenderType
instance ToByteString GenderType
instance ToQuery      GenderType
instance ToHeader     GenderType

instance FromJSON GenderType where
    parseJSON = parseJSONText "GenderType"

data LabelDetectionSortBy
  = LDSBName
  | LDSBTimestamp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LabelDetectionSortBy where
    parser = takeLowerText >>= \case
        "name" -> pure LDSBName
        "timestamp" -> pure LDSBTimestamp
        e -> fromTextError $ "Failure parsing LabelDetectionSortBy from value: '" <> e
           <> "'. Accepted values: name, timestamp"

instance ToText LabelDetectionSortBy where
    toText = \case
        LDSBName -> "NAME"
        LDSBTimestamp -> "TIMESTAMP"

instance Hashable     LabelDetectionSortBy
instance NFData       LabelDetectionSortBy
instance ToByteString LabelDetectionSortBy
instance ToQuery      LabelDetectionSortBy
instance ToHeader     LabelDetectionSortBy

instance ToJSON LabelDetectionSortBy where
    toJSON = toJSONText

data LandmarkType
  = EyeLeft
  | EyeRight
  | LeftEyeBrowLeft
  | LeftEyeBrowRight
  | LeftEyeBrowUp
  | LeftEyeDown
  | LeftEyeLeft
  | LeftEyeRight
  | LeftEyeUp
  | LeftPupil
  | MouthDown
  | MouthLeft
  | MouthRight
  | MouthUp
  | Nose
  | NoseLeft
  | NoseRight
  | RightEyeBrowLeft
  | RightEyeBrowRight
  | RightEyeBrowUp
  | RightEyeDown
  | RightEyeLeft
  | RightEyeRight
  | RightEyeUp
  | RightPupil
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LandmarkType where
    parser = takeLowerText >>= \case
        "eyeleft" -> pure EyeLeft
        "eyeright" -> pure EyeRight
        "lefteyebrowleft" -> pure LeftEyeBrowLeft
        "lefteyebrowright" -> pure LeftEyeBrowRight
        "lefteyebrowup" -> pure LeftEyeBrowUp
        "lefteyedown" -> pure LeftEyeDown
        "lefteyeleft" -> pure LeftEyeLeft
        "lefteyeright" -> pure LeftEyeRight
        "lefteyeup" -> pure LeftEyeUp
        "leftpupil" -> pure LeftPupil
        "mouthdown" -> pure MouthDown
        "mouthleft" -> pure MouthLeft
        "mouthright" -> pure MouthRight
        "mouthup" -> pure MouthUp
        "nose" -> pure Nose
        "noseleft" -> pure NoseLeft
        "noseright" -> pure NoseRight
        "righteyebrowleft" -> pure RightEyeBrowLeft
        "righteyebrowright" -> pure RightEyeBrowRight
        "righteyebrowup" -> pure RightEyeBrowUp
        "righteyedown" -> pure RightEyeDown
        "righteyeleft" -> pure RightEyeLeft
        "righteyeright" -> pure RightEyeRight
        "righteyeup" -> pure RightEyeUp
        "rightpupil" -> pure RightPupil
        e -> fromTextError $ "Failure parsing LandmarkType from value: '" <> e
           <> "'. Accepted values: eyeleft, eyeright, lefteyebrowleft, lefteyebrowright, lefteyebrowup, lefteyedown, lefteyeleft, lefteyeright, lefteyeup, leftpupil, mouthdown, mouthleft, mouthright, mouthup, nose, noseleft, noseright, righteyebrowleft, righteyebrowright, righteyebrowup, righteyedown, righteyeleft, righteyeright, righteyeup, rightpupil"

instance ToText LandmarkType where
    toText = \case
        EyeLeft -> "eyeLeft"
        EyeRight -> "eyeRight"
        LeftEyeBrowLeft -> "leftEyeBrowLeft"
        LeftEyeBrowRight -> "leftEyeBrowRight"
        LeftEyeBrowUp -> "leftEyeBrowUp"
        LeftEyeDown -> "leftEyeDown"
        LeftEyeLeft -> "leftEyeLeft"
        LeftEyeRight -> "leftEyeRight"
        LeftEyeUp -> "leftEyeUp"
        LeftPupil -> "leftPupil"
        MouthDown -> "mouthDown"
        MouthLeft -> "mouthLeft"
        MouthRight -> "mouthRight"
        MouthUp -> "mouthUp"
        Nose -> "nose"
        NoseLeft -> "noseLeft"
        NoseRight -> "noseRight"
        RightEyeBrowLeft -> "rightEyeBrowLeft"
        RightEyeBrowRight -> "rightEyeBrowRight"
        RightEyeBrowUp -> "rightEyeBrowUp"
        RightEyeDown -> "rightEyeDown"
        RightEyeLeft -> "rightEyeLeft"
        RightEyeRight -> "rightEyeRight"
        RightEyeUp -> "rightEyeUp"
        RightPupil -> "rightPupil"

instance Hashable     LandmarkType
instance NFData       LandmarkType
instance ToByteString LandmarkType
instance ToQuery      LandmarkType
instance ToHeader     LandmarkType

instance FromJSON LandmarkType where
    parseJSON = parseJSONText "LandmarkType"

data OrientationCorrection
  = Rotate0
  | Rotate180
  | Rotate270
  | Rotate90
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OrientationCorrection where
    parser = takeLowerText >>= \case
        "rotate_0" -> pure Rotate0
        "rotate_180" -> pure Rotate180
        "rotate_270" -> pure Rotate270
        "rotate_90" -> pure Rotate90
        e -> fromTextError $ "Failure parsing OrientationCorrection from value: '" <> e
           <> "'. Accepted values: rotate_0, rotate_180, rotate_270, rotate_90"

instance ToText OrientationCorrection where
    toText = \case
        Rotate0 -> "ROTATE_0"
        Rotate180 -> "ROTATE_180"
        Rotate270 -> "ROTATE_270"
        Rotate90 -> "ROTATE_90"

instance Hashable     OrientationCorrection
instance NFData       OrientationCorrection
instance ToByteString OrientationCorrection
instance ToQuery      OrientationCorrection
instance ToHeader     OrientationCorrection

instance FromJSON OrientationCorrection where
    parseJSON = parseJSONText "OrientationCorrection"

data PersonTrackingSortBy
  = Index
  | Timestamp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PersonTrackingSortBy where
    parser = takeLowerText >>= \case
        "index" -> pure Index
        "timestamp" -> pure Timestamp
        e -> fromTextError $ "Failure parsing PersonTrackingSortBy from value: '" <> e
           <> "'. Accepted values: index, timestamp"

instance ToText PersonTrackingSortBy where
    toText = \case
        Index -> "INDEX"
        Timestamp -> "TIMESTAMP"

instance Hashable     PersonTrackingSortBy
instance NFData       PersonTrackingSortBy
instance ToByteString PersonTrackingSortBy
instance ToQuery      PersonTrackingSortBy
instance ToHeader     PersonTrackingSortBy

instance ToJSON PersonTrackingSortBy where
    toJSON = toJSONText

data StreamProcessorStatus
  = SPSFailed
  | SPSRunning
  | SPSStarting
  | SPSStopped
  | SPSStopping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StreamProcessorStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure SPSFailed
        "running" -> pure SPSRunning
        "starting" -> pure SPSStarting
        "stopped" -> pure SPSStopped
        "stopping" -> pure SPSStopping
        e -> fromTextError $ "Failure parsing StreamProcessorStatus from value: '" <> e
           <> "'. Accepted values: failed, running, starting, stopped, stopping"

instance ToText StreamProcessorStatus where
    toText = \case
        SPSFailed -> "FAILED"
        SPSRunning -> "RUNNING"
        SPSStarting -> "STARTING"
        SPSStopped -> "STOPPED"
        SPSStopping -> "STOPPING"

instance Hashable     StreamProcessorStatus
instance NFData       StreamProcessorStatus
instance ToByteString StreamProcessorStatus
instance ToQuery      StreamProcessorStatus
instance ToHeader     StreamProcessorStatus

instance FromJSON StreamProcessorStatus where
    parseJSON = parseJSONText "StreamProcessorStatus"

data TextTypes
  = Line
  | Word
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TextTypes where
    parser = takeLowerText >>= \case
        "line" -> pure Line
        "word" -> pure Word
        e -> fromTextError $ "Failure parsing TextTypes from value: '" <> e
           <> "'. Accepted values: line, word"

instance ToText TextTypes where
    toText = \case
        Line -> "LINE"
        Word -> "WORD"

instance Hashable     TextTypes
instance NFData       TextTypes
instance ToByteString TextTypes
instance ToQuery      TextTypes
instance ToHeader     TextTypes

instance FromJSON TextTypes where
    parseJSON = parseJSONText "TextTypes"

data VideoJobStatus
  = Failed
  | InProgress
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VideoJobStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing VideoJobStatus from value: '" <> e
           <> "'. Accepted values: failed, in_progress, succeeded"

instance ToText VideoJobStatus where
    toText = \case
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        Succeeded -> "SUCCEEDED"

instance Hashable     VideoJobStatus
instance NFData       VideoJobStatus
instance ToByteString VideoJobStatus
instance ToQuery      VideoJobStatus
instance ToHeader     VideoJobStatus

instance FromJSON VideoJobStatus where
    parseJSON = parseJSONText "VideoJobStatus"
