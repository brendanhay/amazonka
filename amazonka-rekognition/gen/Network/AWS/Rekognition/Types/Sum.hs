{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Sum
-- Copyright   : (c) 2013-2017 Brendan Hay
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
        Female -> "FEMALE"
        Male -> "MALE"

instance Hashable     GenderType
instance NFData       GenderType
instance ToByteString GenderType
instance ToQuery      GenderType
instance ToHeader     GenderType

instance FromJSON GenderType where
    parseJSON = parseJSONText "GenderType"

data LandmarkType
  = EyeLeft
  | EyeRight
  | LeftEyeDown
  | LeftEyeLeft
  | LeftEyeRight
  | LeftEyeUp
  | LeftEyebrowLeft
  | LeftEyebrowRight
  | LeftEyebrowUp
  | LeftPupil
  | MouthDown
  | MouthLeft
  | MouthRight
  | MouthUp
  | Nose
  | NoseLeft
  | NoseRight
  | RightEyeDown
  | RightEyeLeft
  | RightEyeRight
  | RightEyeUp
  | RightEyebrowLeft
  | RightEyebrowRight
  | RightEyebrowUp
  | RightPupil
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LandmarkType where
    parser = takeLowerText >>= \case
        "eye_left" -> pure EyeLeft
        "eye_right" -> pure EyeRight
        "left_eye_down" -> pure LeftEyeDown
        "left_eye_left" -> pure LeftEyeLeft
        "left_eye_right" -> pure LeftEyeRight
        "left_eye_up" -> pure LeftEyeUp
        "left_eyebrow_left" -> pure LeftEyebrowLeft
        "left_eyebrow_right" -> pure LeftEyebrowRight
        "left_eyebrow_up" -> pure LeftEyebrowUp
        "left_pupil" -> pure LeftPupil
        "mouth_down" -> pure MouthDown
        "mouth_left" -> pure MouthLeft
        "mouth_right" -> pure MouthRight
        "mouth_up" -> pure MouthUp
        "nose" -> pure Nose
        "nose_left" -> pure NoseLeft
        "nose_right" -> pure NoseRight
        "right_eye_down" -> pure RightEyeDown
        "right_eye_left" -> pure RightEyeLeft
        "right_eye_right" -> pure RightEyeRight
        "right_eye_up" -> pure RightEyeUp
        "right_eyebrow_left" -> pure RightEyebrowLeft
        "right_eyebrow_right" -> pure RightEyebrowRight
        "right_eyebrow_up" -> pure RightEyebrowUp
        "right_pupil" -> pure RightPupil
        e -> fromTextError $ "Failure parsing LandmarkType from value: '" <> e
           <> "'. Accepted values: eye_left, eye_right, left_eye_down, left_eye_left, left_eye_right, left_eye_up, left_eyebrow_left, left_eyebrow_right, left_eyebrow_up, left_pupil, mouth_down, mouth_left, mouth_right, mouth_up, nose, nose_left, nose_right, right_eye_down, right_eye_left, right_eye_right, right_eye_up, right_eyebrow_left, right_eyebrow_right, right_eyebrow_up, right_pupil"

instance ToText LandmarkType where
    toText = \case
        EyeLeft -> "EYE_LEFT"
        EyeRight -> "EYE_RIGHT"
        LeftEyeDown -> "LEFT_EYE_DOWN"
        LeftEyeLeft -> "LEFT_EYE_LEFT"
        LeftEyeRight -> "LEFT_EYE_RIGHT"
        LeftEyeUp -> "LEFT_EYE_UP"
        LeftEyebrowLeft -> "LEFT_EYEBROW_LEFT"
        LeftEyebrowRight -> "LEFT_EYEBROW_RIGHT"
        LeftEyebrowUp -> "LEFT_EYEBROW_UP"
        LeftPupil -> "LEFT_PUPIL"
        MouthDown -> "MOUTH_DOWN"
        MouthLeft -> "MOUTH_LEFT"
        MouthRight -> "MOUTH_RIGHT"
        MouthUp -> "MOUTH_UP"
        Nose -> "NOSE"
        NoseLeft -> "NOSE_LEFT"
        NoseRight -> "NOSE_RIGHT"
        RightEyeDown -> "RIGHT_EYE_DOWN"
        RightEyeLeft -> "RIGHT_EYE_LEFT"
        RightEyeRight -> "RIGHT_EYE_RIGHT"
        RightEyeUp -> "RIGHT_EYE_UP"
        RightEyebrowLeft -> "RIGHT_EYEBROW_LEFT"
        RightEyebrowRight -> "RIGHT_EYEBROW_RIGHT"
        RightEyebrowUp -> "RIGHT_EYEBROW_UP"
        RightPupil -> "RIGHT_PUPIL"

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
