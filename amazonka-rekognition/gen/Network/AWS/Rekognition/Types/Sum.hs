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
        Female -> "Female"
        Male -> "Male"

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
