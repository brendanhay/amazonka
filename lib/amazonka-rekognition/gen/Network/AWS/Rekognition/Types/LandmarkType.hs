{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.LandmarkType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.LandmarkType where

import Network.AWS.Prelude

data LandmarkType
  = ChinBottom
  | EyeLeft
  | EyeRight
  | LeftEyeBrowLeft
  | LeftEyeBrowRight
  | LeftEyeBrowUp
  | LeftEyeDown
  | LeftEyeLeft
  | LeftEyeRight
  | LeftEyeUp
  | LeftPupil
  | MidJawlineLeft
  | MidJawlineRight
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
  | UpperJawlineLeft
  | UpperJawlineRight
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

instance FromText LandmarkType where
  parser =
    takeLowerText >>= \case
      "chinbottom" -> pure ChinBottom
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
      "midjawlineleft" -> pure MidJawlineLeft
      "midjawlineright" -> pure MidJawlineRight
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
      "upperjawlineleft" -> pure UpperJawlineLeft
      "upperjawlineright" -> pure UpperJawlineRight
      e ->
        fromTextError $
          "Failure parsing LandmarkType from value: '" <> e
            <> "'. Accepted values: chinbottom, eyeleft, eyeright, lefteyebrowleft, lefteyebrowright, lefteyebrowup, lefteyedown, lefteyeleft, lefteyeright, lefteyeup, leftpupil, midjawlineleft, midjawlineright, mouthdown, mouthleft, mouthright, mouthup, nose, noseleft, noseright, righteyebrowleft, righteyebrowright, righteyebrowup, righteyedown, righteyeleft, righteyeright, righteyeup, rightpupil, upperjawlineleft, upperjawlineright"

instance ToText LandmarkType where
  toText = \case
    ChinBottom -> "chinBottom"
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
    MidJawlineLeft -> "midJawlineLeft"
    MidJawlineRight -> "midJawlineRight"
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
    UpperJawlineLeft -> "upperJawlineLeft"
    UpperJawlineRight -> "upperJawlineRight"

instance Hashable LandmarkType

instance NFData LandmarkType

instance ToByteString LandmarkType

instance ToQuery LandmarkType

instance ToHeader LandmarkType

instance FromJSON LandmarkType where
  parseJSON = parseJSONText "LandmarkType"
