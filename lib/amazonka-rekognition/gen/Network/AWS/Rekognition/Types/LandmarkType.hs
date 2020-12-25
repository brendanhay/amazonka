{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.LandmarkType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.LandmarkType
  ( LandmarkType
      ( LandmarkType',
        LandmarkTypeEyeLeft,
        LandmarkTypeEyeRight,
        LandmarkTypeNose,
        LandmarkTypeMouthLeft,
        LandmarkTypeMouthRight,
        LandmarkTypeLeftEyeBrowLeft,
        LandmarkTypeLeftEyeBrowRight,
        LandmarkTypeLeftEyeBrowUp,
        LandmarkTypeRightEyeBrowLeft,
        LandmarkTypeRightEyeBrowRight,
        LandmarkTypeRightEyeBrowUp,
        LandmarkTypeLeftEyeLeft,
        LandmarkTypeLeftEyeRight,
        LandmarkTypeLeftEyeUp,
        LandmarkTypeLeftEyeDown,
        LandmarkTypeRightEyeLeft,
        LandmarkTypeRightEyeRight,
        LandmarkTypeRightEyeUp,
        LandmarkTypeRightEyeDown,
        LandmarkTypeNoseLeft,
        LandmarkTypeNoseRight,
        LandmarkTypeMouthUp,
        LandmarkTypeMouthDown,
        LandmarkTypeLeftPupil,
        LandmarkTypeRightPupil,
        LandmarkTypeUpperJawlineLeft,
        LandmarkTypeMidJawlineLeft,
        LandmarkTypeChinBottom,
        LandmarkTypeMidJawlineRight,
        LandmarkTypeUpperJawlineRight,
        fromLandmarkType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype LandmarkType = LandmarkType' {fromLandmarkType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern LandmarkTypeEyeLeft :: LandmarkType
pattern LandmarkTypeEyeLeft = LandmarkType' "eyeLeft"

pattern LandmarkTypeEyeRight :: LandmarkType
pattern LandmarkTypeEyeRight = LandmarkType' "eyeRight"

pattern LandmarkTypeNose :: LandmarkType
pattern LandmarkTypeNose = LandmarkType' "nose"

pattern LandmarkTypeMouthLeft :: LandmarkType
pattern LandmarkTypeMouthLeft = LandmarkType' "mouthLeft"

pattern LandmarkTypeMouthRight :: LandmarkType
pattern LandmarkTypeMouthRight = LandmarkType' "mouthRight"

pattern LandmarkTypeLeftEyeBrowLeft :: LandmarkType
pattern LandmarkTypeLeftEyeBrowLeft = LandmarkType' "leftEyeBrowLeft"

pattern LandmarkTypeLeftEyeBrowRight :: LandmarkType
pattern LandmarkTypeLeftEyeBrowRight = LandmarkType' "leftEyeBrowRight"

pattern LandmarkTypeLeftEyeBrowUp :: LandmarkType
pattern LandmarkTypeLeftEyeBrowUp = LandmarkType' "leftEyeBrowUp"

pattern LandmarkTypeRightEyeBrowLeft :: LandmarkType
pattern LandmarkTypeRightEyeBrowLeft = LandmarkType' "rightEyeBrowLeft"

pattern LandmarkTypeRightEyeBrowRight :: LandmarkType
pattern LandmarkTypeRightEyeBrowRight = LandmarkType' "rightEyeBrowRight"

pattern LandmarkTypeRightEyeBrowUp :: LandmarkType
pattern LandmarkTypeRightEyeBrowUp = LandmarkType' "rightEyeBrowUp"

pattern LandmarkTypeLeftEyeLeft :: LandmarkType
pattern LandmarkTypeLeftEyeLeft = LandmarkType' "leftEyeLeft"

pattern LandmarkTypeLeftEyeRight :: LandmarkType
pattern LandmarkTypeLeftEyeRight = LandmarkType' "leftEyeRight"

pattern LandmarkTypeLeftEyeUp :: LandmarkType
pattern LandmarkTypeLeftEyeUp = LandmarkType' "leftEyeUp"

pattern LandmarkTypeLeftEyeDown :: LandmarkType
pattern LandmarkTypeLeftEyeDown = LandmarkType' "leftEyeDown"

pattern LandmarkTypeRightEyeLeft :: LandmarkType
pattern LandmarkTypeRightEyeLeft = LandmarkType' "rightEyeLeft"

pattern LandmarkTypeRightEyeRight :: LandmarkType
pattern LandmarkTypeRightEyeRight = LandmarkType' "rightEyeRight"

pattern LandmarkTypeRightEyeUp :: LandmarkType
pattern LandmarkTypeRightEyeUp = LandmarkType' "rightEyeUp"

pattern LandmarkTypeRightEyeDown :: LandmarkType
pattern LandmarkTypeRightEyeDown = LandmarkType' "rightEyeDown"

pattern LandmarkTypeNoseLeft :: LandmarkType
pattern LandmarkTypeNoseLeft = LandmarkType' "noseLeft"

pattern LandmarkTypeNoseRight :: LandmarkType
pattern LandmarkTypeNoseRight = LandmarkType' "noseRight"

pattern LandmarkTypeMouthUp :: LandmarkType
pattern LandmarkTypeMouthUp = LandmarkType' "mouthUp"

pattern LandmarkTypeMouthDown :: LandmarkType
pattern LandmarkTypeMouthDown = LandmarkType' "mouthDown"

pattern LandmarkTypeLeftPupil :: LandmarkType
pattern LandmarkTypeLeftPupil = LandmarkType' "leftPupil"

pattern LandmarkTypeRightPupil :: LandmarkType
pattern LandmarkTypeRightPupil = LandmarkType' "rightPupil"

pattern LandmarkTypeUpperJawlineLeft :: LandmarkType
pattern LandmarkTypeUpperJawlineLeft = LandmarkType' "upperJawlineLeft"

pattern LandmarkTypeMidJawlineLeft :: LandmarkType
pattern LandmarkTypeMidJawlineLeft = LandmarkType' "midJawlineLeft"

pattern LandmarkTypeChinBottom :: LandmarkType
pattern LandmarkTypeChinBottom = LandmarkType' "chinBottom"

pattern LandmarkTypeMidJawlineRight :: LandmarkType
pattern LandmarkTypeMidJawlineRight = LandmarkType' "midJawlineRight"

pattern LandmarkTypeUpperJawlineRight :: LandmarkType
pattern LandmarkTypeUpperJawlineRight = LandmarkType' "upperJawlineRight"

{-# COMPLETE
  LandmarkTypeEyeLeft,
  LandmarkTypeEyeRight,
  LandmarkTypeNose,
  LandmarkTypeMouthLeft,
  LandmarkTypeMouthRight,
  LandmarkTypeLeftEyeBrowLeft,
  LandmarkTypeLeftEyeBrowRight,
  LandmarkTypeLeftEyeBrowUp,
  LandmarkTypeRightEyeBrowLeft,
  LandmarkTypeRightEyeBrowRight,
  LandmarkTypeRightEyeBrowUp,
  LandmarkTypeLeftEyeLeft,
  LandmarkTypeLeftEyeRight,
  LandmarkTypeLeftEyeUp,
  LandmarkTypeLeftEyeDown,
  LandmarkTypeRightEyeLeft,
  LandmarkTypeRightEyeRight,
  LandmarkTypeRightEyeUp,
  LandmarkTypeRightEyeDown,
  LandmarkTypeNoseLeft,
  LandmarkTypeNoseRight,
  LandmarkTypeMouthUp,
  LandmarkTypeMouthDown,
  LandmarkTypeLeftPupil,
  LandmarkTypeRightPupil,
  LandmarkTypeUpperJawlineLeft,
  LandmarkTypeMidJawlineLeft,
  LandmarkTypeChinBottom,
  LandmarkTypeMidJawlineRight,
  LandmarkTypeUpperJawlineRight,
  LandmarkType'
  #-}
