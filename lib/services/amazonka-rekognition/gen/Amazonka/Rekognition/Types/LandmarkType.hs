{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Rekognition.Types.LandmarkType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.LandmarkType
  ( LandmarkType
      ( ..,
        LandmarkType_ChinBottom,
        LandmarkType_EyeLeft,
        LandmarkType_EyeRight,
        LandmarkType_LeftEyeBrowLeft,
        LandmarkType_LeftEyeBrowRight,
        LandmarkType_LeftEyeBrowUp,
        LandmarkType_LeftEyeDown,
        LandmarkType_LeftEyeLeft,
        LandmarkType_LeftEyeRight,
        LandmarkType_LeftEyeUp,
        LandmarkType_LeftPupil,
        LandmarkType_MidJawlineLeft,
        LandmarkType_MidJawlineRight,
        LandmarkType_MouthDown,
        LandmarkType_MouthLeft,
        LandmarkType_MouthRight,
        LandmarkType_MouthUp,
        LandmarkType_Nose,
        LandmarkType_NoseLeft,
        LandmarkType_NoseRight,
        LandmarkType_RightEyeBrowLeft,
        LandmarkType_RightEyeBrowRight,
        LandmarkType_RightEyeBrowUp,
        LandmarkType_RightEyeDown,
        LandmarkType_RightEyeLeft,
        LandmarkType_RightEyeRight,
        LandmarkType_RightEyeUp,
        LandmarkType_RightPupil,
        LandmarkType_UpperJawlineLeft,
        LandmarkType_UpperJawlineRight
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LandmarkType = LandmarkType'
  { fromLandmarkType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern LandmarkType_ChinBottom :: LandmarkType
pattern LandmarkType_ChinBottom = LandmarkType' "chinBottom"

pattern LandmarkType_EyeLeft :: LandmarkType
pattern LandmarkType_EyeLeft = LandmarkType' "eyeLeft"

pattern LandmarkType_EyeRight :: LandmarkType
pattern LandmarkType_EyeRight = LandmarkType' "eyeRight"

pattern LandmarkType_LeftEyeBrowLeft :: LandmarkType
pattern LandmarkType_LeftEyeBrowLeft = LandmarkType' "leftEyeBrowLeft"

pattern LandmarkType_LeftEyeBrowRight :: LandmarkType
pattern LandmarkType_LeftEyeBrowRight = LandmarkType' "leftEyeBrowRight"

pattern LandmarkType_LeftEyeBrowUp :: LandmarkType
pattern LandmarkType_LeftEyeBrowUp = LandmarkType' "leftEyeBrowUp"

pattern LandmarkType_LeftEyeDown :: LandmarkType
pattern LandmarkType_LeftEyeDown = LandmarkType' "leftEyeDown"

pattern LandmarkType_LeftEyeLeft :: LandmarkType
pattern LandmarkType_LeftEyeLeft = LandmarkType' "leftEyeLeft"

pattern LandmarkType_LeftEyeRight :: LandmarkType
pattern LandmarkType_LeftEyeRight = LandmarkType' "leftEyeRight"

pattern LandmarkType_LeftEyeUp :: LandmarkType
pattern LandmarkType_LeftEyeUp = LandmarkType' "leftEyeUp"

pattern LandmarkType_LeftPupil :: LandmarkType
pattern LandmarkType_LeftPupil = LandmarkType' "leftPupil"

pattern LandmarkType_MidJawlineLeft :: LandmarkType
pattern LandmarkType_MidJawlineLeft = LandmarkType' "midJawlineLeft"

pattern LandmarkType_MidJawlineRight :: LandmarkType
pattern LandmarkType_MidJawlineRight = LandmarkType' "midJawlineRight"

pattern LandmarkType_MouthDown :: LandmarkType
pattern LandmarkType_MouthDown = LandmarkType' "mouthDown"

pattern LandmarkType_MouthLeft :: LandmarkType
pattern LandmarkType_MouthLeft = LandmarkType' "mouthLeft"

pattern LandmarkType_MouthRight :: LandmarkType
pattern LandmarkType_MouthRight = LandmarkType' "mouthRight"

pattern LandmarkType_MouthUp :: LandmarkType
pattern LandmarkType_MouthUp = LandmarkType' "mouthUp"

pattern LandmarkType_Nose :: LandmarkType
pattern LandmarkType_Nose = LandmarkType' "nose"

pattern LandmarkType_NoseLeft :: LandmarkType
pattern LandmarkType_NoseLeft = LandmarkType' "noseLeft"

pattern LandmarkType_NoseRight :: LandmarkType
pattern LandmarkType_NoseRight = LandmarkType' "noseRight"

pattern LandmarkType_RightEyeBrowLeft :: LandmarkType
pattern LandmarkType_RightEyeBrowLeft = LandmarkType' "rightEyeBrowLeft"

pattern LandmarkType_RightEyeBrowRight :: LandmarkType
pattern LandmarkType_RightEyeBrowRight = LandmarkType' "rightEyeBrowRight"

pattern LandmarkType_RightEyeBrowUp :: LandmarkType
pattern LandmarkType_RightEyeBrowUp = LandmarkType' "rightEyeBrowUp"

pattern LandmarkType_RightEyeDown :: LandmarkType
pattern LandmarkType_RightEyeDown = LandmarkType' "rightEyeDown"

pattern LandmarkType_RightEyeLeft :: LandmarkType
pattern LandmarkType_RightEyeLeft = LandmarkType' "rightEyeLeft"

pattern LandmarkType_RightEyeRight :: LandmarkType
pattern LandmarkType_RightEyeRight = LandmarkType' "rightEyeRight"

pattern LandmarkType_RightEyeUp :: LandmarkType
pattern LandmarkType_RightEyeUp = LandmarkType' "rightEyeUp"

pattern LandmarkType_RightPupil :: LandmarkType
pattern LandmarkType_RightPupil = LandmarkType' "rightPupil"

pattern LandmarkType_UpperJawlineLeft :: LandmarkType
pattern LandmarkType_UpperJawlineLeft = LandmarkType' "upperJawlineLeft"

pattern LandmarkType_UpperJawlineRight :: LandmarkType
pattern LandmarkType_UpperJawlineRight = LandmarkType' "upperJawlineRight"

{-# COMPLETE
  LandmarkType_ChinBottom,
  LandmarkType_EyeLeft,
  LandmarkType_EyeRight,
  LandmarkType_LeftEyeBrowLeft,
  LandmarkType_LeftEyeBrowRight,
  LandmarkType_LeftEyeBrowUp,
  LandmarkType_LeftEyeDown,
  LandmarkType_LeftEyeLeft,
  LandmarkType_LeftEyeRight,
  LandmarkType_LeftEyeUp,
  LandmarkType_LeftPupil,
  LandmarkType_MidJawlineLeft,
  LandmarkType_MidJawlineRight,
  LandmarkType_MouthDown,
  LandmarkType_MouthLeft,
  LandmarkType_MouthRight,
  LandmarkType_MouthUp,
  LandmarkType_Nose,
  LandmarkType_NoseLeft,
  LandmarkType_NoseRight,
  LandmarkType_RightEyeBrowLeft,
  LandmarkType_RightEyeBrowRight,
  LandmarkType_RightEyeBrowUp,
  LandmarkType_RightEyeDown,
  LandmarkType_RightEyeLeft,
  LandmarkType_RightEyeRight,
  LandmarkType_RightEyeUp,
  LandmarkType_RightPupil,
  LandmarkType_UpperJawlineLeft,
  LandmarkType_UpperJawlineRight,
  LandmarkType'
  #-}
