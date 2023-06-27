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
-- Module      : Amazonka.Rekognition.Types.UnsearchedFaceReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.UnsearchedFaceReason
  ( UnsearchedFaceReason
      ( ..,
        UnsearchedFaceReason_EXCEEDS_MAX_FACES,
        UnsearchedFaceReason_EXTREME_POSE,
        UnsearchedFaceReason_FACE_NOT_LARGEST,
        UnsearchedFaceReason_LOW_BRIGHTNESS,
        UnsearchedFaceReason_LOW_CONFIDENCE,
        UnsearchedFaceReason_LOW_FACE_QUALITY,
        UnsearchedFaceReason_LOW_SHARPNESS,
        UnsearchedFaceReason_SMALL_BOUNDING_BOX
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UnsearchedFaceReason = UnsearchedFaceReason'
  { fromUnsearchedFaceReason ::
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

pattern UnsearchedFaceReason_EXCEEDS_MAX_FACES :: UnsearchedFaceReason
pattern UnsearchedFaceReason_EXCEEDS_MAX_FACES = UnsearchedFaceReason' "EXCEEDS_MAX_FACES"

pattern UnsearchedFaceReason_EXTREME_POSE :: UnsearchedFaceReason
pattern UnsearchedFaceReason_EXTREME_POSE = UnsearchedFaceReason' "EXTREME_POSE"

pattern UnsearchedFaceReason_FACE_NOT_LARGEST :: UnsearchedFaceReason
pattern UnsearchedFaceReason_FACE_NOT_LARGEST = UnsearchedFaceReason' "FACE_NOT_LARGEST"

pattern UnsearchedFaceReason_LOW_BRIGHTNESS :: UnsearchedFaceReason
pattern UnsearchedFaceReason_LOW_BRIGHTNESS = UnsearchedFaceReason' "LOW_BRIGHTNESS"

pattern UnsearchedFaceReason_LOW_CONFIDENCE :: UnsearchedFaceReason
pattern UnsearchedFaceReason_LOW_CONFIDENCE = UnsearchedFaceReason' "LOW_CONFIDENCE"

pattern UnsearchedFaceReason_LOW_FACE_QUALITY :: UnsearchedFaceReason
pattern UnsearchedFaceReason_LOW_FACE_QUALITY = UnsearchedFaceReason' "LOW_FACE_QUALITY"

pattern UnsearchedFaceReason_LOW_SHARPNESS :: UnsearchedFaceReason
pattern UnsearchedFaceReason_LOW_SHARPNESS = UnsearchedFaceReason' "LOW_SHARPNESS"

pattern UnsearchedFaceReason_SMALL_BOUNDING_BOX :: UnsearchedFaceReason
pattern UnsearchedFaceReason_SMALL_BOUNDING_BOX = UnsearchedFaceReason' "SMALL_BOUNDING_BOX"

{-# COMPLETE
  UnsearchedFaceReason_EXCEEDS_MAX_FACES,
  UnsearchedFaceReason_EXTREME_POSE,
  UnsearchedFaceReason_FACE_NOT_LARGEST,
  UnsearchedFaceReason_LOW_BRIGHTNESS,
  UnsearchedFaceReason_LOW_CONFIDENCE,
  UnsearchedFaceReason_LOW_FACE_QUALITY,
  UnsearchedFaceReason_LOW_SHARPNESS,
  UnsearchedFaceReason_SMALL_BOUNDING_BOX,
  UnsearchedFaceReason'
  #-}
