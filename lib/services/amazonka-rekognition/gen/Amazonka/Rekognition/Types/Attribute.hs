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
-- Module      : Amazonka.Rekognition.Types.Attribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Attribute
  ( Attribute
      ( ..,
        Attribute_AGE_RANGE,
        Attribute_ALL,
        Attribute_BEARD,
        Attribute_DEFAULT,
        Attribute_EMOTIONS,
        Attribute_EYEGLASSES,
        Attribute_EYES_OPEN,
        Attribute_EYE_DIRECTION,
        Attribute_FACE_OCCLUDED,
        Attribute_GENDER,
        Attribute_MOUTH_OPEN,
        Attribute_MUSTACHE,
        Attribute_SMILE,
        Attribute_SUNGLASSES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Attribute = Attribute'
  { fromAttribute ::
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

pattern Attribute_AGE_RANGE :: Attribute
pattern Attribute_AGE_RANGE = Attribute' "AGE_RANGE"

pattern Attribute_ALL :: Attribute
pattern Attribute_ALL = Attribute' "ALL"

pattern Attribute_BEARD :: Attribute
pattern Attribute_BEARD = Attribute' "BEARD"

pattern Attribute_DEFAULT :: Attribute
pattern Attribute_DEFAULT = Attribute' "DEFAULT"

pattern Attribute_EMOTIONS :: Attribute
pattern Attribute_EMOTIONS = Attribute' "EMOTIONS"

pattern Attribute_EYEGLASSES :: Attribute
pattern Attribute_EYEGLASSES = Attribute' "EYEGLASSES"

pattern Attribute_EYES_OPEN :: Attribute
pattern Attribute_EYES_OPEN = Attribute' "EYES_OPEN"

pattern Attribute_EYE_DIRECTION :: Attribute
pattern Attribute_EYE_DIRECTION = Attribute' "EYE_DIRECTION"

pattern Attribute_FACE_OCCLUDED :: Attribute
pattern Attribute_FACE_OCCLUDED = Attribute' "FACE_OCCLUDED"

pattern Attribute_GENDER :: Attribute
pattern Attribute_GENDER = Attribute' "GENDER"

pattern Attribute_MOUTH_OPEN :: Attribute
pattern Attribute_MOUTH_OPEN = Attribute' "MOUTH_OPEN"

pattern Attribute_MUSTACHE :: Attribute
pattern Attribute_MUSTACHE = Attribute' "MUSTACHE"

pattern Attribute_SMILE :: Attribute
pattern Attribute_SMILE = Attribute' "SMILE"

pattern Attribute_SUNGLASSES :: Attribute
pattern Attribute_SUNGLASSES = Attribute' "SUNGLASSES"

{-# COMPLETE
  Attribute_AGE_RANGE,
  Attribute_ALL,
  Attribute_BEARD,
  Attribute_DEFAULT,
  Attribute_EMOTIONS,
  Attribute_EYEGLASSES,
  Attribute_EYES_OPEN,
  Attribute_EYE_DIRECTION,
  Attribute_FACE_OCCLUDED,
  Attribute_GENDER,
  Attribute_MOUTH_OPEN,
  Attribute_MUSTACHE,
  Attribute_SMILE,
  Attribute_SUNGLASSES,
  Attribute'
  #-}
