{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputRotate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputRotate where

import Network.AWS.Prelude

-- | Use Rotate (InputRotate) to specify how the service rotates your video. You can choose automatic rotation or specify a rotation. You can specify a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video container is .mov or .mp4 and your input has rotation metadata, you can choose Automatic to have the service rotate your video according to the rotation specified in the metadata. The rotation must be within one degree of 90, 180, or 270 degrees. If the rotation metadata specifies any other rotation, the service will default to no rotation. By default, the service does no rotation, even if your input video has rotation metadata. The service doesn't pass through rotation metadata.
data InputRotate
  = Auto
  | Degree0
  | Degrees180
  | Degrees270
  | Degrees90
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

instance FromText InputRotate where
  parser =
    takeLowerText >>= \case
      "auto" -> pure Auto
      "degree_0" -> pure Degree0
      "degrees_180" -> pure Degrees180
      "degrees_270" -> pure Degrees270
      "degrees_90" -> pure Degrees90
      e ->
        fromTextError $
          "Failure parsing InputRotate from value: '" <> e
            <> "'. Accepted values: auto, degree_0, degrees_180, degrees_270, degrees_90"

instance ToText InputRotate where
  toText = \case
    Auto -> "AUTO"
    Degree0 -> "DEGREE_0"
    Degrees180 -> "DEGREES_180"
    Degrees270 -> "DEGREES_270"
    Degrees90 -> "DEGREES_90"

instance Hashable InputRotate

instance NFData InputRotate

instance ToByteString InputRotate

instance ToQuery InputRotate

instance ToHeader InputRotate

instance ToJSON InputRotate where
  toJSON = toJSONText

instance FromJSON InputRotate where
  parseJSON = parseJSONText "InputRotate"
