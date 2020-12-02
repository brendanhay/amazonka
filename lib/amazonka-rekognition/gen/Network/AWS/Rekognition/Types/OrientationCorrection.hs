{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.OrientationCorrection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.OrientationCorrection where

import Network.AWS.Prelude

data OrientationCorrection
  = Rotate0
  | Rotate180
  | Rotate270
  | Rotate90
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

instance FromText OrientationCorrection where
  parser =
    takeLowerText >>= \case
      "rotate_0" -> pure Rotate0
      "rotate_180" -> pure Rotate180
      "rotate_270" -> pure Rotate270
      "rotate_90" -> pure Rotate90
      e ->
        fromTextError $
          "Failure parsing OrientationCorrection from value: '" <> e
            <> "'. Accepted values: rotate_0, rotate_180, rotate_270, rotate_90"

instance ToText OrientationCorrection where
  toText = \case
    Rotate0 -> "ROTATE_0"
    Rotate180 -> "ROTATE_180"
    Rotate270 -> "ROTATE_270"
    Rotate90 -> "ROTATE_90"

instance Hashable OrientationCorrection

instance NFData OrientationCorrection

instance ToByteString OrientationCorrection

instance ToQuery OrientationCorrection

instance ToHeader OrientationCorrection

instance FromJSON OrientationCorrection where
  parseJSON = parseJSONText "OrientationCorrection"
