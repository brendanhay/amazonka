{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MotionImageInsertionMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MotionImageInsertionMode where

import Network.AWS.Prelude

-- | Choose the type of motion graphic asset that you are providing for your overlay. You can choose either a .mov file or a series of .png files.
data MotionImageInsertionMode
  = Mov
  | Png
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

instance FromText MotionImageInsertionMode where
  parser =
    takeLowerText >>= \case
      "mov" -> pure Mov
      "png" -> pure Png
      e ->
        fromTextError $
          "Failure parsing MotionImageInsertionMode from value: '" <> e
            <> "'. Accepted values: mov, png"

instance ToText MotionImageInsertionMode where
  toText = \case
    Mov -> "MOV"
    Png -> "PNG"

instance Hashable MotionImageInsertionMode

instance NFData MotionImageInsertionMode

instance ToByteString MotionImageInsertionMode

instance ToQuery MotionImageInsertionMode

instance ToHeader MotionImageInsertionMode

instance ToJSON MotionImageInsertionMode where
  toJSON = toJSONText

instance FromJSON MotionImageInsertionMode where
  parseJSON = parseJSONText "MotionImageInsertionMode"
