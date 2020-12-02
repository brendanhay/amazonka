{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vc3Telecine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vc3Telecine where

import Network.AWS.Prelude

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
data Vc3Telecine
  = VTHard
  | VTNone
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

instance FromText Vc3Telecine where
  parser =
    takeLowerText >>= \case
      "hard" -> pure VTHard
      "none" -> pure VTNone
      e ->
        fromTextError $
          "Failure parsing Vc3Telecine from value: '" <> e
            <> "'. Accepted values: hard, none"

instance ToText Vc3Telecine where
  toText = \case
    VTHard -> "HARD"
    VTNone -> "NONE"

instance Hashable Vc3Telecine

instance NFData Vc3Telecine

instance ToByteString Vc3Telecine

instance ToQuery Vc3Telecine

instance ToHeader Vc3Telecine

instance ToJSON Vc3Telecine where
  toJSON = toJSONText

instance FromJSON Vc3Telecine where
  parseJSON = parseJSONText "Vc3Telecine"
