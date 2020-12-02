{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AvcIntraTelecine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvcIntraTelecine where

import Network.AWS.Prelude

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
data AvcIntraTelecine
  = AITHard
  | AITNone
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

instance FromText AvcIntraTelecine where
  parser =
    takeLowerText >>= \case
      "hard" -> pure AITHard
      "none" -> pure AITNone
      e ->
        fromTextError $
          "Failure parsing AvcIntraTelecine from value: '" <> e
            <> "'. Accepted values: hard, none"

instance ToText AvcIntraTelecine where
  toText = \case
    AITHard -> "HARD"
    AITNone -> "NONE"

instance Hashable AvcIntraTelecine

instance NFData AvcIntraTelecine

instance ToByteString AvcIntraTelecine

instance ToQuery AvcIntraTelecine

instance ToHeader AvcIntraTelecine

instance ToJSON AvcIntraTelecine where
  toJSON = toJSONText

instance FromJSON AvcIntraTelecine where
  parseJSON = parseJSONText "AvcIntraTelecine"
