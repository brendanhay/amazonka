{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.SegmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.SegmentType where

import Network.AWS.Prelude

data SegmentType
  = Shot
  | TechnicalCue
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

instance FromText SegmentType where
  parser =
    takeLowerText >>= \case
      "shot" -> pure Shot
      "technical_cue" -> pure TechnicalCue
      e ->
        fromTextError $
          "Failure parsing SegmentType from value: '" <> e
            <> "'. Accepted values: shot, technical_cue"

instance ToText SegmentType where
  toText = \case
    Shot -> "SHOT"
    TechnicalCue -> "TECHNICAL_CUE"

instance Hashable SegmentType

instance NFData SegmentType

instance ToByteString SegmentType

instance ToQuery SegmentType

instance ToHeader SegmentType

instance ToJSON SegmentType where
  toJSON = toJSONText

instance FromJSON SegmentType where
  parseJSON = parseJSONText "SegmentType"
