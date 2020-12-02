{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureIntervalUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FrameCaptureIntervalUnit where

import Network.AWS.Prelude

-- | Frame Capture Interval Unit
data FrameCaptureIntervalUnit
  = Milliseconds
  | Seconds
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

instance FromText FrameCaptureIntervalUnit where
  parser =
    takeLowerText >>= \case
      "milliseconds" -> pure Milliseconds
      "seconds" -> pure Seconds
      e ->
        fromTextError $
          "Failure parsing FrameCaptureIntervalUnit from value: '" <> e
            <> "'. Accepted values: milliseconds, seconds"

instance ToText FrameCaptureIntervalUnit where
  toText = \case
    Milliseconds -> "MILLISECONDS"
    Seconds -> "SECONDS"

instance Hashable FrameCaptureIntervalUnit

instance NFData FrameCaptureIntervalUnit

instance ToByteString FrameCaptureIntervalUnit

instance ToQuery FrameCaptureIntervalUnit

instance ToHeader FrameCaptureIntervalUnit

instance ToJSON FrameCaptureIntervalUnit where
  toJSON = toJSONText

instance FromJSON FrameCaptureIntervalUnit where
  parseJSON = parseJSONText "FrameCaptureIntervalUnit"
