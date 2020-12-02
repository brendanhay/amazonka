{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264GopSizeUnits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264GopSizeUnits where

import Network.AWS.Prelude

-- | Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
data H264GopSizeUnits
  = HGSUFrames
  | HGSUSeconds
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

instance FromText H264GopSizeUnits where
  parser =
    takeLowerText >>= \case
      "frames" -> pure HGSUFrames
      "seconds" -> pure HGSUSeconds
      e ->
        fromTextError $
          "Failure parsing H264GopSizeUnits from value: '" <> e
            <> "'. Accepted values: frames, seconds"

instance ToText H264GopSizeUnits where
  toText = \case
    HGSUFrames -> "FRAMES"
    HGSUSeconds -> "SECONDS"

instance Hashable H264GopSizeUnits

instance NFData H264GopSizeUnits

instance ToByteString H264GopSizeUnits

instance ToQuery H264GopSizeUnits

instance ToHeader H264GopSizeUnits

instance ToJSON H264GopSizeUnits where
  toJSON = toJSONText

instance FromJSON H264GopSizeUnits where
  parseJSON = parseJSONText "H264GopSizeUnits"
