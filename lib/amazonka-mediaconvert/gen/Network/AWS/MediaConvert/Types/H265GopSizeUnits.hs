{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265GopSizeUnits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265GopSizeUnits where

import Network.AWS.Prelude

-- | Indicates if the GOP Size in H265 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
data H265GopSizeUnits
  = Frames
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

instance FromText H265GopSizeUnits where
  parser =
    takeLowerText >>= \case
      "frames" -> pure Frames
      "seconds" -> pure Seconds
      e ->
        fromTextError $
          "Failure parsing H265GopSizeUnits from value: '" <> e
            <> "'. Accepted values: frames, seconds"

instance ToText H265GopSizeUnits where
  toText = \case
    Frames -> "FRAMES"
    Seconds -> "SECONDS"

instance Hashable H265GopSizeUnits

instance NFData H265GopSizeUnits

instance ToByteString H265GopSizeUnits

instance ToQuery H265GopSizeUnits

instance ToHeader H265GopSizeUnits

instance ToJSON H265GopSizeUnits where
  toJSON = toJSONText

instance FromJSON H265GopSizeUnits where
  parseJSON = parseJSONText "H265GopSizeUnits"
