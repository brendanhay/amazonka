{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265GopSizeUnits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265GopSizeUnits where

import Network.AWS.Prelude

-- | H265 Gop Size Units
data H265GopSizeUnits
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

instance FromText H265GopSizeUnits where
  parser =
    takeLowerText >>= \case
      "frames" -> pure HGSUFrames
      "seconds" -> pure HGSUSeconds
      e ->
        fromTextError $
          "Failure parsing H265GopSizeUnits from value: '" <> e
            <> "'. Accepted values: frames, seconds"

instance ToText H265GopSizeUnits where
  toText = \case
    HGSUFrames -> "FRAMES"
    HGSUSeconds -> "SECONDS"

instance Hashable H265GopSizeUnits

instance NFData H265GopSizeUnits

instance ToByteString H265GopSizeUnits

instance ToQuery H265GopSizeUnits

instance ToHeader H265GopSizeUnits

instance ToJSON H265GopSizeUnits where
  toJSON = toJSONText

instance FromJSON H265GopSizeUnits where
  parseJSON = parseJSONText "H265GopSizeUnits"
