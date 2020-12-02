{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2GopSizeUnits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2GopSizeUnits where

import Network.AWS.Prelude

-- | Mpeg2 Gop Size Units
data Mpeg2GopSizeUnits
  = MGSUFrames
  | MGSUSeconds
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

instance FromText Mpeg2GopSizeUnits where
  parser =
    takeLowerText >>= \case
      "frames" -> pure MGSUFrames
      "seconds" -> pure MGSUSeconds
      e ->
        fromTextError $
          "Failure parsing Mpeg2GopSizeUnits from value: '" <> e
            <> "'. Accepted values: frames, seconds"

instance ToText Mpeg2GopSizeUnits where
  toText = \case
    MGSUFrames -> "FRAMES"
    MGSUSeconds -> "SECONDS"

instance Hashable Mpeg2GopSizeUnits

instance NFData Mpeg2GopSizeUnits

instance ToByteString Mpeg2GopSizeUnits

instance ToQuery Mpeg2GopSizeUnits

instance ToHeader Mpeg2GopSizeUnits

instance ToJSON Mpeg2GopSizeUnits where
  toJSON = toJSONText

instance FromJSON Mpeg2GopSizeUnits where
  parseJSON = parseJSONText "Mpeg2GopSizeUnits"
