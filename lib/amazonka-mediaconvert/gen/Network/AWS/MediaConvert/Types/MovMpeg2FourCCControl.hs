{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl where

import Network.AWS.Prelude

-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
data MovMpeg2FourCCControl
  = Mpeg
  | Xdcam
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

instance FromText MovMpeg2FourCCControl where
  parser =
    takeLowerText >>= \case
      "mpeg" -> pure Mpeg
      "xdcam" -> pure Xdcam
      e ->
        fromTextError $
          "Failure parsing MovMpeg2FourCCControl from value: '" <> e
            <> "'. Accepted values: mpeg, xdcam"

instance ToText MovMpeg2FourCCControl where
  toText = \case
    Mpeg -> "MPEG"
    Xdcam -> "XDCAM"

instance Hashable MovMpeg2FourCCControl

instance NFData MovMpeg2FourCCControl

instance ToByteString MovMpeg2FourCCControl

instance ToQuery MovMpeg2FourCCControl

instance ToHeader MovMpeg2FourCCControl

instance ToJSON MovMpeg2FourCCControl where
  toJSON = toJSONText

instance FromJSON MovMpeg2FourCCControl where
  parseJSON = parseJSONText "MovMpeg2FourCCControl"
