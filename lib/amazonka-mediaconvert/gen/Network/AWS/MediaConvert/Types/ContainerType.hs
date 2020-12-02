{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ContainerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ContainerType where

import Network.AWS.Prelude

-- | Container for this output. Some containers require a container settings object. If not specified, the default object will be created.
data ContainerType
  = CTCmfc
  | CTF4V
  | CTIsmv
  | CTM2TS
  | CTM3U8
  | CTMP4
  | CTMov
  | CTMpd
  | CTMxf
  | CTRaw
  | CTWebm
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

instance FromText ContainerType where
  parser =
    takeLowerText >>= \case
      "cmfc" -> pure CTCmfc
      "f4v" -> pure CTF4V
      "ismv" -> pure CTIsmv
      "m2ts" -> pure CTM2TS
      "m3u8" -> pure CTM3U8
      "mp4" -> pure CTMP4
      "mov" -> pure CTMov
      "mpd" -> pure CTMpd
      "mxf" -> pure CTMxf
      "raw" -> pure CTRaw
      "webm" -> pure CTWebm
      e ->
        fromTextError $
          "Failure parsing ContainerType from value: '" <> e
            <> "'. Accepted values: cmfc, f4v, ismv, m2ts, m3u8, mp4, mov, mpd, mxf, raw, webm"

instance ToText ContainerType where
  toText = \case
    CTCmfc -> "CMFC"
    CTF4V -> "F4V"
    CTIsmv -> "ISMV"
    CTM2TS -> "M2TS"
    CTM3U8 -> "M3U8"
    CTMP4 -> "MP4"
    CTMov -> "MOV"
    CTMpd -> "MPD"
    CTMxf -> "MXF"
    CTRaw -> "RAW"
    CTWebm -> "WEBM"

instance Hashable ContainerType

instance NFData ContainerType

instance ToByteString ContainerType

instance ToQuery ContainerType

instance ToHeader ContainerType

instance ToJSON ContainerType where
  toJSON = toJSONText

instance FromJSON ContainerType where
  parseJSON = parseJSONText "ContainerType"
