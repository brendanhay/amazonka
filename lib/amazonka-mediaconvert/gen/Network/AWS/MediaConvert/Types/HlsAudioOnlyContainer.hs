{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer where

import Network.AWS.Prelude

-- | Use this setting only in audio-only outputs. Choose MPEG-2 Transport Stream (M2TS) to create a file in an MPEG2-TS container. Keep the default value Automatic (AUTOMATIC) to create a raw audio-only file with no container. Regardless of the value that you specify here, if this output has video, the service will place outputs into an MPEG2-TS container.
data HlsAudioOnlyContainer
  = HAOCAutomatic
  | HAOCM2TS
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

instance FromText HlsAudioOnlyContainer where
  parser =
    takeLowerText >>= \case
      "automatic" -> pure HAOCAutomatic
      "m2ts" -> pure HAOCM2TS
      e ->
        fromTextError $
          "Failure parsing HlsAudioOnlyContainer from value: '" <> e
            <> "'. Accepted values: automatic, m2ts"

instance ToText HlsAudioOnlyContainer where
  toText = \case
    HAOCAutomatic -> "AUTOMATIC"
    HAOCM2TS -> "M2TS"

instance Hashable HlsAudioOnlyContainer

instance NFData HlsAudioOnlyContainer

instance ToByteString HlsAudioOnlyContainer

instance ToQuery HlsAudioOnlyContainer

instance ToHeader HlsAudioOnlyContainer

instance ToJSON HlsAudioOnlyContainer where
  toJSON = toJSONText

instance FromJSON HlsAudioOnlyContainer where
  parseJSON = parseJSONText "HlsAudioOnlyContainer"
