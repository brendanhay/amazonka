{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsAudioBufferModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsAudioBufferModel where

import Network.AWS.Prelude

-- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
data M2tsAudioBufferModel
  = Atsc
  | Dvb
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

instance FromText M2tsAudioBufferModel where
  parser =
    takeLowerText >>= \case
      "atsc" -> pure Atsc
      "dvb" -> pure Dvb
      e ->
        fromTextError $
          "Failure parsing M2tsAudioBufferModel from value: '" <> e
            <> "'. Accepted values: atsc, dvb"

instance ToText M2tsAudioBufferModel where
  toText = \case
    Atsc -> "ATSC"
    Dvb -> "DVB"

instance Hashable M2tsAudioBufferModel

instance NFData M2tsAudioBufferModel

instance ToByteString M2tsAudioBufferModel

instance ToQuery M2tsAudioBufferModel

instance ToHeader M2tsAudioBufferModel

instance ToJSON M2tsAudioBufferModel where
  toJSON = toJSONText

instance FromJSON M2tsAudioBufferModel where
  parseJSON = parseJSONText "M2tsAudioBufferModel"
