{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsAudioStreamType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsAudioStreamType where

import Network.AWS.Prelude

-- | M2ts Audio Stream Type
data M2tsAudioStreamType
  = MASTAtsc
  | MASTDvb
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

instance FromText M2tsAudioStreamType where
  parser =
    takeLowerText >>= \case
      "atsc" -> pure MASTAtsc
      "dvb" -> pure MASTDvb
      e ->
        fromTextError $
          "Failure parsing M2tsAudioStreamType from value: '" <> e
            <> "'. Accepted values: atsc, dvb"

instance ToText M2tsAudioStreamType where
  toText = \case
    MASTAtsc -> "ATSC"
    MASTDvb -> "DVB"

instance Hashable M2tsAudioStreamType

instance NFData M2tsAudioStreamType

instance ToByteString M2tsAudioStreamType

instance ToQuery M2tsAudioStreamType

instance ToHeader M2tsAudioStreamType

instance ToJSON M2tsAudioStreamType where
  toJSON = toJSONText

instance FromJSON M2tsAudioStreamType where
  parseJSON = parseJSONText "M2tsAudioStreamType"
