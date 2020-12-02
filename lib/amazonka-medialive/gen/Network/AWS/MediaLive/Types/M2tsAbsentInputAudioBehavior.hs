{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsAbsentInputAudioBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsAbsentInputAudioBehavior where

import Network.AWS.Prelude

-- | M2ts Absent Input Audio Behavior
data M2tsAbsentInputAudioBehavior
  = Drop
  | EncodeSilence
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

instance FromText M2tsAbsentInputAudioBehavior where
  parser =
    takeLowerText >>= \case
      "drop" -> pure Drop
      "encode_silence" -> pure EncodeSilence
      e ->
        fromTextError $
          "Failure parsing M2tsAbsentInputAudioBehavior from value: '" <> e
            <> "'. Accepted values: drop, encode_silence"

instance ToText M2tsAbsentInputAudioBehavior where
  toText = \case
    Drop -> "DROP"
    EncodeSilence -> "ENCODE_SILENCE"

instance Hashable M2tsAbsentInputAudioBehavior

instance NFData M2tsAbsentInputAudioBehavior

instance ToByteString M2tsAbsentInputAudioBehavior

instance ToQuery M2tsAbsentInputAudioBehavior

instance ToHeader M2tsAbsentInputAudioBehavior

instance ToJSON M2tsAbsentInputAudioBehavior where
  toJSON = toJSONText

instance FromJSON M2tsAbsentInputAudioBehavior where
  parseJSON = parseJSONText "M2tsAbsentInputAudioBehavior"
