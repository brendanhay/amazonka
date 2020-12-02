{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode where

import Network.AWS.Prelude

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
data H264UnregisteredSeiTimecode
  = HDisabled
  | HEnabled
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

instance FromText H264UnregisteredSeiTimecode where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HDisabled
      "enabled" -> pure HEnabled
      e ->
        fromTextError $
          "Failure parsing H264UnregisteredSeiTimecode from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H264UnregisteredSeiTimecode where
  toText = \case
    HDisabled -> "DISABLED"
    HEnabled -> "ENABLED"

instance Hashable H264UnregisteredSeiTimecode

instance NFData H264UnregisteredSeiTimecode

instance ToByteString H264UnregisteredSeiTimecode

instance ToQuery H264UnregisteredSeiTimecode

instance ToHeader H264UnregisteredSeiTimecode

instance ToJSON H264UnregisteredSeiTimecode where
  toJSON = toJSONText

instance FromJSON H264UnregisteredSeiTimecode where
  parseJSON = parseJSONText "H264UnregisteredSeiTimecode"
