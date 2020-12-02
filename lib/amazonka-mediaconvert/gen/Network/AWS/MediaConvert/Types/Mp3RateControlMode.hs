{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp3RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp3RateControlMode where

import Network.AWS.Prelude

-- | Specify whether the service encodes this MP3 audio output with a constant bitrate (CBR) or a variable bitrate (VBR).
data Mp3RateControlMode
  = MCbr
  | MVbr
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

instance FromText Mp3RateControlMode where
  parser =
    takeLowerText >>= \case
      "cbr" -> pure MCbr
      "vbr" -> pure MVbr
      e ->
        fromTextError $
          "Failure parsing Mp3RateControlMode from value: '" <> e
            <> "'. Accepted values: cbr, vbr"

instance ToText Mp3RateControlMode where
  toText = \case
    MCbr -> "CBR"
    MVbr -> "VBR"

instance Hashable Mp3RateControlMode

instance NFData Mp3RateControlMode

instance ToByteString Mp3RateControlMode

instance ToQuery Mp3RateControlMode

instance ToHeader Mp3RateControlMode

instance ToJSON Mp3RateControlMode where
  toJSON = toJSONText

instance FromJSON Mp3RateControlMode where
  parseJSON = parseJSONText "Mp3RateControlMode"
