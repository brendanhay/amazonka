{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdScte35Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdScte35Source where

import Network.AWS.Prelude

-- | Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
data MpdScte35Source
  = MpdNone
  | MpdPassthrough
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

instance FromText MpdScte35Source where
  parser =
    takeLowerText >>= \case
      "none" -> pure MpdNone
      "passthrough" -> pure MpdPassthrough
      e ->
        fromTextError $
          "Failure parsing MpdScte35Source from value: '" <> e
            <> "'. Accepted values: none, passthrough"

instance ToText MpdScte35Source where
  toText = \case
    MpdNone -> "NONE"
    MpdPassthrough -> "PASSTHROUGH"

instance Hashable MpdScte35Source

instance NFData MpdScte35Source

instance ToByteString MpdScte35Source

instance ToQuery MpdScte35Source

instance ToHeader MpdScte35Source

instance ToJSON MpdScte35Source where
  toJSON = toJSONText

instance FromJSON MpdScte35Source where
  parseJSON = parseJSONText "MpdScte35Source"
