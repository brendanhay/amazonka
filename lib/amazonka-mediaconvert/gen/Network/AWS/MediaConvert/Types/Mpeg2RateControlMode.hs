{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2RateControlMode where

import Network.AWS.Prelude

-- | Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
data Mpeg2RateControlMode
  = MRCMCbr
  | MRCMVbr
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

instance FromText Mpeg2RateControlMode where
  parser =
    takeLowerText >>= \case
      "cbr" -> pure MRCMCbr
      "vbr" -> pure MRCMVbr
      e ->
        fromTextError $
          "Failure parsing Mpeg2RateControlMode from value: '" <> e
            <> "'. Accepted values: cbr, vbr"

instance ToText Mpeg2RateControlMode where
  toText = \case
    MRCMCbr -> "CBR"
    MRCMVbr -> "VBR"

instance Hashable Mpeg2RateControlMode

instance NFData Mpeg2RateControlMode

instance ToByteString Mpeg2RateControlMode

instance ToQuery Mpeg2RateControlMode

instance ToHeader Mpeg2RateControlMode

instance ToJSON Mpeg2RateControlMode where
  toJSON = toJSONText

instance FromJSON Mpeg2RateControlMode where
  parseJSON = parseJSONText "Mpeg2RateControlMode"
