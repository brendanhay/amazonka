{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp8RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp8RateControlMode where

import Network.AWS.Prelude

-- | With the VP8 codec, you can use only the variable bitrate (VBR) rate control mode.
data Vp8RateControlMode = VRCMVbr
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

instance FromText Vp8RateControlMode where
  parser =
    takeLowerText >>= \case
      "vbr" -> pure VRCMVbr
      e ->
        fromTextError $
          "Failure parsing Vp8RateControlMode from value: '" <> e
            <> "'. Accepted values: vbr"

instance ToText Vp8RateControlMode where
  toText = \case
    VRCMVbr -> "VBR"

instance Hashable Vp8RateControlMode

instance NFData Vp8RateControlMode

instance ToByteString Vp8RateControlMode

instance ToQuery Vp8RateControlMode

instance ToHeader Vp8RateControlMode

instance ToJSON Vp8RateControlMode where
  toJSON = toJSONText

instance FromJSON Vp8RateControlMode where
  parseJSON = parseJSONText "Vp8RateControlMode"
