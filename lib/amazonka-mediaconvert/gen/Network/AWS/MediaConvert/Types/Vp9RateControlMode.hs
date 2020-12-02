{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp9RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp9RateControlMode where

import Network.AWS.Prelude

-- | With the VP9 codec, you can use only the variable bitrate (VBR) rate control mode.
data Vp9RateControlMode = Vbr
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

instance FromText Vp9RateControlMode where
  parser =
    takeLowerText >>= \case
      "vbr" -> pure Vbr
      e ->
        fromTextError $
          "Failure parsing Vp9RateControlMode from value: '" <> e
            <> "'. Accepted values: vbr"

instance ToText Vp9RateControlMode where
  toText = \case
    Vbr -> "VBR"

instance Hashable Vp9RateControlMode

instance NFData Vp9RateControlMode

instance ToByteString Vp9RateControlMode

instance ToQuery Vp9RateControlMode

instance ToHeader Vp9RateControlMode

instance ToJSON Vp9RateControlMode where
  toJSON = toJSONText

instance FromJSON Vp9RateControlMode where
  parseJSON = parseJSONText "Vp9RateControlMode"
