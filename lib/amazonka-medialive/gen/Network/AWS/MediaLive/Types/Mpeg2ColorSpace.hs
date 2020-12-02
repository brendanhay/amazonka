{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2ColorSpace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2ColorSpace where

import Network.AWS.Prelude

-- | Mpeg2 Color Space
data Mpeg2ColorSpace
  = MCSAuto
  | MCSPassthrough
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

instance FromText Mpeg2ColorSpace where
  parser =
    takeLowerText >>= \case
      "auto" -> pure MCSAuto
      "passthrough" -> pure MCSPassthrough
      e ->
        fromTextError $
          "Failure parsing Mpeg2ColorSpace from value: '" <> e
            <> "'. Accepted values: auto, passthrough"

instance ToText Mpeg2ColorSpace where
  toText = \case
    MCSAuto -> "AUTO"
    MCSPassthrough -> "PASSTHROUGH"

instance Hashable Mpeg2ColorSpace

instance NFData Mpeg2ColorSpace

instance ToByteString Mpeg2ColorSpace

instance ToQuery Mpeg2ColorSpace

instance ToHeader Mpeg2ColorSpace

instance ToJSON Mpeg2ColorSpace where
  toJSON = toJSONText

instance FromJSON Mpeg2ColorSpace where
  parseJSON = parseJSONText "Mpeg2ColorSpace"
