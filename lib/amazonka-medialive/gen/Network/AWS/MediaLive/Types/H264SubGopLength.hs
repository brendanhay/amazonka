{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264SubGopLength
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264SubGopLength where

import Network.AWS.Prelude

-- | H264 Sub Gop Length
data H264SubGopLength
  = Dynamic
  | Fixed
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

instance FromText H264SubGopLength where
  parser =
    takeLowerText >>= \case
      "dynamic" -> pure Dynamic
      "fixed" -> pure Fixed
      e ->
        fromTextError $
          "Failure parsing H264SubGopLength from value: '" <> e
            <> "'. Accepted values: dynamic, fixed"

instance ToText H264SubGopLength where
  toText = \case
    Dynamic -> "DYNAMIC"
    Fixed -> "FIXED"

instance Hashable H264SubGopLength

instance NFData H264SubGopLength

instance ToByteString H264SubGopLength

instance ToQuery H264SubGopLength

instance ToHeader H264SubGopLength

instance ToJSON H264SubGopLength where
  toJSON = toJSONText

instance FromJSON H264SubGopLength where
  parseJSON = parseJSONText "H264SubGopLength"
