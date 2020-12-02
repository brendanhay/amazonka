{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264EntropyEncoding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264EntropyEncoding where

import Network.AWS.Prelude

-- | H264 Entropy Encoding
data H264EntropyEncoding
  = Cabac
  | Cavlc
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

instance FromText H264EntropyEncoding where
  parser =
    takeLowerText >>= \case
      "cabac" -> pure Cabac
      "cavlc" -> pure Cavlc
      e ->
        fromTextError $
          "Failure parsing H264EntropyEncoding from value: '" <> e
            <> "'. Accepted values: cabac, cavlc"

instance ToText H264EntropyEncoding where
  toText = \case
    Cabac -> "CABAC"
    Cavlc -> "CAVLC"

instance Hashable H264EntropyEncoding

instance NFData H264EntropyEncoding

instance ToByteString H264EntropyEncoding

instance ToQuery H264EntropyEncoding

instance ToHeader H264EntropyEncoding

instance ToJSON H264EntropyEncoding where
  toJSON = toJSONText

instance FromJSON H264EntropyEncoding where
  parseJSON = parseJSONText "H264EntropyEncoding"
