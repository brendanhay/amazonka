{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265TemporalIds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265TemporalIds where

import Network.AWS.Prelude

-- | Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.
data H265TemporalIds
  = HTIDisabled
  | HTIEnabled
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

instance FromText H265TemporalIds where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HTIDisabled
      "enabled" -> pure HTIEnabled
      e ->
        fromTextError $
          "Failure parsing H265TemporalIds from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H265TemporalIds where
  toText = \case
    HTIDisabled -> "DISABLED"
    HTIEnabled -> "ENABLED"

instance Hashable H265TemporalIds

instance NFData H265TemporalIds

instance ToByteString H265TemporalIds

instance ToQuery H265TemporalIds

instance ToHeader H265TemporalIds

instance ToJSON H265TemporalIds where
  toJSON = toJSONText

instance FromJSON H265TemporalIds where
  parseJSON = parseJSONText "H265TemporalIds"
