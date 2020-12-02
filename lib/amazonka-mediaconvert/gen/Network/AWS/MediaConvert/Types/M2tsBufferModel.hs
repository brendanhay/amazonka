{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsBufferModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsBufferModel where

import Network.AWS.Prelude

-- | Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
data M2tsBufferModel
  = MBMMultiplex
  | MBMNone
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

instance FromText M2tsBufferModel where
  parser =
    takeLowerText >>= \case
      "multiplex" -> pure MBMMultiplex
      "none" -> pure MBMNone
      e ->
        fromTextError $
          "Failure parsing M2tsBufferModel from value: '" <> e
            <> "'. Accepted values: multiplex, none"

instance ToText M2tsBufferModel where
  toText = \case
    MBMMultiplex -> "MULTIPLEX"
    MBMNone -> "NONE"

instance Hashable M2tsBufferModel

instance NFData M2tsBufferModel

instance ToByteString M2tsBufferModel

instance ToQuery M2tsBufferModel

instance ToHeader M2tsBufferModel

instance ToJSON M2tsBufferModel where
  toJSON = toJSONText

instance FromJSON M2tsBufferModel where
  parseJSON = parseJSONText "M2tsBufferModel"
