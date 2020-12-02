{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsScte35Control
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsScte35Control where

import Network.AWS.Prelude

-- | M2ts Scte35 Control
data M2tsScte35Control
  = MSCNone
  | MSCPassthrough
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

instance FromText M2tsScte35Control where
  parser =
    takeLowerText >>= \case
      "none" -> pure MSCNone
      "passthrough" -> pure MSCPassthrough
      e ->
        fromTextError $
          "Failure parsing M2tsScte35Control from value: '" <> e
            <> "'. Accepted values: none, passthrough"

instance ToText M2tsScte35Control where
  toText = \case
    MSCNone -> "NONE"
    MSCPassthrough -> "PASSTHROUGH"

instance Hashable M2tsScte35Control

instance NFData M2tsScte35Control

instance ToByteString M2tsScte35Control

instance ToQuery M2tsScte35Control

instance ToHeader M2tsScte35Control

instance ToJSON M2tsScte35Control where
  toJSON = toJSONText

instance FromJSON M2tsScte35Control where
  parseJSON = parseJSONText "M2tsScte35Control"
