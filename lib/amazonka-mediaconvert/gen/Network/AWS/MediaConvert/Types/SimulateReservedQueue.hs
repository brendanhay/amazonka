{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.SimulateReservedQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SimulateReservedQueue where

import Network.AWS.Prelude

-- | Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
data SimulateReservedQueue
  = SRQDisabled
  | SRQEnabled
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

instance FromText SimulateReservedQueue where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure SRQDisabled
      "enabled" -> pure SRQEnabled
      e ->
        fromTextError $
          "Failure parsing SimulateReservedQueue from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText SimulateReservedQueue where
  toText = \case
    SRQDisabled -> "DISABLED"
    SRQEnabled -> "ENABLED"

instance Hashable SimulateReservedQueue

instance NFData SimulateReservedQueue

instance ToByteString SimulateReservedQueue

instance ToQuery SimulateReservedQueue

instance ToHeader SimulateReservedQueue

instance ToJSON SimulateReservedQueue where
  toJSON = toJSONText

instance FromJSON SimulateReservedQueue where
  parseJSON = parseJSONText "SimulateReservedQueue"
