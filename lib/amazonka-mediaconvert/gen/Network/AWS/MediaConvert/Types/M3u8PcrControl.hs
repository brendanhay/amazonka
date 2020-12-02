{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M3u8PcrControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M3u8PcrControl where

import Network.AWS.Prelude

-- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
data M3u8PcrControl
  = MPCConfiguredPcrPeriod
  | MPCPcrEveryPesPacket
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

instance FromText M3u8PcrControl where
  parser =
    takeLowerText >>= \case
      "configured_pcr_period" -> pure MPCConfiguredPcrPeriod
      "pcr_every_pes_packet" -> pure MPCPcrEveryPesPacket
      e ->
        fromTextError $
          "Failure parsing M3u8PcrControl from value: '" <> e
            <> "'. Accepted values: configured_pcr_period, pcr_every_pes_packet"

instance ToText M3u8PcrControl where
  toText = \case
    MPCConfiguredPcrPeriod -> "CONFIGURED_PCR_PERIOD"
    MPCPcrEveryPesPacket -> "PCR_EVERY_PES_PACKET"

instance Hashable M3u8PcrControl

instance NFData M3u8PcrControl

instance ToByteString M3u8PcrControl

instance ToQuery M3u8PcrControl

instance ToHeader M3u8PcrControl

instance ToJSON M3u8PcrControl where
  toJSON = toJSONText

instance FromJSON M3u8PcrControl where
  parseJSON = parseJSONText "M3u8PcrControl"
