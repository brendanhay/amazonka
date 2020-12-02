{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsPcrControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsPcrControl where

import Network.AWS.Prelude

-- | When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This is effective only when the PCR PID is the same as the video or audio elementary stream.
data M2tsPcrControl
  = ConfiguredPcrPeriod
  | PcrEveryPesPacket
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

instance FromText M2tsPcrControl where
  parser =
    takeLowerText >>= \case
      "configured_pcr_period" -> pure ConfiguredPcrPeriod
      "pcr_every_pes_packet" -> pure PcrEveryPesPacket
      e ->
        fromTextError $
          "Failure parsing M2tsPcrControl from value: '" <> e
            <> "'. Accepted values: configured_pcr_period, pcr_every_pes_packet"

instance ToText M2tsPcrControl where
  toText = \case
    ConfiguredPcrPeriod -> "CONFIGURED_PCR_PERIOD"
    PcrEveryPesPacket -> "PCR_EVERY_PES_PACKET"

instance Hashable M2tsPcrControl

instance NFData M2tsPcrControl

instance ToByteString M2tsPcrControl

instance ToQuery M2tsPcrControl

instance ToHeader M2tsPcrControl

instance ToJSON M2tsPcrControl where
  toJSON = toJSONText

instance FromJSON M2tsPcrControl where
  parseJSON = parseJSONText "M2tsPcrControl"
