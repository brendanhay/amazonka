{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsPcrControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsPcrControl where

import Network.AWS.Prelude

-- | M2ts Pcr Control
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
