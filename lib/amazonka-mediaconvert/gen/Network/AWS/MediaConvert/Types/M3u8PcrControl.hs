-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M3u8PcrControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M3u8PcrControl
  ( M3u8PcrControl
      ( M3u8PcrControl',
        MPCConfiguredPcrPeriod,
        MPCPcrEveryPesPacket
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
newtype M3u8PcrControl = M3u8PcrControl' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern MPCConfiguredPcrPeriod :: M3u8PcrControl
pattern MPCConfiguredPcrPeriod = M3u8PcrControl' "CONFIGURED_PCR_PERIOD"

pattern MPCPcrEveryPesPacket :: M3u8PcrControl
pattern MPCPcrEveryPesPacket = M3u8PcrControl' "PCR_EVERY_PES_PACKET"

{-# COMPLETE
  MPCConfiguredPcrPeriod,
  MPCPcrEveryPesPacket,
  M3u8PcrControl'
  #-}
