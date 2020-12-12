{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsPcrControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsPcrControl
  ( M2tsPcrControl
      ( M2tsPcrControl',
        ConfiguredPcrPeriod,
        PcrEveryPesPacket
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This is effective only when the PCR PID is the same as the video or audio elementary stream.
newtype M2tsPcrControl = M2tsPcrControl' Lude.Text
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

pattern ConfiguredPcrPeriod :: M2tsPcrControl
pattern ConfiguredPcrPeriod = M2tsPcrControl' "CONFIGURED_PCR_PERIOD"

pattern PcrEveryPesPacket :: M2tsPcrControl
pattern PcrEveryPesPacket = M2tsPcrControl' "PCR_EVERY_PES_PACKET"

{-# COMPLETE
  ConfiguredPcrPeriod,
  PcrEveryPesPacket,
  M2tsPcrControl'
  #-}
