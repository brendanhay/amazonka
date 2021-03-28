{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M3u8PcrControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.M3u8PcrControl
  ( M3u8PcrControl
    ( M3u8PcrControl'
    , M3u8PcrControlPcrEveryPesPacket
    , M3u8PcrControlConfiguredPcrPeriod
    , fromM3u8PcrControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
newtype M3u8PcrControl = M3u8PcrControl'{fromM3u8PcrControl ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern M3u8PcrControlPcrEveryPesPacket :: M3u8PcrControl
pattern M3u8PcrControlPcrEveryPesPacket = M3u8PcrControl' "PCR_EVERY_PES_PACKET"

pattern M3u8PcrControlConfiguredPcrPeriod :: M3u8PcrControl
pattern M3u8PcrControlConfiguredPcrPeriod = M3u8PcrControl' "CONFIGURED_PCR_PERIOD"

{-# COMPLETE 
  M3u8PcrControlPcrEveryPesPacket,

  M3u8PcrControlConfiguredPcrPeriod,
  M3u8PcrControl'
  #-}
