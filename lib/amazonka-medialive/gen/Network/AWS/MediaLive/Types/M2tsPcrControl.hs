{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsPcrControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsPcrControl
  ( M2tsPcrControl
    ( M2tsPcrControl'
    , M2tsPcrControlConfiguredPcrPeriod
    , M2tsPcrControlPcrEveryPesPacket
    , fromM2tsPcrControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | M2ts Pcr Control
newtype M2tsPcrControl = M2tsPcrControl'{fromM2tsPcrControl ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern M2tsPcrControlConfiguredPcrPeriod :: M2tsPcrControl
pattern M2tsPcrControlConfiguredPcrPeriod = M2tsPcrControl' "CONFIGURED_PCR_PERIOD"

pattern M2tsPcrControlPcrEveryPesPacket :: M2tsPcrControl
pattern M2tsPcrControlPcrEveryPesPacket = M2tsPcrControl' "PCR_EVERY_PES_PACKET"

{-# COMPLETE 
  M2tsPcrControlConfiguredPcrPeriod,

  M2tsPcrControlPcrEveryPesPacket,
  M2tsPcrControl'
  #-}
