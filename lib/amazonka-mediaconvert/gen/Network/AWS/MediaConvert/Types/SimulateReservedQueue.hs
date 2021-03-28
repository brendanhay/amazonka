{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.SimulateReservedQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.SimulateReservedQueue
  ( SimulateReservedQueue
    ( SimulateReservedQueue'
    , SimulateReservedQueueDisabled
    , SimulateReservedQueueEnabled
    , fromSimulateReservedQueue
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
newtype SimulateReservedQueue = SimulateReservedQueue'{fromSimulateReservedQueue
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern SimulateReservedQueueDisabled :: SimulateReservedQueue
pattern SimulateReservedQueueDisabled = SimulateReservedQueue' "DISABLED"

pattern SimulateReservedQueueEnabled :: SimulateReservedQueue
pattern SimulateReservedQueueEnabled = SimulateReservedQueue' "ENABLED"

{-# COMPLETE 
  SimulateReservedQueueDisabled,

  SimulateReservedQueueEnabled,
  SimulateReservedQueue'
  #-}
