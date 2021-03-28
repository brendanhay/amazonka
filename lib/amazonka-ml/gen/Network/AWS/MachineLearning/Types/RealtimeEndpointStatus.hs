{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RealtimeEndpointStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.RealtimeEndpointStatus
  ( RealtimeEndpointStatus
    ( RealtimeEndpointStatus'
    , RealtimeEndpointStatusNone
    , RealtimeEndpointStatusReady
    , RealtimeEndpointStatusUpdating
    , RealtimeEndpointStatusFailed
    , fromRealtimeEndpointStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype RealtimeEndpointStatus = RealtimeEndpointStatus'{fromRealtimeEndpointStatus
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern RealtimeEndpointStatusNone :: RealtimeEndpointStatus
pattern RealtimeEndpointStatusNone = RealtimeEndpointStatus' "NONE"

pattern RealtimeEndpointStatusReady :: RealtimeEndpointStatus
pattern RealtimeEndpointStatusReady = RealtimeEndpointStatus' "READY"

pattern RealtimeEndpointStatusUpdating :: RealtimeEndpointStatus
pattern RealtimeEndpointStatusUpdating = RealtimeEndpointStatus' "UPDATING"

pattern RealtimeEndpointStatusFailed :: RealtimeEndpointStatus
pattern RealtimeEndpointStatusFailed = RealtimeEndpointStatus' "FAILED"

{-# COMPLETE 
  RealtimeEndpointStatusNone,

  RealtimeEndpointStatusReady,

  RealtimeEndpointStatusUpdating,

  RealtimeEndpointStatusFailed,
  RealtimeEndpointStatus'
  #-}
