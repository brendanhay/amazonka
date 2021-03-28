{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.ServerStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorksCM.Types.ServerStatus
  ( ServerStatus
    ( ServerStatus'
    , ServerStatusBackingUp
    , ServerStatusConnectionLost
    , ServerStatusCreating
    , ServerStatusDeleting
    , ServerStatusModifying
    , ServerStatusFailed
    , ServerStatusHealthy
    , ServerStatusRunning
    , ServerStatusRestoring
    , ServerStatusSetup
    , ServerStatusUnderMaintenance
    , ServerStatusUnhealthy
    , ServerStatusTerminated
    , fromServerStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ServerStatus = ServerStatus'{fromServerStatus :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern ServerStatusBackingUp :: ServerStatus
pattern ServerStatusBackingUp = ServerStatus' "BACKING_UP"

pattern ServerStatusConnectionLost :: ServerStatus
pattern ServerStatusConnectionLost = ServerStatus' "CONNECTION_LOST"

pattern ServerStatusCreating :: ServerStatus
pattern ServerStatusCreating = ServerStatus' "CREATING"

pattern ServerStatusDeleting :: ServerStatus
pattern ServerStatusDeleting = ServerStatus' "DELETING"

pattern ServerStatusModifying :: ServerStatus
pattern ServerStatusModifying = ServerStatus' "MODIFYING"

pattern ServerStatusFailed :: ServerStatus
pattern ServerStatusFailed = ServerStatus' "FAILED"

pattern ServerStatusHealthy :: ServerStatus
pattern ServerStatusHealthy = ServerStatus' "HEALTHY"

pattern ServerStatusRunning :: ServerStatus
pattern ServerStatusRunning = ServerStatus' "RUNNING"

pattern ServerStatusRestoring :: ServerStatus
pattern ServerStatusRestoring = ServerStatus' "RESTORING"

pattern ServerStatusSetup :: ServerStatus
pattern ServerStatusSetup = ServerStatus' "SETUP"

pattern ServerStatusUnderMaintenance :: ServerStatus
pattern ServerStatusUnderMaintenance = ServerStatus' "UNDER_MAINTENANCE"

pattern ServerStatusUnhealthy :: ServerStatus
pattern ServerStatusUnhealthy = ServerStatus' "UNHEALTHY"

pattern ServerStatusTerminated :: ServerStatus
pattern ServerStatusTerminated = ServerStatus' "TERMINATED"

{-# COMPLETE 
  ServerStatusBackingUp,

  ServerStatusConnectionLost,

  ServerStatusCreating,

  ServerStatusDeleting,

  ServerStatusModifying,

  ServerStatusFailed,

  ServerStatusHealthy,

  ServerStatusRunning,

  ServerStatusRestoring,

  ServerStatusSetup,

  ServerStatusUnderMaintenance,

  ServerStatusUnhealthy,

  ServerStatusTerminated,
  ServerStatus'
  #-}
