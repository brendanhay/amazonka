{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.ServerStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.ServerStatus
  ( ServerStatus
      ( ServerStatus',
        BackingUp,
        ConnectionLost,
        Creating,
        Deleting,
        Failed,
        Healthy,
        Modifying,
        Restoring,
        Running,
        Setup,
        Terminated,
        UnderMaintenance,
        Unhealthy
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ServerStatus = ServerStatus' Lude.Text
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

pattern BackingUp :: ServerStatus
pattern BackingUp = ServerStatus' "BACKING_UP"

pattern ConnectionLost :: ServerStatus
pattern ConnectionLost = ServerStatus' "CONNECTION_LOST"

pattern Creating :: ServerStatus
pattern Creating = ServerStatus' "CREATING"

pattern Deleting :: ServerStatus
pattern Deleting = ServerStatus' "DELETING"

pattern Failed :: ServerStatus
pattern Failed = ServerStatus' "FAILED"

pattern Healthy :: ServerStatus
pattern Healthy = ServerStatus' "HEALTHY"

pattern Modifying :: ServerStatus
pattern Modifying = ServerStatus' "MODIFYING"

pattern Restoring :: ServerStatus
pattern Restoring = ServerStatus' "RESTORING"

pattern Running :: ServerStatus
pattern Running = ServerStatus' "RUNNING"

pattern Setup :: ServerStatus
pattern Setup = ServerStatus' "SETUP"

pattern Terminated :: ServerStatus
pattern Terminated = ServerStatus' "TERMINATED"

pattern UnderMaintenance :: ServerStatus
pattern UnderMaintenance = ServerStatus' "UNDER_MAINTENANCE"

pattern Unhealthy :: ServerStatus
pattern Unhealthy = ServerStatus' "UNHEALTHY"

{-# COMPLETE
  BackingUp,
  ConnectionLost,
  Creating,
  Deleting,
  Failed,
  Healthy,
  Modifying,
  Restoring,
  Running,
  Setup,
  Terminated,
  UnderMaintenance,
  Unhealthy,
  ServerStatus'
  #-}
