{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus
  ( WorkspaceConnectionStatus (..)
  -- * Smart constructor
  , mkWorkspaceConnectionStatus
  -- * Lenses
  , wcsConnectionState
  , wcsConnectionStateCheckTimestamp
  , wcsLastKnownUserConnectionTimestamp
  , wcsWorkspaceId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.ConnectionState as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceId as Types

-- | Describes the connection status of a WorkSpace.
--
-- /See:/ 'mkWorkspaceConnectionStatus' smart constructor.
data WorkspaceConnectionStatus = WorkspaceConnectionStatus'
  { connectionState :: Core.Maybe Types.ConnectionState
    -- ^ The connection state of the WorkSpace. The connection state is unknown if the WorkSpace is stopped.
  , connectionStateCheckTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp of the connection status check.
  , lastKnownUserConnectionTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp of the last known user connection.
  , workspaceId :: Core.Maybe Types.WorkspaceId
    -- ^ The identifier of the WorkSpace.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'WorkspaceConnectionStatus' value with any optional fields omitted.
mkWorkspaceConnectionStatus
    :: WorkspaceConnectionStatus
mkWorkspaceConnectionStatus
  = WorkspaceConnectionStatus'{connectionState = Core.Nothing,
                               connectionStateCheckTimestamp = Core.Nothing,
                               lastKnownUserConnectionTimestamp = Core.Nothing,
                               workspaceId = Core.Nothing}

-- | The connection state of the WorkSpace. The connection state is unknown if the WorkSpace is stopped.
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcsConnectionState :: Lens.Lens' WorkspaceConnectionStatus (Core.Maybe Types.ConnectionState)
wcsConnectionState = Lens.field @"connectionState"
{-# INLINEABLE wcsConnectionState #-}
{-# DEPRECATED connectionState "Use generic-lens or generic-optics with 'connectionState' instead"  #-}

-- | The timestamp of the connection status check.
--
-- /Note:/ Consider using 'connectionStateCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcsConnectionStateCheckTimestamp :: Lens.Lens' WorkspaceConnectionStatus (Core.Maybe Core.NominalDiffTime)
wcsConnectionStateCheckTimestamp = Lens.field @"connectionStateCheckTimestamp"
{-# INLINEABLE wcsConnectionStateCheckTimestamp #-}
{-# DEPRECATED connectionStateCheckTimestamp "Use generic-lens or generic-optics with 'connectionStateCheckTimestamp' instead"  #-}

-- | The timestamp of the last known user connection.
--
-- /Note:/ Consider using 'lastKnownUserConnectionTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcsLastKnownUserConnectionTimestamp :: Lens.Lens' WorkspaceConnectionStatus (Core.Maybe Core.NominalDiffTime)
wcsLastKnownUserConnectionTimestamp = Lens.field @"lastKnownUserConnectionTimestamp"
{-# INLINEABLE wcsLastKnownUserConnectionTimestamp #-}
{-# DEPRECATED lastKnownUserConnectionTimestamp "Use generic-lens or generic-optics with 'lastKnownUserConnectionTimestamp' instead"  #-}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcsWorkspaceId :: Lens.Lens' WorkspaceConnectionStatus (Core.Maybe Types.WorkspaceId)
wcsWorkspaceId = Lens.field @"workspaceId"
{-# INLINEABLE wcsWorkspaceId #-}
{-# DEPRECATED workspaceId "Use generic-lens or generic-optics with 'workspaceId' instead"  #-}

instance Core.FromJSON WorkspaceConnectionStatus where
        parseJSON
          = Core.withObject "WorkspaceConnectionStatus" Core.$
              \ x ->
                WorkspaceConnectionStatus' Core.<$>
                  (x Core..:? "ConnectionState") Core.<*>
                    x Core..:? "ConnectionStateCheckTimestamp"
                    Core.<*> x Core..:? "LastKnownUserConnectionTimestamp"
                    Core.<*> x Core..:? "WorkspaceId"
