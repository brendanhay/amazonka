{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.InstancesCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.InstancesCount
  ( InstancesCount (..)
  -- * Smart constructor
  , mkInstancesCount
  -- * Lenses
  , icAssigning
  , icBooting
  , icConnectionLost
  , icDeregistering
  , icOnline
  , icPending
  , icRebooting
  , icRegistered
  , icRegistering
  , icRequested
  , icRunningSetup
  , icSetupFailed
  , icShuttingDown
  , icStartFailed
  , icStopFailed
  , icStopped
  , icStopping
  , icTerminated
  , icTerminating
  , icUnassigning
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes how many instances a stack has for each status.
--
-- /See:/ 'mkInstancesCount' smart constructor.
data InstancesCount = InstancesCount'
  { assigning :: Core.Maybe Core.Int
    -- ^ The number of instances in the Assigning state.
  , booting :: Core.Maybe Core.Int
    -- ^ The number of instances with @booting@ status.
  , connectionLost :: Core.Maybe Core.Int
    -- ^ The number of instances with @connection_lost@ status.
  , deregistering :: Core.Maybe Core.Int
    -- ^ The number of instances in the Deregistering state.
  , online :: Core.Maybe Core.Int
    -- ^ The number of instances with @online@ status.
  , pending :: Core.Maybe Core.Int
    -- ^ The number of instances with @pending@ status.
  , rebooting :: Core.Maybe Core.Int
    -- ^ The number of instances with @rebooting@ status.
  , registered :: Core.Maybe Core.Int
    -- ^ The number of instances in the Registered state.
  , registering :: Core.Maybe Core.Int
    -- ^ The number of instances in the Registering state.
  , requested :: Core.Maybe Core.Int
    -- ^ The number of instances with @requested@ status.
  , runningSetup :: Core.Maybe Core.Int
    -- ^ The number of instances with @running_setup@ status.
  , setupFailed :: Core.Maybe Core.Int
    -- ^ The number of instances with @setup_failed@ status.
  , shuttingDown :: Core.Maybe Core.Int
    -- ^ The number of instances with @shutting_down@ status.
  , startFailed :: Core.Maybe Core.Int
    -- ^ The number of instances with @start_failed@ status.
  , stopFailed :: Core.Maybe Core.Int
    -- ^ The number of instances with @stop_failed@ status.
  , stopped :: Core.Maybe Core.Int
    -- ^ The number of instances with @stopped@ status.
  , stopping :: Core.Maybe Core.Int
    -- ^ The number of instances with @stopping@ status.
  , terminated :: Core.Maybe Core.Int
    -- ^ The number of instances with @terminated@ status.
  , terminating :: Core.Maybe Core.Int
    -- ^ The number of instances with @terminating@ status.
  , unassigning :: Core.Maybe Core.Int
    -- ^ The number of instances in the Unassigning state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstancesCount' value with any optional fields omitted.
mkInstancesCount
    :: InstancesCount
mkInstancesCount
  = InstancesCount'{assigning = Core.Nothing, booting = Core.Nothing,
                    connectionLost = Core.Nothing, deregistering = Core.Nothing,
                    online = Core.Nothing, pending = Core.Nothing,
                    rebooting = Core.Nothing, registered = Core.Nothing,
                    registering = Core.Nothing, requested = Core.Nothing,
                    runningSetup = Core.Nothing, setupFailed = Core.Nothing,
                    shuttingDown = Core.Nothing, startFailed = Core.Nothing,
                    stopFailed = Core.Nothing, stopped = Core.Nothing,
                    stopping = Core.Nothing, terminated = Core.Nothing,
                    terminating = Core.Nothing, unassigning = Core.Nothing}

-- | The number of instances in the Assigning state.
--
-- /Note:/ Consider using 'assigning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icAssigning :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icAssigning = Lens.field @"assigning"
{-# INLINEABLE icAssigning #-}
{-# DEPRECATED assigning "Use generic-lens or generic-optics with 'assigning' instead"  #-}

-- | The number of instances with @booting@ status.
--
-- /Note:/ Consider using 'booting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icBooting :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icBooting = Lens.field @"booting"
{-# INLINEABLE icBooting #-}
{-# DEPRECATED booting "Use generic-lens or generic-optics with 'booting' instead"  #-}

-- | The number of instances with @connection_lost@ status.
--
-- /Note:/ Consider using 'connectionLost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icConnectionLost :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icConnectionLost = Lens.field @"connectionLost"
{-# INLINEABLE icConnectionLost #-}
{-# DEPRECATED connectionLost "Use generic-lens or generic-optics with 'connectionLost' instead"  #-}

-- | The number of instances in the Deregistering state.
--
-- /Note:/ Consider using 'deregistering' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icDeregistering :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icDeregistering = Lens.field @"deregistering"
{-# INLINEABLE icDeregistering #-}
{-# DEPRECATED deregistering "Use generic-lens or generic-optics with 'deregistering' instead"  #-}

-- | The number of instances with @online@ status.
--
-- /Note:/ Consider using 'online' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icOnline :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icOnline = Lens.field @"online"
{-# INLINEABLE icOnline #-}
{-# DEPRECATED online "Use generic-lens or generic-optics with 'online' instead"  #-}

-- | The number of instances with @pending@ status.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icPending :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icPending = Lens.field @"pending"
{-# INLINEABLE icPending #-}
{-# DEPRECATED pending "Use generic-lens or generic-optics with 'pending' instead"  #-}

-- | The number of instances with @rebooting@ status.
--
-- /Note:/ Consider using 'rebooting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icRebooting :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icRebooting = Lens.field @"rebooting"
{-# INLINEABLE icRebooting #-}
{-# DEPRECATED rebooting "Use generic-lens or generic-optics with 'rebooting' instead"  #-}

-- | The number of instances in the Registered state.
--
-- /Note:/ Consider using 'registered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icRegistered :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icRegistered = Lens.field @"registered"
{-# INLINEABLE icRegistered #-}
{-# DEPRECATED registered "Use generic-lens or generic-optics with 'registered' instead"  #-}

-- | The number of instances in the Registering state.
--
-- /Note:/ Consider using 'registering' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icRegistering :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icRegistering = Lens.field @"registering"
{-# INLINEABLE icRegistering #-}
{-# DEPRECATED registering "Use generic-lens or generic-optics with 'registering' instead"  #-}

-- | The number of instances with @requested@ status.
--
-- /Note:/ Consider using 'requested' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icRequested :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icRequested = Lens.field @"requested"
{-# INLINEABLE icRequested #-}
{-# DEPRECATED requested "Use generic-lens or generic-optics with 'requested' instead"  #-}

-- | The number of instances with @running_setup@ status.
--
-- /Note:/ Consider using 'runningSetup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icRunningSetup :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icRunningSetup = Lens.field @"runningSetup"
{-# INLINEABLE icRunningSetup #-}
{-# DEPRECATED runningSetup "Use generic-lens or generic-optics with 'runningSetup' instead"  #-}

-- | The number of instances with @setup_failed@ status.
--
-- /Note:/ Consider using 'setupFailed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icSetupFailed :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icSetupFailed = Lens.field @"setupFailed"
{-# INLINEABLE icSetupFailed #-}
{-# DEPRECATED setupFailed "Use generic-lens or generic-optics with 'setupFailed' instead"  #-}

-- | The number of instances with @shutting_down@ status.
--
-- /Note:/ Consider using 'shuttingDown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icShuttingDown :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icShuttingDown = Lens.field @"shuttingDown"
{-# INLINEABLE icShuttingDown #-}
{-# DEPRECATED shuttingDown "Use generic-lens or generic-optics with 'shuttingDown' instead"  #-}

-- | The number of instances with @start_failed@ status.
--
-- /Note:/ Consider using 'startFailed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icStartFailed :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icStartFailed = Lens.field @"startFailed"
{-# INLINEABLE icStartFailed #-}
{-# DEPRECATED startFailed "Use generic-lens or generic-optics with 'startFailed' instead"  #-}

-- | The number of instances with @stop_failed@ status.
--
-- /Note:/ Consider using 'stopFailed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icStopFailed :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icStopFailed = Lens.field @"stopFailed"
{-# INLINEABLE icStopFailed #-}
{-# DEPRECATED stopFailed "Use generic-lens or generic-optics with 'stopFailed' instead"  #-}

-- | The number of instances with @stopped@ status.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icStopped :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icStopped = Lens.field @"stopped"
{-# INLINEABLE icStopped #-}
{-# DEPRECATED stopped "Use generic-lens or generic-optics with 'stopped' instead"  #-}

-- | The number of instances with @stopping@ status.
--
-- /Note:/ Consider using 'stopping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icStopping :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icStopping = Lens.field @"stopping"
{-# INLINEABLE icStopping #-}
{-# DEPRECATED stopping "Use generic-lens or generic-optics with 'stopping' instead"  #-}

-- | The number of instances with @terminated@ status.
--
-- /Note:/ Consider using 'terminated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTerminated :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icTerminated = Lens.field @"terminated"
{-# INLINEABLE icTerminated #-}
{-# DEPRECATED terminated "Use generic-lens or generic-optics with 'terminated' instead"  #-}

-- | The number of instances with @terminating@ status.
--
-- /Note:/ Consider using 'terminating' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTerminating :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icTerminating = Lens.field @"terminating"
{-# INLINEABLE icTerminating #-}
{-# DEPRECATED terminating "Use generic-lens or generic-optics with 'terminating' instead"  #-}

-- | The number of instances in the Unassigning state.
--
-- /Note:/ Consider using 'unassigning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icUnassigning :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
icUnassigning = Lens.field @"unassigning"
{-# INLINEABLE icUnassigning #-}
{-# DEPRECATED unassigning "Use generic-lens or generic-optics with 'unassigning' instead"  #-}

instance Core.FromJSON InstancesCount where
        parseJSON
          = Core.withObject "InstancesCount" Core.$
              \ x ->
                InstancesCount' Core.<$>
                  (x Core..:? "Assigning") Core.<*> x Core..:? "Booting" Core.<*>
                    x Core..:? "ConnectionLost"
                    Core.<*> x Core..:? "Deregistering"
                    Core.<*> x Core..:? "Online"
                    Core.<*> x Core..:? "Pending"
                    Core.<*> x Core..:? "Rebooting"
                    Core.<*> x Core..:? "Registered"
                    Core.<*> x Core..:? "Registering"
                    Core.<*> x Core..:? "Requested"
                    Core.<*> x Core..:? "RunningSetup"
                    Core.<*> x Core..:? "SetupFailed"
                    Core.<*> x Core..:? "ShuttingDown"
                    Core.<*> x Core..:? "StartFailed"
                    Core.<*> x Core..:? "StopFailed"
                    Core.<*> x Core..:? "Stopped"
                    Core.<*> x Core..:? "Stopping"
                    Core.<*> x Core..:? "Terminated"
                    Core.<*> x Core..:? "Terminating"
                    Core.<*> x Core..:? "Unassigning"
