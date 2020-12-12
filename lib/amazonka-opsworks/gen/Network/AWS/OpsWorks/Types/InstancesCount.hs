{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.InstancesCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.InstancesCount
  ( InstancesCount (..),

    -- * Smart constructor
    mkInstancesCount,

    -- * Lenses
    icTerminating,
    icPending,
    icOnline,
    icUnassigning,
    icDeregistering,
    icRunningSetup,
    icRequested,
    icStopFailed,
    icBooting,
    icStopped,
    icRebooting,
    icAssigning,
    icShuttingDown,
    icSetupFailed,
    icConnectionLost,
    icTerminated,
    icStopping,
    icRegistered,
    icStartFailed,
    icRegistering,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes how many instances a stack has for each status.
--
-- /See:/ 'mkInstancesCount' smart constructor.
data InstancesCount = InstancesCount'
  { terminating ::
      Lude.Maybe Lude.Int,
    pending :: Lude.Maybe Lude.Int,
    online :: Lude.Maybe Lude.Int,
    unassigning :: Lude.Maybe Lude.Int,
    deregistering :: Lude.Maybe Lude.Int,
    runningSetup :: Lude.Maybe Lude.Int,
    requested :: Lude.Maybe Lude.Int,
    stopFailed :: Lude.Maybe Lude.Int,
    booting :: Lude.Maybe Lude.Int,
    stopped :: Lude.Maybe Lude.Int,
    rebooting :: Lude.Maybe Lude.Int,
    assigning :: Lude.Maybe Lude.Int,
    shuttingDown :: Lude.Maybe Lude.Int,
    setupFailed :: Lude.Maybe Lude.Int,
    connectionLost :: Lude.Maybe Lude.Int,
    terminated :: Lude.Maybe Lude.Int,
    stopping :: Lude.Maybe Lude.Int,
    registered :: Lude.Maybe Lude.Int,
    startFailed :: Lude.Maybe Lude.Int,
    registering :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstancesCount' with the minimum fields required to make a request.
--
-- * 'assigning' - The number of instances in the Assigning state.
-- * 'booting' - The number of instances with @booting@ status.
-- * 'connectionLost' - The number of instances with @connection_lost@ status.
-- * 'deregistering' - The number of instances in the Deregistering state.
-- * 'online' - The number of instances with @online@ status.
-- * 'pending' - The number of instances with @pending@ status.
-- * 'rebooting' - The number of instances with @rebooting@ status.
-- * 'registered' - The number of instances in the Registered state.
-- * 'registering' - The number of instances in the Registering state.
-- * 'requested' - The number of instances with @requested@ status.
-- * 'runningSetup' - The number of instances with @running_setup@ status.
-- * 'setupFailed' - The number of instances with @setup_failed@ status.
-- * 'shuttingDown' - The number of instances with @shutting_down@ status.
-- * 'startFailed' - The number of instances with @start_failed@ status.
-- * 'stopFailed' - The number of instances with @stop_failed@ status.
-- * 'stopped' - The number of instances with @stopped@ status.
-- * 'stopping' - The number of instances with @stopping@ status.
-- * 'terminated' - The number of instances with @terminated@ status.
-- * 'terminating' - The number of instances with @terminating@ status.
-- * 'unassigning' - The number of instances in the Unassigning state.
mkInstancesCount ::
  InstancesCount
mkInstancesCount =
  InstancesCount'
    { terminating = Lude.Nothing,
      pending = Lude.Nothing,
      online = Lude.Nothing,
      unassigning = Lude.Nothing,
      deregistering = Lude.Nothing,
      runningSetup = Lude.Nothing,
      requested = Lude.Nothing,
      stopFailed = Lude.Nothing,
      booting = Lude.Nothing,
      stopped = Lude.Nothing,
      rebooting = Lude.Nothing,
      assigning = Lude.Nothing,
      shuttingDown = Lude.Nothing,
      setupFailed = Lude.Nothing,
      connectionLost = Lude.Nothing,
      terminated = Lude.Nothing,
      stopping = Lude.Nothing,
      registered = Lude.Nothing,
      startFailed = Lude.Nothing,
      registering = Lude.Nothing
    }

-- | The number of instances with @terminating@ status.
--
-- /Note:/ Consider using 'terminating' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTerminating :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icTerminating = Lens.lens (terminating :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {terminating = a} :: InstancesCount)
{-# DEPRECATED icTerminating "Use generic-lens or generic-optics with 'terminating' instead." #-}

-- | The number of instances with @pending@ status.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icPending :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icPending = Lens.lens (pending :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {pending = a} :: InstancesCount)
{-# DEPRECATED icPending "Use generic-lens or generic-optics with 'pending' instead." #-}

-- | The number of instances with @online@ status.
--
-- /Note:/ Consider using 'online' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icOnline :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icOnline = Lens.lens (online :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {online = a} :: InstancesCount)
{-# DEPRECATED icOnline "Use generic-lens or generic-optics with 'online' instead." #-}

-- | The number of instances in the Unassigning state.
--
-- /Note:/ Consider using 'unassigning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icUnassigning :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icUnassigning = Lens.lens (unassigning :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {unassigning = a} :: InstancesCount)
{-# DEPRECATED icUnassigning "Use generic-lens or generic-optics with 'unassigning' instead." #-}

-- | The number of instances in the Deregistering state.
--
-- /Note:/ Consider using 'deregistering' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icDeregistering :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icDeregistering = Lens.lens (deregistering :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {deregistering = a} :: InstancesCount)
{-# DEPRECATED icDeregistering "Use generic-lens or generic-optics with 'deregistering' instead." #-}

-- | The number of instances with @running_setup@ status.
--
-- /Note:/ Consider using 'runningSetup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icRunningSetup :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icRunningSetup = Lens.lens (runningSetup :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {runningSetup = a} :: InstancesCount)
{-# DEPRECATED icRunningSetup "Use generic-lens or generic-optics with 'runningSetup' instead." #-}

-- | The number of instances with @requested@ status.
--
-- /Note:/ Consider using 'requested' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icRequested :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icRequested = Lens.lens (requested :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {requested = a} :: InstancesCount)
{-# DEPRECATED icRequested "Use generic-lens or generic-optics with 'requested' instead." #-}

-- | The number of instances with @stop_failed@ status.
--
-- /Note:/ Consider using 'stopFailed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icStopFailed :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icStopFailed = Lens.lens (stopFailed :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {stopFailed = a} :: InstancesCount)
{-# DEPRECATED icStopFailed "Use generic-lens or generic-optics with 'stopFailed' instead." #-}

-- | The number of instances with @booting@ status.
--
-- /Note:/ Consider using 'booting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icBooting :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icBooting = Lens.lens (booting :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {booting = a} :: InstancesCount)
{-# DEPRECATED icBooting "Use generic-lens or generic-optics with 'booting' instead." #-}

-- | The number of instances with @stopped@ status.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icStopped :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icStopped = Lens.lens (stopped :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {stopped = a} :: InstancesCount)
{-# DEPRECATED icStopped "Use generic-lens or generic-optics with 'stopped' instead." #-}

-- | The number of instances with @rebooting@ status.
--
-- /Note:/ Consider using 'rebooting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icRebooting :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icRebooting = Lens.lens (rebooting :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {rebooting = a} :: InstancesCount)
{-# DEPRECATED icRebooting "Use generic-lens or generic-optics with 'rebooting' instead." #-}

-- | The number of instances in the Assigning state.
--
-- /Note:/ Consider using 'assigning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icAssigning :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icAssigning = Lens.lens (assigning :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {assigning = a} :: InstancesCount)
{-# DEPRECATED icAssigning "Use generic-lens or generic-optics with 'assigning' instead." #-}

-- | The number of instances with @shutting_down@ status.
--
-- /Note:/ Consider using 'shuttingDown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icShuttingDown :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icShuttingDown = Lens.lens (shuttingDown :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {shuttingDown = a} :: InstancesCount)
{-# DEPRECATED icShuttingDown "Use generic-lens or generic-optics with 'shuttingDown' instead." #-}

-- | The number of instances with @setup_failed@ status.
--
-- /Note:/ Consider using 'setupFailed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icSetupFailed :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icSetupFailed = Lens.lens (setupFailed :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {setupFailed = a} :: InstancesCount)
{-# DEPRECATED icSetupFailed "Use generic-lens or generic-optics with 'setupFailed' instead." #-}

-- | The number of instances with @connection_lost@ status.
--
-- /Note:/ Consider using 'connectionLost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icConnectionLost :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icConnectionLost = Lens.lens (connectionLost :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {connectionLost = a} :: InstancesCount)
{-# DEPRECATED icConnectionLost "Use generic-lens or generic-optics with 'connectionLost' instead." #-}

-- | The number of instances with @terminated@ status.
--
-- /Note:/ Consider using 'terminated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTerminated :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icTerminated = Lens.lens (terminated :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {terminated = a} :: InstancesCount)
{-# DEPRECATED icTerminated "Use generic-lens or generic-optics with 'terminated' instead." #-}

-- | The number of instances with @stopping@ status.
--
-- /Note:/ Consider using 'stopping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icStopping :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icStopping = Lens.lens (stopping :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {stopping = a} :: InstancesCount)
{-# DEPRECATED icStopping "Use generic-lens or generic-optics with 'stopping' instead." #-}

-- | The number of instances in the Registered state.
--
-- /Note:/ Consider using 'registered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icRegistered :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icRegistered = Lens.lens (registered :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {registered = a} :: InstancesCount)
{-# DEPRECATED icRegistered "Use generic-lens or generic-optics with 'registered' instead." #-}

-- | The number of instances with @start_failed@ status.
--
-- /Note:/ Consider using 'startFailed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icStartFailed :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icStartFailed = Lens.lens (startFailed :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {startFailed = a} :: InstancesCount)
{-# DEPRECATED icStartFailed "Use generic-lens or generic-optics with 'startFailed' instead." #-}

-- | The number of instances in the Registering state.
--
-- /Note:/ Consider using 'registering' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icRegistering :: Lens.Lens' InstancesCount (Lude.Maybe Lude.Int)
icRegistering = Lens.lens (registering :: InstancesCount -> Lude.Maybe Lude.Int) (\s a -> s {registering = a} :: InstancesCount)
{-# DEPRECATED icRegistering "Use generic-lens or generic-optics with 'registering' instead." #-}

instance Lude.FromJSON InstancesCount where
  parseJSON =
    Lude.withObject
      "InstancesCount"
      ( \x ->
          InstancesCount'
            Lude.<$> (x Lude..:? "Terminating")
            Lude.<*> (x Lude..:? "Pending")
            Lude.<*> (x Lude..:? "Online")
            Lude.<*> (x Lude..:? "Unassigning")
            Lude.<*> (x Lude..:? "Deregistering")
            Lude.<*> (x Lude..:? "RunningSetup")
            Lude.<*> (x Lude..:? "Requested")
            Lude.<*> (x Lude..:? "StopFailed")
            Lude.<*> (x Lude..:? "Booting")
            Lude.<*> (x Lude..:? "Stopped")
            Lude.<*> (x Lude..:? "Rebooting")
            Lude.<*> (x Lude..:? "Assigning")
            Lude.<*> (x Lude..:? "ShuttingDown")
            Lude.<*> (x Lude..:? "SetupFailed")
            Lude.<*> (x Lude..:? "ConnectionLost")
            Lude.<*> (x Lude..:? "Terminated")
            Lude.<*> (x Lude..:? "Stopping")
            Lude.<*> (x Lude..:? "Registered")
            Lude.<*> (x Lude..:? "StartFailed")
            Lude.<*> (x Lude..:? "Registering")
      )
