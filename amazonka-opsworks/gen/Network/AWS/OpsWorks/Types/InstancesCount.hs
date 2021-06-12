{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.InstancesCount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.InstancesCount where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes how many instances a stack has for each status.
--
-- /See:/ 'newInstancesCount' smart constructor.
data InstancesCount = InstancesCount'
  { -- | The number of instances with @online@ status.
    online :: Core.Maybe Core.Int,
    -- | The number of instances with @setup_failed@ status.
    setupFailed :: Core.Maybe Core.Int,
    -- | The number of instances in the Registering state.
    registering :: Core.Maybe Core.Int,
    -- | The number of instances with @booting@ status.
    booting :: Core.Maybe Core.Int,
    -- | The number of instances with @stop_failed@ status.
    stopFailed :: Core.Maybe Core.Int,
    -- | The number of instances with @start_failed@ status.
    startFailed :: Core.Maybe Core.Int,
    -- | The number of instances with @running_setup@ status.
    runningSetup :: Core.Maybe Core.Int,
    -- | The number of instances with @terminated@ status.
    terminated :: Core.Maybe Core.Int,
    -- | The number of instances with @pending@ status.
    pending :: Core.Maybe Core.Int,
    -- | The number of instances with @terminating@ status.
    terminating :: Core.Maybe Core.Int,
    -- | The number of instances with @shutting_down@ status.
    shuttingDown :: Core.Maybe Core.Int,
    -- | The number of instances in the Assigning state.
    assigning :: Core.Maybe Core.Int,
    -- | The number of instances with @stopped@ status.
    stopped :: Core.Maybe Core.Int,
    -- | The number of instances with @rebooting@ status.
    rebooting :: Core.Maybe Core.Int,
    -- | The number of instances in the Registered state.
    registered :: Core.Maybe Core.Int,
    -- | The number of instances with @requested@ status.
    requested :: Core.Maybe Core.Int,
    -- | The number of instances in the Deregistering state.
    deregistering :: Core.Maybe Core.Int,
    -- | The number of instances with @stopping@ status.
    stopping :: Core.Maybe Core.Int,
    -- | The number of instances in the Unassigning state.
    unassigning :: Core.Maybe Core.Int,
    -- | The number of instances with @connection_lost@ status.
    connectionLost :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstancesCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'online', 'instancesCount_online' - The number of instances with @online@ status.
--
-- 'setupFailed', 'instancesCount_setupFailed' - The number of instances with @setup_failed@ status.
--
-- 'registering', 'instancesCount_registering' - The number of instances in the Registering state.
--
-- 'booting', 'instancesCount_booting' - The number of instances with @booting@ status.
--
-- 'stopFailed', 'instancesCount_stopFailed' - The number of instances with @stop_failed@ status.
--
-- 'startFailed', 'instancesCount_startFailed' - The number of instances with @start_failed@ status.
--
-- 'runningSetup', 'instancesCount_runningSetup' - The number of instances with @running_setup@ status.
--
-- 'terminated', 'instancesCount_terminated' - The number of instances with @terminated@ status.
--
-- 'pending', 'instancesCount_pending' - The number of instances with @pending@ status.
--
-- 'terminating', 'instancesCount_terminating' - The number of instances with @terminating@ status.
--
-- 'shuttingDown', 'instancesCount_shuttingDown' - The number of instances with @shutting_down@ status.
--
-- 'assigning', 'instancesCount_assigning' - The number of instances in the Assigning state.
--
-- 'stopped', 'instancesCount_stopped' - The number of instances with @stopped@ status.
--
-- 'rebooting', 'instancesCount_rebooting' - The number of instances with @rebooting@ status.
--
-- 'registered', 'instancesCount_registered' - The number of instances in the Registered state.
--
-- 'requested', 'instancesCount_requested' - The number of instances with @requested@ status.
--
-- 'deregistering', 'instancesCount_deregistering' - The number of instances in the Deregistering state.
--
-- 'stopping', 'instancesCount_stopping' - The number of instances with @stopping@ status.
--
-- 'unassigning', 'instancesCount_unassigning' - The number of instances in the Unassigning state.
--
-- 'connectionLost', 'instancesCount_connectionLost' - The number of instances with @connection_lost@ status.
newInstancesCount ::
  InstancesCount
newInstancesCount =
  InstancesCount'
    { online = Core.Nothing,
      setupFailed = Core.Nothing,
      registering = Core.Nothing,
      booting = Core.Nothing,
      stopFailed = Core.Nothing,
      startFailed = Core.Nothing,
      runningSetup = Core.Nothing,
      terminated = Core.Nothing,
      pending = Core.Nothing,
      terminating = Core.Nothing,
      shuttingDown = Core.Nothing,
      assigning = Core.Nothing,
      stopped = Core.Nothing,
      rebooting = Core.Nothing,
      registered = Core.Nothing,
      requested = Core.Nothing,
      deregistering = Core.Nothing,
      stopping = Core.Nothing,
      unassigning = Core.Nothing,
      connectionLost = Core.Nothing
    }

-- | The number of instances with @online@ status.
instancesCount_online :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_online = Lens.lens (\InstancesCount' {online} -> online) (\s@InstancesCount' {} a -> s {online = a} :: InstancesCount)

-- | The number of instances with @setup_failed@ status.
instancesCount_setupFailed :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_setupFailed = Lens.lens (\InstancesCount' {setupFailed} -> setupFailed) (\s@InstancesCount' {} a -> s {setupFailed = a} :: InstancesCount)

-- | The number of instances in the Registering state.
instancesCount_registering :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_registering = Lens.lens (\InstancesCount' {registering} -> registering) (\s@InstancesCount' {} a -> s {registering = a} :: InstancesCount)

-- | The number of instances with @booting@ status.
instancesCount_booting :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_booting = Lens.lens (\InstancesCount' {booting} -> booting) (\s@InstancesCount' {} a -> s {booting = a} :: InstancesCount)

-- | The number of instances with @stop_failed@ status.
instancesCount_stopFailed :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_stopFailed = Lens.lens (\InstancesCount' {stopFailed} -> stopFailed) (\s@InstancesCount' {} a -> s {stopFailed = a} :: InstancesCount)

-- | The number of instances with @start_failed@ status.
instancesCount_startFailed :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_startFailed = Lens.lens (\InstancesCount' {startFailed} -> startFailed) (\s@InstancesCount' {} a -> s {startFailed = a} :: InstancesCount)

-- | The number of instances with @running_setup@ status.
instancesCount_runningSetup :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_runningSetup = Lens.lens (\InstancesCount' {runningSetup} -> runningSetup) (\s@InstancesCount' {} a -> s {runningSetup = a} :: InstancesCount)

-- | The number of instances with @terminated@ status.
instancesCount_terminated :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_terminated = Lens.lens (\InstancesCount' {terminated} -> terminated) (\s@InstancesCount' {} a -> s {terminated = a} :: InstancesCount)

-- | The number of instances with @pending@ status.
instancesCount_pending :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_pending = Lens.lens (\InstancesCount' {pending} -> pending) (\s@InstancesCount' {} a -> s {pending = a} :: InstancesCount)

-- | The number of instances with @terminating@ status.
instancesCount_terminating :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_terminating = Lens.lens (\InstancesCount' {terminating} -> terminating) (\s@InstancesCount' {} a -> s {terminating = a} :: InstancesCount)

-- | The number of instances with @shutting_down@ status.
instancesCount_shuttingDown :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_shuttingDown = Lens.lens (\InstancesCount' {shuttingDown} -> shuttingDown) (\s@InstancesCount' {} a -> s {shuttingDown = a} :: InstancesCount)

-- | The number of instances in the Assigning state.
instancesCount_assigning :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_assigning = Lens.lens (\InstancesCount' {assigning} -> assigning) (\s@InstancesCount' {} a -> s {assigning = a} :: InstancesCount)

-- | The number of instances with @stopped@ status.
instancesCount_stopped :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_stopped = Lens.lens (\InstancesCount' {stopped} -> stopped) (\s@InstancesCount' {} a -> s {stopped = a} :: InstancesCount)

-- | The number of instances with @rebooting@ status.
instancesCount_rebooting :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_rebooting = Lens.lens (\InstancesCount' {rebooting} -> rebooting) (\s@InstancesCount' {} a -> s {rebooting = a} :: InstancesCount)

-- | The number of instances in the Registered state.
instancesCount_registered :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_registered = Lens.lens (\InstancesCount' {registered} -> registered) (\s@InstancesCount' {} a -> s {registered = a} :: InstancesCount)

-- | The number of instances with @requested@ status.
instancesCount_requested :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_requested = Lens.lens (\InstancesCount' {requested} -> requested) (\s@InstancesCount' {} a -> s {requested = a} :: InstancesCount)

-- | The number of instances in the Deregistering state.
instancesCount_deregistering :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_deregistering = Lens.lens (\InstancesCount' {deregistering} -> deregistering) (\s@InstancesCount' {} a -> s {deregistering = a} :: InstancesCount)

-- | The number of instances with @stopping@ status.
instancesCount_stopping :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_stopping = Lens.lens (\InstancesCount' {stopping} -> stopping) (\s@InstancesCount' {} a -> s {stopping = a} :: InstancesCount)

-- | The number of instances in the Unassigning state.
instancesCount_unassigning :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_unassigning = Lens.lens (\InstancesCount' {unassigning} -> unassigning) (\s@InstancesCount' {} a -> s {unassigning = a} :: InstancesCount)

-- | The number of instances with @connection_lost@ status.
instancesCount_connectionLost :: Lens.Lens' InstancesCount (Core.Maybe Core.Int)
instancesCount_connectionLost = Lens.lens (\InstancesCount' {connectionLost} -> connectionLost) (\s@InstancesCount' {} a -> s {connectionLost = a} :: InstancesCount)

instance Core.FromJSON InstancesCount where
  parseJSON =
    Core.withObject
      "InstancesCount"
      ( \x ->
          InstancesCount'
            Core.<$> (x Core..:? "Online")
            Core.<*> (x Core..:? "SetupFailed")
            Core.<*> (x Core..:? "Registering")
            Core.<*> (x Core..:? "Booting")
            Core.<*> (x Core..:? "StopFailed")
            Core.<*> (x Core..:? "StartFailed")
            Core.<*> (x Core..:? "RunningSetup")
            Core.<*> (x Core..:? "Terminated")
            Core.<*> (x Core..:? "Pending")
            Core.<*> (x Core..:? "Terminating")
            Core.<*> (x Core..:? "ShuttingDown")
            Core.<*> (x Core..:? "Assigning")
            Core.<*> (x Core..:? "Stopped")
            Core.<*> (x Core..:? "Rebooting")
            Core.<*> (x Core..:? "Registered")
            Core.<*> (x Core..:? "Requested")
            Core.<*> (x Core..:? "Deregistering")
            Core.<*> (x Core..:? "Stopping")
            Core.<*> (x Core..:? "Unassigning")
            Core.<*> (x Core..:? "ConnectionLost")
      )

instance Core.Hashable InstancesCount

instance Core.NFData InstancesCount
