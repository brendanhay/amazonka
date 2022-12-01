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
-- Module      : Amazonka.OpsWorks.Types.InstancesCount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.InstancesCount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes how many instances a stack has for each status.
--
-- /See:/ 'newInstancesCount' smart constructor.
data InstancesCount = InstancesCount'
  { -- | The number of instances with @booting@ status.
    booting :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Deregistering state.
    deregistering :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @running_setup@ status.
    runningSetup :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @stop_failed@ status.
    stopFailed :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @terminated@ status.
    terminated :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @requested@ status.
    requested :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @connection_lost@ status.
    connectionLost :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @online@ status.
    online :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @start_failed@ status.
    startFailed :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @terminating@ status.
    terminating :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Unassigning state.
    unassigning :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @setup_failed@ status.
    setupFailed :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @rebooting@ status.
    rebooting :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Assigning state.
    assigning :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @stopped@ status.
    stopped :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Registered state.
    registered :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @pending@ status.
    pending :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @stopping@ status.
    stopping :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @shutting_down@ status.
    shuttingDown :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Registering state.
    registering :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstancesCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'booting', 'instancesCount_booting' - The number of instances with @booting@ status.
--
-- 'deregistering', 'instancesCount_deregistering' - The number of instances in the Deregistering state.
--
-- 'runningSetup', 'instancesCount_runningSetup' - The number of instances with @running_setup@ status.
--
-- 'stopFailed', 'instancesCount_stopFailed' - The number of instances with @stop_failed@ status.
--
-- 'terminated', 'instancesCount_terminated' - The number of instances with @terminated@ status.
--
-- 'requested', 'instancesCount_requested' - The number of instances with @requested@ status.
--
-- 'connectionLost', 'instancesCount_connectionLost' - The number of instances with @connection_lost@ status.
--
-- 'online', 'instancesCount_online' - The number of instances with @online@ status.
--
-- 'startFailed', 'instancesCount_startFailed' - The number of instances with @start_failed@ status.
--
-- 'terminating', 'instancesCount_terminating' - The number of instances with @terminating@ status.
--
-- 'unassigning', 'instancesCount_unassigning' - The number of instances in the Unassigning state.
--
-- 'setupFailed', 'instancesCount_setupFailed' - The number of instances with @setup_failed@ status.
--
-- 'rebooting', 'instancesCount_rebooting' - The number of instances with @rebooting@ status.
--
-- 'assigning', 'instancesCount_assigning' - The number of instances in the Assigning state.
--
-- 'stopped', 'instancesCount_stopped' - The number of instances with @stopped@ status.
--
-- 'registered', 'instancesCount_registered' - The number of instances in the Registered state.
--
-- 'pending', 'instancesCount_pending' - The number of instances with @pending@ status.
--
-- 'stopping', 'instancesCount_stopping' - The number of instances with @stopping@ status.
--
-- 'shuttingDown', 'instancesCount_shuttingDown' - The number of instances with @shutting_down@ status.
--
-- 'registering', 'instancesCount_registering' - The number of instances in the Registering state.
newInstancesCount ::
  InstancesCount
newInstancesCount =
  InstancesCount'
    { booting = Prelude.Nothing,
      deregistering = Prelude.Nothing,
      runningSetup = Prelude.Nothing,
      stopFailed = Prelude.Nothing,
      terminated = Prelude.Nothing,
      requested = Prelude.Nothing,
      connectionLost = Prelude.Nothing,
      online = Prelude.Nothing,
      startFailed = Prelude.Nothing,
      terminating = Prelude.Nothing,
      unassigning = Prelude.Nothing,
      setupFailed = Prelude.Nothing,
      rebooting = Prelude.Nothing,
      assigning = Prelude.Nothing,
      stopped = Prelude.Nothing,
      registered = Prelude.Nothing,
      pending = Prelude.Nothing,
      stopping = Prelude.Nothing,
      shuttingDown = Prelude.Nothing,
      registering = Prelude.Nothing
    }

-- | The number of instances with @booting@ status.
instancesCount_booting :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_booting = Lens.lens (\InstancesCount' {booting} -> booting) (\s@InstancesCount' {} a -> s {booting = a} :: InstancesCount)

-- | The number of instances in the Deregistering state.
instancesCount_deregistering :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_deregistering = Lens.lens (\InstancesCount' {deregistering} -> deregistering) (\s@InstancesCount' {} a -> s {deregistering = a} :: InstancesCount)

-- | The number of instances with @running_setup@ status.
instancesCount_runningSetup :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_runningSetup = Lens.lens (\InstancesCount' {runningSetup} -> runningSetup) (\s@InstancesCount' {} a -> s {runningSetup = a} :: InstancesCount)

-- | The number of instances with @stop_failed@ status.
instancesCount_stopFailed :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_stopFailed = Lens.lens (\InstancesCount' {stopFailed} -> stopFailed) (\s@InstancesCount' {} a -> s {stopFailed = a} :: InstancesCount)

-- | The number of instances with @terminated@ status.
instancesCount_terminated :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_terminated = Lens.lens (\InstancesCount' {terminated} -> terminated) (\s@InstancesCount' {} a -> s {terminated = a} :: InstancesCount)

-- | The number of instances with @requested@ status.
instancesCount_requested :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_requested = Lens.lens (\InstancesCount' {requested} -> requested) (\s@InstancesCount' {} a -> s {requested = a} :: InstancesCount)

-- | The number of instances with @connection_lost@ status.
instancesCount_connectionLost :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_connectionLost = Lens.lens (\InstancesCount' {connectionLost} -> connectionLost) (\s@InstancesCount' {} a -> s {connectionLost = a} :: InstancesCount)

-- | The number of instances with @online@ status.
instancesCount_online :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_online = Lens.lens (\InstancesCount' {online} -> online) (\s@InstancesCount' {} a -> s {online = a} :: InstancesCount)

-- | The number of instances with @start_failed@ status.
instancesCount_startFailed :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_startFailed = Lens.lens (\InstancesCount' {startFailed} -> startFailed) (\s@InstancesCount' {} a -> s {startFailed = a} :: InstancesCount)

-- | The number of instances with @terminating@ status.
instancesCount_terminating :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_terminating = Lens.lens (\InstancesCount' {terminating} -> terminating) (\s@InstancesCount' {} a -> s {terminating = a} :: InstancesCount)

-- | The number of instances in the Unassigning state.
instancesCount_unassigning :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_unassigning = Lens.lens (\InstancesCount' {unassigning} -> unassigning) (\s@InstancesCount' {} a -> s {unassigning = a} :: InstancesCount)

-- | The number of instances with @setup_failed@ status.
instancesCount_setupFailed :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_setupFailed = Lens.lens (\InstancesCount' {setupFailed} -> setupFailed) (\s@InstancesCount' {} a -> s {setupFailed = a} :: InstancesCount)

-- | The number of instances with @rebooting@ status.
instancesCount_rebooting :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_rebooting = Lens.lens (\InstancesCount' {rebooting} -> rebooting) (\s@InstancesCount' {} a -> s {rebooting = a} :: InstancesCount)

-- | The number of instances in the Assigning state.
instancesCount_assigning :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_assigning = Lens.lens (\InstancesCount' {assigning} -> assigning) (\s@InstancesCount' {} a -> s {assigning = a} :: InstancesCount)

-- | The number of instances with @stopped@ status.
instancesCount_stopped :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_stopped = Lens.lens (\InstancesCount' {stopped} -> stopped) (\s@InstancesCount' {} a -> s {stopped = a} :: InstancesCount)

-- | The number of instances in the Registered state.
instancesCount_registered :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_registered = Lens.lens (\InstancesCount' {registered} -> registered) (\s@InstancesCount' {} a -> s {registered = a} :: InstancesCount)

-- | The number of instances with @pending@ status.
instancesCount_pending :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_pending = Lens.lens (\InstancesCount' {pending} -> pending) (\s@InstancesCount' {} a -> s {pending = a} :: InstancesCount)

-- | The number of instances with @stopping@ status.
instancesCount_stopping :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_stopping = Lens.lens (\InstancesCount' {stopping} -> stopping) (\s@InstancesCount' {} a -> s {stopping = a} :: InstancesCount)

-- | The number of instances with @shutting_down@ status.
instancesCount_shuttingDown :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_shuttingDown = Lens.lens (\InstancesCount' {shuttingDown} -> shuttingDown) (\s@InstancesCount' {} a -> s {shuttingDown = a} :: InstancesCount)

-- | The number of instances in the Registering state.
instancesCount_registering :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_registering = Lens.lens (\InstancesCount' {registering} -> registering) (\s@InstancesCount' {} a -> s {registering = a} :: InstancesCount)

instance Core.FromJSON InstancesCount where
  parseJSON =
    Core.withObject
      "InstancesCount"
      ( \x ->
          InstancesCount'
            Prelude.<$> (x Core..:? "Booting")
            Prelude.<*> (x Core..:? "Deregistering")
            Prelude.<*> (x Core..:? "RunningSetup")
            Prelude.<*> (x Core..:? "StopFailed")
            Prelude.<*> (x Core..:? "Terminated")
            Prelude.<*> (x Core..:? "Requested")
            Prelude.<*> (x Core..:? "ConnectionLost")
            Prelude.<*> (x Core..:? "Online")
            Prelude.<*> (x Core..:? "StartFailed")
            Prelude.<*> (x Core..:? "Terminating")
            Prelude.<*> (x Core..:? "Unassigning")
            Prelude.<*> (x Core..:? "SetupFailed")
            Prelude.<*> (x Core..:? "Rebooting")
            Prelude.<*> (x Core..:? "Assigning")
            Prelude.<*> (x Core..:? "Stopped")
            Prelude.<*> (x Core..:? "Registered")
            Prelude.<*> (x Core..:? "Pending")
            Prelude.<*> (x Core..:? "Stopping")
            Prelude.<*> (x Core..:? "ShuttingDown")
            Prelude.<*> (x Core..:? "Registering")
      )

instance Prelude.Hashable InstancesCount where
  hashWithSalt _salt InstancesCount' {..} =
    _salt `Prelude.hashWithSalt` booting
      `Prelude.hashWithSalt` deregistering
      `Prelude.hashWithSalt` runningSetup
      `Prelude.hashWithSalt` stopFailed
      `Prelude.hashWithSalt` terminated
      `Prelude.hashWithSalt` requested
      `Prelude.hashWithSalt` connectionLost
      `Prelude.hashWithSalt` online
      `Prelude.hashWithSalt` startFailed
      `Prelude.hashWithSalt` terminating
      `Prelude.hashWithSalt` unassigning
      `Prelude.hashWithSalt` setupFailed
      `Prelude.hashWithSalt` rebooting
      `Prelude.hashWithSalt` assigning
      `Prelude.hashWithSalt` stopped
      `Prelude.hashWithSalt` registered
      `Prelude.hashWithSalt` pending
      `Prelude.hashWithSalt` stopping
      `Prelude.hashWithSalt` shuttingDown
      `Prelude.hashWithSalt` registering

instance Prelude.NFData InstancesCount where
  rnf InstancesCount' {..} =
    Prelude.rnf booting
      `Prelude.seq` Prelude.rnf deregistering
      `Prelude.seq` Prelude.rnf runningSetup
      `Prelude.seq` Prelude.rnf stopFailed
      `Prelude.seq` Prelude.rnf terminated
      `Prelude.seq` Prelude.rnf requested
      `Prelude.seq` Prelude.rnf connectionLost
      `Prelude.seq` Prelude.rnf online
      `Prelude.seq` Prelude.rnf startFailed
      `Prelude.seq` Prelude.rnf terminating
      `Prelude.seq` Prelude.rnf unassigning
      `Prelude.seq` Prelude.rnf setupFailed
      `Prelude.seq` Prelude.rnf rebooting
      `Prelude.seq` Prelude.rnf assigning
      `Prelude.seq` Prelude.rnf stopped
      `Prelude.seq` Prelude.rnf registered
      `Prelude.seq` Prelude.rnf pending
      `Prelude.seq` Prelude.rnf stopping
      `Prelude.seq` Prelude.rnf shuttingDown
      `Prelude.seq` Prelude.rnf registering
