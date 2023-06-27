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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.InstancesCount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes how many instances a stack has for each status.
--
-- /See:/ 'newInstancesCount' smart constructor.
data InstancesCount = InstancesCount'
  { -- | The number of instances in the Assigning state.
    assigning :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @booting@ status.
    booting :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @connection_lost@ status.
    connectionLost :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Deregistering state.
    deregistering :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @online@ status.
    online :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @pending@ status.
    pending :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @rebooting@ status.
    rebooting :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Registered state.
    registered :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Registering state.
    registering :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @requested@ status.
    requested :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @running_setup@ status.
    runningSetup :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @setup_failed@ status.
    setupFailed :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @shutting_down@ status.
    shuttingDown :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @start_failed@ status.
    startFailed :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @stop_failed@ status.
    stopFailed :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @stopped@ status.
    stopped :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @stopping@ status.
    stopping :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @terminated@ status.
    terminated :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @terminating@ status.
    terminating :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Unassigning state.
    unassigning :: Prelude.Maybe Prelude.Int
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
-- 'assigning', 'instancesCount_assigning' - The number of instances in the Assigning state.
--
-- 'booting', 'instancesCount_booting' - The number of instances with @booting@ status.
--
-- 'connectionLost', 'instancesCount_connectionLost' - The number of instances with @connection_lost@ status.
--
-- 'deregistering', 'instancesCount_deregistering' - The number of instances in the Deregistering state.
--
-- 'online', 'instancesCount_online' - The number of instances with @online@ status.
--
-- 'pending', 'instancesCount_pending' - The number of instances with @pending@ status.
--
-- 'rebooting', 'instancesCount_rebooting' - The number of instances with @rebooting@ status.
--
-- 'registered', 'instancesCount_registered' - The number of instances in the Registered state.
--
-- 'registering', 'instancesCount_registering' - The number of instances in the Registering state.
--
-- 'requested', 'instancesCount_requested' - The number of instances with @requested@ status.
--
-- 'runningSetup', 'instancesCount_runningSetup' - The number of instances with @running_setup@ status.
--
-- 'setupFailed', 'instancesCount_setupFailed' - The number of instances with @setup_failed@ status.
--
-- 'shuttingDown', 'instancesCount_shuttingDown' - The number of instances with @shutting_down@ status.
--
-- 'startFailed', 'instancesCount_startFailed' - The number of instances with @start_failed@ status.
--
-- 'stopFailed', 'instancesCount_stopFailed' - The number of instances with @stop_failed@ status.
--
-- 'stopped', 'instancesCount_stopped' - The number of instances with @stopped@ status.
--
-- 'stopping', 'instancesCount_stopping' - The number of instances with @stopping@ status.
--
-- 'terminated', 'instancesCount_terminated' - The number of instances with @terminated@ status.
--
-- 'terminating', 'instancesCount_terminating' - The number of instances with @terminating@ status.
--
-- 'unassigning', 'instancesCount_unassigning' - The number of instances in the Unassigning state.
newInstancesCount ::
  InstancesCount
newInstancesCount =
  InstancesCount'
    { assigning = Prelude.Nothing,
      booting = Prelude.Nothing,
      connectionLost = Prelude.Nothing,
      deregistering = Prelude.Nothing,
      online = Prelude.Nothing,
      pending = Prelude.Nothing,
      rebooting = Prelude.Nothing,
      registered = Prelude.Nothing,
      registering = Prelude.Nothing,
      requested = Prelude.Nothing,
      runningSetup = Prelude.Nothing,
      setupFailed = Prelude.Nothing,
      shuttingDown = Prelude.Nothing,
      startFailed = Prelude.Nothing,
      stopFailed = Prelude.Nothing,
      stopped = Prelude.Nothing,
      stopping = Prelude.Nothing,
      terminated = Prelude.Nothing,
      terminating = Prelude.Nothing,
      unassigning = Prelude.Nothing
    }

-- | The number of instances in the Assigning state.
instancesCount_assigning :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_assigning = Lens.lens (\InstancesCount' {assigning} -> assigning) (\s@InstancesCount' {} a -> s {assigning = a} :: InstancesCount)

-- | The number of instances with @booting@ status.
instancesCount_booting :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_booting = Lens.lens (\InstancesCount' {booting} -> booting) (\s@InstancesCount' {} a -> s {booting = a} :: InstancesCount)

-- | The number of instances with @connection_lost@ status.
instancesCount_connectionLost :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_connectionLost = Lens.lens (\InstancesCount' {connectionLost} -> connectionLost) (\s@InstancesCount' {} a -> s {connectionLost = a} :: InstancesCount)

-- | The number of instances in the Deregistering state.
instancesCount_deregistering :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_deregistering = Lens.lens (\InstancesCount' {deregistering} -> deregistering) (\s@InstancesCount' {} a -> s {deregistering = a} :: InstancesCount)

-- | The number of instances with @online@ status.
instancesCount_online :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_online = Lens.lens (\InstancesCount' {online} -> online) (\s@InstancesCount' {} a -> s {online = a} :: InstancesCount)

-- | The number of instances with @pending@ status.
instancesCount_pending :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_pending = Lens.lens (\InstancesCount' {pending} -> pending) (\s@InstancesCount' {} a -> s {pending = a} :: InstancesCount)

-- | The number of instances with @rebooting@ status.
instancesCount_rebooting :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_rebooting = Lens.lens (\InstancesCount' {rebooting} -> rebooting) (\s@InstancesCount' {} a -> s {rebooting = a} :: InstancesCount)

-- | The number of instances in the Registered state.
instancesCount_registered :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_registered = Lens.lens (\InstancesCount' {registered} -> registered) (\s@InstancesCount' {} a -> s {registered = a} :: InstancesCount)

-- | The number of instances in the Registering state.
instancesCount_registering :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_registering = Lens.lens (\InstancesCount' {registering} -> registering) (\s@InstancesCount' {} a -> s {registering = a} :: InstancesCount)

-- | The number of instances with @requested@ status.
instancesCount_requested :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_requested = Lens.lens (\InstancesCount' {requested} -> requested) (\s@InstancesCount' {} a -> s {requested = a} :: InstancesCount)

-- | The number of instances with @running_setup@ status.
instancesCount_runningSetup :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_runningSetup = Lens.lens (\InstancesCount' {runningSetup} -> runningSetup) (\s@InstancesCount' {} a -> s {runningSetup = a} :: InstancesCount)

-- | The number of instances with @setup_failed@ status.
instancesCount_setupFailed :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_setupFailed = Lens.lens (\InstancesCount' {setupFailed} -> setupFailed) (\s@InstancesCount' {} a -> s {setupFailed = a} :: InstancesCount)

-- | The number of instances with @shutting_down@ status.
instancesCount_shuttingDown :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_shuttingDown = Lens.lens (\InstancesCount' {shuttingDown} -> shuttingDown) (\s@InstancesCount' {} a -> s {shuttingDown = a} :: InstancesCount)

-- | The number of instances with @start_failed@ status.
instancesCount_startFailed :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_startFailed = Lens.lens (\InstancesCount' {startFailed} -> startFailed) (\s@InstancesCount' {} a -> s {startFailed = a} :: InstancesCount)

-- | The number of instances with @stop_failed@ status.
instancesCount_stopFailed :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_stopFailed = Lens.lens (\InstancesCount' {stopFailed} -> stopFailed) (\s@InstancesCount' {} a -> s {stopFailed = a} :: InstancesCount)

-- | The number of instances with @stopped@ status.
instancesCount_stopped :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_stopped = Lens.lens (\InstancesCount' {stopped} -> stopped) (\s@InstancesCount' {} a -> s {stopped = a} :: InstancesCount)

-- | The number of instances with @stopping@ status.
instancesCount_stopping :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_stopping = Lens.lens (\InstancesCount' {stopping} -> stopping) (\s@InstancesCount' {} a -> s {stopping = a} :: InstancesCount)

-- | The number of instances with @terminated@ status.
instancesCount_terminated :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_terminated = Lens.lens (\InstancesCount' {terminated} -> terminated) (\s@InstancesCount' {} a -> s {terminated = a} :: InstancesCount)

-- | The number of instances with @terminating@ status.
instancesCount_terminating :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_terminating = Lens.lens (\InstancesCount' {terminating} -> terminating) (\s@InstancesCount' {} a -> s {terminating = a} :: InstancesCount)

-- | The number of instances in the Unassigning state.
instancesCount_unassigning :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_unassigning = Lens.lens (\InstancesCount' {unassigning} -> unassigning) (\s@InstancesCount' {} a -> s {unassigning = a} :: InstancesCount)

instance Data.FromJSON InstancesCount where
  parseJSON =
    Data.withObject
      "InstancesCount"
      ( \x ->
          InstancesCount'
            Prelude.<$> (x Data..:? "Assigning")
            Prelude.<*> (x Data..:? "Booting")
            Prelude.<*> (x Data..:? "ConnectionLost")
            Prelude.<*> (x Data..:? "Deregistering")
            Prelude.<*> (x Data..:? "Online")
            Prelude.<*> (x Data..:? "Pending")
            Prelude.<*> (x Data..:? "Rebooting")
            Prelude.<*> (x Data..:? "Registered")
            Prelude.<*> (x Data..:? "Registering")
            Prelude.<*> (x Data..:? "Requested")
            Prelude.<*> (x Data..:? "RunningSetup")
            Prelude.<*> (x Data..:? "SetupFailed")
            Prelude.<*> (x Data..:? "ShuttingDown")
            Prelude.<*> (x Data..:? "StartFailed")
            Prelude.<*> (x Data..:? "StopFailed")
            Prelude.<*> (x Data..:? "Stopped")
            Prelude.<*> (x Data..:? "Stopping")
            Prelude.<*> (x Data..:? "Terminated")
            Prelude.<*> (x Data..:? "Terminating")
            Prelude.<*> (x Data..:? "Unassigning")
      )

instance Prelude.Hashable InstancesCount where
  hashWithSalt _salt InstancesCount' {..} =
    _salt
      `Prelude.hashWithSalt` assigning
      `Prelude.hashWithSalt` booting
      `Prelude.hashWithSalt` connectionLost
      `Prelude.hashWithSalt` deregistering
      `Prelude.hashWithSalt` online
      `Prelude.hashWithSalt` pending
      `Prelude.hashWithSalt` rebooting
      `Prelude.hashWithSalt` registered
      `Prelude.hashWithSalt` registering
      `Prelude.hashWithSalt` requested
      `Prelude.hashWithSalt` runningSetup
      `Prelude.hashWithSalt` setupFailed
      `Prelude.hashWithSalt` shuttingDown
      `Prelude.hashWithSalt` startFailed
      `Prelude.hashWithSalt` stopFailed
      `Prelude.hashWithSalt` stopped
      `Prelude.hashWithSalt` stopping
      `Prelude.hashWithSalt` terminated
      `Prelude.hashWithSalt` terminating
      `Prelude.hashWithSalt` unassigning

instance Prelude.NFData InstancesCount where
  rnf InstancesCount' {..} =
    Prelude.rnf assigning
      `Prelude.seq` Prelude.rnf booting
      `Prelude.seq` Prelude.rnf connectionLost
      `Prelude.seq` Prelude.rnf deregistering
      `Prelude.seq` Prelude.rnf online
      `Prelude.seq` Prelude.rnf pending
      `Prelude.seq` Prelude.rnf rebooting
      `Prelude.seq` Prelude.rnf registered
      `Prelude.seq` Prelude.rnf registering
      `Prelude.seq` Prelude.rnf requested
      `Prelude.seq` Prelude.rnf runningSetup
      `Prelude.seq` Prelude.rnf setupFailed
      `Prelude.seq` Prelude.rnf shuttingDown
      `Prelude.seq` Prelude.rnf startFailed
      `Prelude.seq` Prelude.rnf stopFailed
      `Prelude.seq` Prelude.rnf stopped
      `Prelude.seq` Prelude.rnf stopping
      `Prelude.seq` Prelude.rnf terminated
      `Prelude.seq` Prelude.rnf terminating
      `Prelude.seq` Prelude.rnf unassigning
