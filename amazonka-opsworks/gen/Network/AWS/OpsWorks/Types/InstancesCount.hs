{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes how many instances a stack has for each status.
--
-- /See:/ 'newInstancesCount' smart constructor.
data InstancesCount = InstancesCount'
  { -- | The number of instances with @online@ status.
    online :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @setup_failed@ status.
    setupFailed :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Registering state.
    registering :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @booting@ status.
    booting :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @stop_failed@ status.
    stopFailed :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @start_failed@ status.
    startFailed :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @running_setup@ status.
    runningSetup :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @terminated@ status.
    terminated :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @pending@ status.
    pending :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @terminating@ status.
    terminating :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @shutting_down@ status.
    shuttingDown :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Assigning state.
    assigning :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @stopped@ status.
    stopped :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @rebooting@ status.
    rebooting :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Registered state.
    registered :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @requested@ status.
    requested :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Deregistering state.
    deregistering :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @stopping@ status.
    stopping :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the Unassigning state.
    unassigning :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @connection_lost@ status.
    connectionLost :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { online = Prelude.Nothing,
      setupFailed = Prelude.Nothing,
      registering = Prelude.Nothing,
      booting = Prelude.Nothing,
      stopFailed = Prelude.Nothing,
      startFailed = Prelude.Nothing,
      runningSetup = Prelude.Nothing,
      terminated = Prelude.Nothing,
      pending = Prelude.Nothing,
      terminating = Prelude.Nothing,
      shuttingDown = Prelude.Nothing,
      assigning = Prelude.Nothing,
      stopped = Prelude.Nothing,
      rebooting = Prelude.Nothing,
      registered = Prelude.Nothing,
      requested = Prelude.Nothing,
      deregistering = Prelude.Nothing,
      stopping = Prelude.Nothing,
      unassigning = Prelude.Nothing,
      connectionLost = Prelude.Nothing
    }

-- | The number of instances with @online@ status.
instancesCount_online :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_online = Lens.lens (\InstancesCount' {online} -> online) (\s@InstancesCount' {} a -> s {online = a} :: InstancesCount)

-- | The number of instances with @setup_failed@ status.
instancesCount_setupFailed :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_setupFailed = Lens.lens (\InstancesCount' {setupFailed} -> setupFailed) (\s@InstancesCount' {} a -> s {setupFailed = a} :: InstancesCount)

-- | The number of instances in the Registering state.
instancesCount_registering :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_registering = Lens.lens (\InstancesCount' {registering} -> registering) (\s@InstancesCount' {} a -> s {registering = a} :: InstancesCount)

-- | The number of instances with @booting@ status.
instancesCount_booting :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_booting = Lens.lens (\InstancesCount' {booting} -> booting) (\s@InstancesCount' {} a -> s {booting = a} :: InstancesCount)

-- | The number of instances with @stop_failed@ status.
instancesCount_stopFailed :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_stopFailed = Lens.lens (\InstancesCount' {stopFailed} -> stopFailed) (\s@InstancesCount' {} a -> s {stopFailed = a} :: InstancesCount)

-- | The number of instances with @start_failed@ status.
instancesCount_startFailed :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_startFailed = Lens.lens (\InstancesCount' {startFailed} -> startFailed) (\s@InstancesCount' {} a -> s {startFailed = a} :: InstancesCount)

-- | The number of instances with @running_setup@ status.
instancesCount_runningSetup :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_runningSetup = Lens.lens (\InstancesCount' {runningSetup} -> runningSetup) (\s@InstancesCount' {} a -> s {runningSetup = a} :: InstancesCount)

-- | The number of instances with @terminated@ status.
instancesCount_terminated :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_terminated = Lens.lens (\InstancesCount' {terminated} -> terminated) (\s@InstancesCount' {} a -> s {terminated = a} :: InstancesCount)

-- | The number of instances with @pending@ status.
instancesCount_pending :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_pending = Lens.lens (\InstancesCount' {pending} -> pending) (\s@InstancesCount' {} a -> s {pending = a} :: InstancesCount)

-- | The number of instances with @terminating@ status.
instancesCount_terminating :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_terminating = Lens.lens (\InstancesCount' {terminating} -> terminating) (\s@InstancesCount' {} a -> s {terminating = a} :: InstancesCount)

-- | The number of instances with @shutting_down@ status.
instancesCount_shuttingDown :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_shuttingDown = Lens.lens (\InstancesCount' {shuttingDown} -> shuttingDown) (\s@InstancesCount' {} a -> s {shuttingDown = a} :: InstancesCount)

-- | The number of instances in the Assigning state.
instancesCount_assigning :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_assigning = Lens.lens (\InstancesCount' {assigning} -> assigning) (\s@InstancesCount' {} a -> s {assigning = a} :: InstancesCount)

-- | The number of instances with @stopped@ status.
instancesCount_stopped :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_stopped = Lens.lens (\InstancesCount' {stopped} -> stopped) (\s@InstancesCount' {} a -> s {stopped = a} :: InstancesCount)

-- | The number of instances with @rebooting@ status.
instancesCount_rebooting :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_rebooting = Lens.lens (\InstancesCount' {rebooting} -> rebooting) (\s@InstancesCount' {} a -> s {rebooting = a} :: InstancesCount)

-- | The number of instances in the Registered state.
instancesCount_registered :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_registered = Lens.lens (\InstancesCount' {registered} -> registered) (\s@InstancesCount' {} a -> s {registered = a} :: InstancesCount)

-- | The number of instances with @requested@ status.
instancesCount_requested :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_requested = Lens.lens (\InstancesCount' {requested} -> requested) (\s@InstancesCount' {} a -> s {requested = a} :: InstancesCount)

-- | The number of instances in the Deregistering state.
instancesCount_deregistering :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_deregistering = Lens.lens (\InstancesCount' {deregistering} -> deregistering) (\s@InstancesCount' {} a -> s {deregistering = a} :: InstancesCount)

-- | The number of instances with @stopping@ status.
instancesCount_stopping :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_stopping = Lens.lens (\InstancesCount' {stopping} -> stopping) (\s@InstancesCount' {} a -> s {stopping = a} :: InstancesCount)

-- | The number of instances in the Unassigning state.
instancesCount_unassigning :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_unassigning = Lens.lens (\InstancesCount' {unassigning} -> unassigning) (\s@InstancesCount' {} a -> s {unassigning = a} :: InstancesCount)

-- | The number of instances with @connection_lost@ status.
instancesCount_connectionLost :: Lens.Lens' InstancesCount (Prelude.Maybe Prelude.Int)
instancesCount_connectionLost = Lens.lens (\InstancesCount' {connectionLost} -> connectionLost) (\s@InstancesCount' {} a -> s {connectionLost = a} :: InstancesCount)

instance Prelude.FromJSON InstancesCount where
  parseJSON =
    Prelude.withObject
      "InstancesCount"
      ( \x ->
          InstancesCount'
            Prelude.<$> (x Prelude..:? "Online")
            Prelude.<*> (x Prelude..:? "SetupFailed")
            Prelude.<*> (x Prelude..:? "Registering")
            Prelude.<*> (x Prelude..:? "Booting")
            Prelude.<*> (x Prelude..:? "StopFailed")
            Prelude.<*> (x Prelude..:? "StartFailed")
            Prelude.<*> (x Prelude..:? "RunningSetup")
            Prelude.<*> (x Prelude..:? "Terminated")
            Prelude.<*> (x Prelude..:? "Pending")
            Prelude.<*> (x Prelude..:? "Terminating")
            Prelude.<*> (x Prelude..:? "ShuttingDown")
            Prelude.<*> (x Prelude..:? "Assigning")
            Prelude.<*> (x Prelude..:? "Stopped")
            Prelude.<*> (x Prelude..:? "Rebooting")
            Prelude.<*> (x Prelude..:? "Registered")
            Prelude.<*> (x Prelude..:? "Requested")
            Prelude.<*> (x Prelude..:? "Deregistering")
            Prelude.<*> (x Prelude..:? "Stopping")
            Prelude.<*> (x Prelude..:? "Unassigning")
            Prelude.<*> (x Prelude..:? "ConnectionLost")
      )

instance Prelude.Hashable InstancesCount

instance Prelude.NFData InstancesCount
