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
-- Module      : Amazonka.EMR.Types.InstanceFleetStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceFleetStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.InstanceFleetState
import Amazonka.EMR.Types.InstanceFleetStateChangeReason
import Amazonka.EMR.Types.InstanceFleetTimeline
import qualified Amazonka.Prelude as Prelude

-- | The status of the instance fleet.
--
-- The instance fleet configuration is available only in Amazon EMR
-- releases 4.8.0 and later, excluding 5.0.x versions.
--
-- /See:/ 'newInstanceFleetStatus' smart constructor.
data InstanceFleetStatus = InstanceFleetStatus'
  { -- | A code representing the instance fleet status.
    --
    -- -   @PROVISIONING@—The instance fleet is provisioning Amazon EC2
    --     resources and is not yet ready to run jobs.
    --
    -- -   @BOOTSTRAPPING@—Amazon EC2 instances and other resources have been
    --     provisioned and the bootstrap actions specified for the instances
    --     are underway.
    --
    -- -   @RUNNING@—Amazon EC2 instances and other resources are running. They
    --     are either executing jobs or waiting to execute jobs.
    --
    -- -   @RESIZING@—A resize operation is underway. Amazon EC2 instances are
    --     either being added or removed.
    --
    -- -   @SUSPENDED@—A resize operation could not complete. Existing Amazon
    --     EC2 instances are running, but instances can\'t be added or removed.
    --
    -- -   @TERMINATING@—The instance fleet is terminating Amazon EC2
    --     instances.
    --
    -- -   @TERMINATED@—The instance fleet is no longer active, and all Amazon
    --     EC2 instances have been terminated.
    state :: Prelude.Maybe InstanceFleetState,
    -- | Provides status change reason details for the instance fleet.
    stateChangeReason :: Prelude.Maybe InstanceFleetStateChangeReason,
    -- | Provides historical timestamps for the instance fleet, including the
    -- time of creation, the time it became ready to run jobs, and the time of
    -- termination.
    timeline :: Prelude.Maybe InstanceFleetTimeline
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceFleetStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'instanceFleetStatus_state' - A code representing the instance fleet status.
--
-- -   @PROVISIONING@—The instance fleet is provisioning Amazon EC2
--     resources and is not yet ready to run jobs.
--
-- -   @BOOTSTRAPPING@—Amazon EC2 instances and other resources have been
--     provisioned and the bootstrap actions specified for the instances
--     are underway.
--
-- -   @RUNNING@—Amazon EC2 instances and other resources are running. They
--     are either executing jobs or waiting to execute jobs.
--
-- -   @RESIZING@—A resize operation is underway. Amazon EC2 instances are
--     either being added or removed.
--
-- -   @SUSPENDED@—A resize operation could not complete. Existing Amazon
--     EC2 instances are running, but instances can\'t be added or removed.
--
-- -   @TERMINATING@—The instance fleet is terminating Amazon EC2
--     instances.
--
-- -   @TERMINATED@—The instance fleet is no longer active, and all Amazon
--     EC2 instances have been terminated.
--
-- 'stateChangeReason', 'instanceFleetStatus_stateChangeReason' - Provides status change reason details for the instance fleet.
--
-- 'timeline', 'instanceFleetStatus_timeline' - Provides historical timestamps for the instance fleet, including the
-- time of creation, the time it became ready to run jobs, and the time of
-- termination.
newInstanceFleetStatus ::
  InstanceFleetStatus
newInstanceFleetStatus =
  InstanceFleetStatus'
    { state = Prelude.Nothing,
      stateChangeReason = Prelude.Nothing,
      timeline = Prelude.Nothing
    }

-- | A code representing the instance fleet status.
--
-- -   @PROVISIONING@—The instance fleet is provisioning Amazon EC2
--     resources and is not yet ready to run jobs.
--
-- -   @BOOTSTRAPPING@—Amazon EC2 instances and other resources have been
--     provisioned and the bootstrap actions specified for the instances
--     are underway.
--
-- -   @RUNNING@—Amazon EC2 instances and other resources are running. They
--     are either executing jobs or waiting to execute jobs.
--
-- -   @RESIZING@—A resize operation is underway. Amazon EC2 instances are
--     either being added or removed.
--
-- -   @SUSPENDED@—A resize operation could not complete. Existing Amazon
--     EC2 instances are running, but instances can\'t be added or removed.
--
-- -   @TERMINATING@—The instance fleet is terminating Amazon EC2
--     instances.
--
-- -   @TERMINATED@—The instance fleet is no longer active, and all Amazon
--     EC2 instances have been terminated.
instanceFleetStatus_state :: Lens.Lens' InstanceFleetStatus (Prelude.Maybe InstanceFleetState)
instanceFleetStatus_state = Lens.lens (\InstanceFleetStatus' {state} -> state) (\s@InstanceFleetStatus' {} a -> s {state = a} :: InstanceFleetStatus)

-- | Provides status change reason details for the instance fleet.
instanceFleetStatus_stateChangeReason :: Lens.Lens' InstanceFleetStatus (Prelude.Maybe InstanceFleetStateChangeReason)
instanceFleetStatus_stateChangeReason = Lens.lens (\InstanceFleetStatus' {stateChangeReason} -> stateChangeReason) (\s@InstanceFleetStatus' {} a -> s {stateChangeReason = a} :: InstanceFleetStatus)

-- | Provides historical timestamps for the instance fleet, including the
-- time of creation, the time it became ready to run jobs, and the time of
-- termination.
instanceFleetStatus_timeline :: Lens.Lens' InstanceFleetStatus (Prelude.Maybe InstanceFleetTimeline)
instanceFleetStatus_timeline = Lens.lens (\InstanceFleetStatus' {timeline} -> timeline) (\s@InstanceFleetStatus' {} a -> s {timeline = a} :: InstanceFleetStatus)

instance Data.FromJSON InstanceFleetStatus where
  parseJSON =
    Data.withObject
      "InstanceFleetStatus"
      ( \x ->
          InstanceFleetStatus'
            Prelude.<$> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StateChangeReason")
            Prelude.<*> (x Data..:? "Timeline")
      )

instance Prelude.Hashable InstanceFleetStatus where
  hashWithSalt _salt InstanceFleetStatus' {..} =
    _salt
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateChangeReason
      `Prelude.hashWithSalt` timeline

instance Prelude.NFData InstanceFleetStatus where
  rnf InstanceFleetStatus' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateChangeReason
      `Prelude.seq` Prelude.rnf timeline
