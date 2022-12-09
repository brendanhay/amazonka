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
-- Module      : Amazonka.MGN.Types.ParticipatingServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ParticipatingServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.LaunchStatus
import Amazonka.MGN.Types.PostLaunchActionsStatus
import qualified Amazonka.Prelude as Prelude

-- | Server participating in Job.
--
-- /See:/ 'newParticipatingServer' smart constructor.
data ParticipatingServer = ParticipatingServer'
  { -- | Participating server launch status.
    launchStatus :: Prelude.Maybe LaunchStatus,
    -- | Participating server\'s launched ec2 instance ID.
    launchedEc2InstanceID :: Prelude.Maybe Prelude.Text,
    -- | Participating server\'s Post Launch Actions Status.
    postLaunchActionsStatus :: Prelude.Maybe PostLaunchActionsStatus,
    -- | Participating server Source Server ID.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParticipatingServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchStatus', 'participatingServer_launchStatus' - Participating server launch status.
--
-- 'launchedEc2InstanceID', 'participatingServer_launchedEc2InstanceID' - Participating server\'s launched ec2 instance ID.
--
-- 'postLaunchActionsStatus', 'participatingServer_postLaunchActionsStatus' - Participating server\'s Post Launch Actions Status.
--
-- 'sourceServerID', 'participatingServer_sourceServerID' - Participating server Source Server ID.
newParticipatingServer ::
  -- | 'sourceServerID'
  Prelude.Text ->
  ParticipatingServer
newParticipatingServer pSourceServerID_ =
  ParticipatingServer'
    { launchStatus =
        Prelude.Nothing,
      launchedEc2InstanceID = Prelude.Nothing,
      postLaunchActionsStatus = Prelude.Nothing,
      sourceServerID = pSourceServerID_
    }

-- | Participating server launch status.
participatingServer_launchStatus :: Lens.Lens' ParticipatingServer (Prelude.Maybe LaunchStatus)
participatingServer_launchStatus = Lens.lens (\ParticipatingServer' {launchStatus} -> launchStatus) (\s@ParticipatingServer' {} a -> s {launchStatus = a} :: ParticipatingServer)

-- | Participating server\'s launched ec2 instance ID.
participatingServer_launchedEc2InstanceID :: Lens.Lens' ParticipatingServer (Prelude.Maybe Prelude.Text)
participatingServer_launchedEc2InstanceID = Lens.lens (\ParticipatingServer' {launchedEc2InstanceID} -> launchedEc2InstanceID) (\s@ParticipatingServer' {} a -> s {launchedEc2InstanceID = a} :: ParticipatingServer)

-- | Participating server\'s Post Launch Actions Status.
participatingServer_postLaunchActionsStatus :: Lens.Lens' ParticipatingServer (Prelude.Maybe PostLaunchActionsStatus)
participatingServer_postLaunchActionsStatus = Lens.lens (\ParticipatingServer' {postLaunchActionsStatus} -> postLaunchActionsStatus) (\s@ParticipatingServer' {} a -> s {postLaunchActionsStatus = a} :: ParticipatingServer)

-- | Participating server Source Server ID.
participatingServer_sourceServerID :: Lens.Lens' ParticipatingServer Prelude.Text
participatingServer_sourceServerID = Lens.lens (\ParticipatingServer' {sourceServerID} -> sourceServerID) (\s@ParticipatingServer' {} a -> s {sourceServerID = a} :: ParticipatingServer)

instance Data.FromJSON ParticipatingServer where
  parseJSON =
    Data.withObject
      "ParticipatingServer"
      ( \x ->
          ParticipatingServer'
            Prelude.<$> (x Data..:? "launchStatus")
            Prelude.<*> (x Data..:? "launchedEc2InstanceID")
            Prelude.<*> (x Data..:? "postLaunchActionsStatus")
            Prelude.<*> (x Data..: "sourceServerID")
      )

instance Prelude.Hashable ParticipatingServer where
  hashWithSalt _salt ParticipatingServer' {..} =
    _salt `Prelude.hashWithSalt` launchStatus
      `Prelude.hashWithSalt` launchedEc2InstanceID
      `Prelude.hashWithSalt` postLaunchActionsStatus
      `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData ParticipatingServer where
  rnf ParticipatingServer' {..} =
    Prelude.rnf launchStatus
      `Prelude.seq` Prelude.rnf launchedEc2InstanceID
      `Prelude.seq` Prelude.rnf postLaunchActionsStatus
      `Prelude.seq` Prelude.rnf sourceServerID
