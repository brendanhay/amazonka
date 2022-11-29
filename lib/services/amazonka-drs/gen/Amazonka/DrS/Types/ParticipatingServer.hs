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
-- Module      : Amazonka.DrS.Types.ParticipatingServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.ParticipatingServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DrS.Types.LaunchStatus
import qualified Amazonka.Prelude as Prelude

-- | Represents a server participating in an asynchronous Job.
--
-- /See:/ 'newParticipatingServer' smart constructor.
data ParticipatingServer = ParticipatingServer'
  { -- | The launch status of a participating server.
    launchStatus :: Prelude.Maybe LaunchStatus,
    -- | The Recovery Instance ID of a participating server.
    recoveryInstanceID :: Prelude.Maybe Prelude.Text,
    -- | The Source Server ID of a participating server.
    sourceServerID :: Prelude.Maybe Prelude.Text
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
-- 'launchStatus', 'participatingServer_launchStatus' - The launch status of a participating server.
--
-- 'recoveryInstanceID', 'participatingServer_recoveryInstanceID' - The Recovery Instance ID of a participating server.
--
-- 'sourceServerID', 'participatingServer_sourceServerID' - The Source Server ID of a participating server.
newParticipatingServer ::
  ParticipatingServer
newParticipatingServer =
  ParticipatingServer'
    { launchStatus =
        Prelude.Nothing,
      recoveryInstanceID = Prelude.Nothing,
      sourceServerID = Prelude.Nothing
    }

-- | The launch status of a participating server.
participatingServer_launchStatus :: Lens.Lens' ParticipatingServer (Prelude.Maybe LaunchStatus)
participatingServer_launchStatus = Lens.lens (\ParticipatingServer' {launchStatus} -> launchStatus) (\s@ParticipatingServer' {} a -> s {launchStatus = a} :: ParticipatingServer)

-- | The Recovery Instance ID of a participating server.
participatingServer_recoveryInstanceID :: Lens.Lens' ParticipatingServer (Prelude.Maybe Prelude.Text)
participatingServer_recoveryInstanceID = Lens.lens (\ParticipatingServer' {recoveryInstanceID} -> recoveryInstanceID) (\s@ParticipatingServer' {} a -> s {recoveryInstanceID = a} :: ParticipatingServer)

-- | The Source Server ID of a participating server.
participatingServer_sourceServerID :: Lens.Lens' ParticipatingServer (Prelude.Maybe Prelude.Text)
participatingServer_sourceServerID = Lens.lens (\ParticipatingServer' {sourceServerID} -> sourceServerID) (\s@ParticipatingServer' {} a -> s {sourceServerID = a} :: ParticipatingServer)

instance Core.FromJSON ParticipatingServer where
  parseJSON =
    Core.withObject
      "ParticipatingServer"
      ( \x ->
          ParticipatingServer'
            Prelude.<$> (x Core..:? "launchStatus")
            Prelude.<*> (x Core..:? "recoveryInstanceID")
            Prelude.<*> (x Core..:? "sourceServerID")
      )

instance Prelude.Hashable ParticipatingServer where
  hashWithSalt _salt ParticipatingServer' {..} =
    _salt `Prelude.hashWithSalt` launchStatus
      `Prelude.hashWithSalt` recoveryInstanceID
      `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData ParticipatingServer where
  rnf ParticipatingServer' {..} =
    Prelude.rnf launchStatus
      `Prelude.seq` Prelude.rnf recoveryInstanceID
      `Prelude.seq` Prelude.rnf sourceServerID
