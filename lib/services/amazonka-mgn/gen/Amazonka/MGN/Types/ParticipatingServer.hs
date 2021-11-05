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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ParticipatingServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MGN.Types.LaunchStatus
import qualified Amazonka.Prelude as Prelude

-- | Server participating in Job.
--
-- /See:/ 'newParticipatingServer' smart constructor.
data ParticipatingServer = ParticipatingServer'
  { -- | Participating server launch status.
    launchStatus :: Prelude.Maybe LaunchStatus,
    -- | Participating server Source Server ID.
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
-- 'launchStatus', 'participatingServer_launchStatus' - Participating server launch status.
--
-- 'sourceServerID', 'participatingServer_sourceServerID' - Participating server Source Server ID.
newParticipatingServer ::
  ParticipatingServer
newParticipatingServer =
  ParticipatingServer'
    { launchStatus =
        Prelude.Nothing,
      sourceServerID = Prelude.Nothing
    }

-- | Participating server launch status.
participatingServer_launchStatus :: Lens.Lens' ParticipatingServer (Prelude.Maybe LaunchStatus)
participatingServer_launchStatus = Lens.lens (\ParticipatingServer' {launchStatus} -> launchStatus) (\s@ParticipatingServer' {} a -> s {launchStatus = a} :: ParticipatingServer)

-- | Participating server Source Server ID.
participatingServer_sourceServerID :: Lens.Lens' ParticipatingServer (Prelude.Maybe Prelude.Text)
participatingServer_sourceServerID = Lens.lens (\ParticipatingServer' {sourceServerID} -> sourceServerID) (\s@ParticipatingServer' {} a -> s {sourceServerID = a} :: ParticipatingServer)

instance Core.FromJSON ParticipatingServer where
  parseJSON =
    Core.withObject
      "ParticipatingServer"
      ( \x ->
          ParticipatingServer'
            Prelude.<$> (x Core..:? "launchStatus")
            Prelude.<*> (x Core..:? "sourceServerID")
      )

instance Prelude.Hashable ParticipatingServer

instance Prelude.NFData ParticipatingServer
