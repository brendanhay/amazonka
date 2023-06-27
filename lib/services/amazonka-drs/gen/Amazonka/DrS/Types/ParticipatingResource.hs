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
-- Module      : Amazonka.DrS.Types.ParticipatingResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.ParticipatingResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.LaunchStatus
import Amazonka.DrS.Types.ParticipatingResourceID
import qualified Amazonka.Prelude as Prelude

-- | Represents a resource participating in an asynchronous Job.
--
-- /See:/ 'newParticipatingResource' smart constructor.
data ParticipatingResource = ParticipatingResource'
  { -- | The launch status of a participating resource.
    launchStatus :: Prelude.Maybe LaunchStatus,
    -- | The ID of a participating resource.
    participatingResourceID :: Prelude.Maybe ParticipatingResourceID
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParticipatingResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchStatus', 'participatingResource_launchStatus' - The launch status of a participating resource.
--
-- 'participatingResourceID', 'participatingResource_participatingResourceID' - The ID of a participating resource.
newParticipatingResource ::
  ParticipatingResource
newParticipatingResource =
  ParticipatingResource'
    { launchStatus =
        Prelude.Nothing,
      participatingResourceID = Prelude.Nothing
    }

-- | The launch status of a participating resource.
participatingResource_launchStatus :: Lens.Lens' ParticipatingResource (Prelude.Maybe LaunchStatus)
participatingResource_launchStatus = Lens.lens (\ParticipatingResource' {launchStatus} -> launchStatus) (\s@ParticipatingResource' {} a -> s {launchStatus = a} :: ParticipatingResource)

-- | The ID of a participating resource.
participatingResource_participatingResourceID :: Lens.Lens' ParticipatingResource (Prelude.Maybe ParticipatingResourceID)
participatingResource_participatingResourceID = Lens.lens (\ParticipatingResource' {participatingResourceID} -> participatingResourceID) (\s@ParticipatingResource' {} a -> s {participatingResourceID = a} :: ParticipatingResource)

instance Data.FromJSON ParticipatingResource where
  parseJSON =
    Data.withObject
      "ParticipatingResource"
      ( \x ->
          ParticipatingResource'
            Prelude.<$> (x Data..:? "launchStatus")
            Prelude.<*> (x Data..:? "participatingResourceID")
      )

instance Prelude.Hashable ParticipatingResource where
  hashWithSalt _salt ParticipatingResource' {..} =
    _salt
      `Prelude.hashWithSalt` launchStatus
      `Prelude.hashWithSalt` participatingResourceID

instance Prelude.NFData ParticipatingResource where
  rnf ParticipatingResource' {..} =
    Prelude.rnf launchStatus
      `Prelude.seq` Prelude.rnf participatingResourceID
