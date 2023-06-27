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
-- Module      : Amazonka.DrS.Types.LifeCycleLastLaunch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.LifeCycleLastLaunch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.LaunchStatus
import Amazonka.DrS.Types.LifeCycleLastLaunchInitiated
import qualified Amazonka.Prelude as Prelude

-- | An object containing information regarding the last launch of a Source
-- Server.
--
-- /See:/ 'newLifeCycleLastLaunch' smart constructor.
data LifeCycleLastLaunch = LifeCycleLastLaunch'
  { -- | An object containing information regarding the initiation of the last
    -- launch of a Source Server.
    initiated :: Prelude.Maybe LifeCycleLastLaunchInitiated,
    -- | Status of Source Server\'s last launch.
    status :: Prelude.Maybe LaunchStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifeCycleLastLaunch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initiated', 'lifeCycleLastLaunch_initiated' - An object containing information regarding the initiation of the last
-- launch of a Source Server.
--
-- 'status', 'lifeCycleLastLaunch_status' - Status of Source Server\'s last launch.
newLifeCycleLastLaunch ::
  LifeCycleLastLaunch
newLifeCycleLastLaunch =
  LifeCycleLastLaunch'
    { initiated = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | An object containing information regarding the initiation of the last
-- launch of a Source Server.
lifeCycleLastLaunch_initiated :: Lens.Lens' LifeCycleLastLaunch (Prelude.Maybe LifeCycleLastLaunchInitiated)
lifeCycleLastLaunch_initiated = Lens.lens (\LifeCycleLastLaunch' {initiated} -> initiated) (\s@LifeCycleLastLaunch' {} a -> s {initiated = a} :: LifeCycleLastLaunch)

-- | Status of Source Server\'s last launch.
lifeCycleLastLaunch_status :: Lens.Lens' LifeCycleLastLaunch (Prelude.Maybe LaunchStatus)
lifeCycleLastLaunch_status = Lens.lens (\LifeCycleLastLaunch' {status} -> status) (\s@LifeCycleLastLaunch' {} a -> s {status = a} :: LifeCycleLastLaunch)

instance Data.FromJSON LifeCycleLastLaunch where
  parseJSON =
    Data.withObject
      "LifeCycleLastLaunch"
      ( \x ->
          LifeCycleLastLaunch'
            Prelude.<$> (x Data..:? "initiated")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable LifeCycleLastLaunch where
  hashWithSalt _salt LifeCycleLastLaunch' {..} =
    _salt
      `Prelude.hashWithSalt` initiated
      `Prelude.hashWithSalt` status

instance Prelude.NFData LifeCycleLastLaunch where
  rnf LifeCycleLastLaunch' {..} =
    Prelude.rnf initiated
      `Prelude.seq` Prelude.rnf status
