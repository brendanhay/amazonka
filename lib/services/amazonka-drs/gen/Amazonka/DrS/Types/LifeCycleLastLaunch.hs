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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.LifeCycleLastLaunch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DrS.Types.LifeCycleLastLaunchInitiated
import qualified Amazonka.Prelude as Prelude

-- | An object containing information regarding the last launch of a Source
-- Server.
--
-- /See:/ 'newLifeCycleLastLaunch' smart constructor.
data LifeCycleLastLaunch = LifeCycleLastLaunch'
  { -- | An object containing information regarding the initiation of the last
    -- launch of a Source Server.
    initiated :: Prelude.Maybe LifeCycleLastLaunchInitiated
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
newLifeCycleLastLaunch ::
  LifeCycleLastLaunch
newLifeCycleLastLaunch =
  LifeCycleLastLaunch' {initiated = Prelude.Nothing}

-- | An object containing information regarding the initiation of the last
-- launch of a Source Server.
lifeCycleLastLaunch_initiated :: Lens.Lens' LifeCycleLastLaunch (Prelude.Maybe LifeCycleLastLaunchInitiated)
lifeCycleLastLaunch_initiated = Lens.lens (\LifeCycleLastLaunch' {initiated} -> initiated) (\s@LifeCycleLastLaunch' {} a -> s {initiated = a} :: LifeCycleLastLaunch)

instance Core.FromJSON LifeCycleLastLaunch where
  parseJSON =
    Core.withObject
      "LifeCycleLastLaunch"
      ( \x ->
          LifeCycleLastLaunch'
            Prelude.<$> (x Core..:? "initiated")
      )

instance Prelude.Hashable LifeCycleLastLaunch where
  hashWithSalt _salt LifeCycleLastLaunch' {..} =
    _salt `Prelude.hashWithSalt` initiated

instance Prelude.NFData LifeCycleLastLaunch where
  rnf LifeCycleLastLaunch' {..} = Prelude.rnf initiated
