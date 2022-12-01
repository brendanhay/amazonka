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
-- Module      : Amazonka.Synthetics.Types.CanaryLastRun
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.CanaryLastRun where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Synthetics.Types.CanaryRun

-- | This structure contains information about the most recent run of a
-- single canary.
--
-- /See:/ 'newCanaryLastRun' smart constructor.
data CanaryLastRun = CanaryLastRun'
  { -- | The results from this canary\'s most recent run.
    lastRun :: Prelude.Maybe CanaryRun,
    -- | The name of the canary.
    canaryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CanaryLastRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastRun', 'canaryLastRun_lastRun' - The results from this canary\'s most recent run.
--
-- 'canaryName', 'canaryLastRun_canaryName' - The name of the canary.
newCanaryLastRun ::
  CanaryLastRun
newCanaryLastRun =
  CanaryLastRun'
    { lastRun = Prelude.Nothing,
      canaryName = Prelude.Nothing
    }

-- | The results from this canary\'s most recent run.
canaryLastRun_lastRun :: Lens.Lens' CanaryLastRun (Prelude.Maybe CanaryRun)
canaryLastRun_lastRun = Lens.lens (\CanaryLastRun' {lastRun} -> lastRun) (\s@CanaryLastRun' {} a -> s {lastRun = a} :: CanaryLastRun)

-- | The name of the canary.
canaryLastRun_canaryName :: Lens.Lens' CanaryLastRun (Prelude.Maybe Prelude.Text)
canaryLastRun_canaryName = Lens.lens (\CanaryLastRun' {canaryName} -> canaryName) (\s@CanaryLastRun' {} a -> s {canaryName = a} :: CanaryLastRun)

instance Core.FromJSON CanaryLastRun where
  parseJSON =
    Core.withObject
      "CanaryLastRun"
      ( \x ->
          CanaryLastRun'
            Prelude.<$> (x Core..:? "LastRun")
            Prelude.<*> (x Core..:? "CanaryName")
      )

instance Prelude.Hashable CanaryLastRun where
  hashWithSalt _salt CanaryLastRun' {..} =
    _salt `Prelude.hashWithSalt` lastRun
      `Prelude.hashWithSalt` canaryName

instance Prelude.NFData CanaryLastRun where
  rnf CanaryLastRun' {..} =
    Prelude.rnf lastRun
      `Prelude.seq` Prelude.rnf canaryName
