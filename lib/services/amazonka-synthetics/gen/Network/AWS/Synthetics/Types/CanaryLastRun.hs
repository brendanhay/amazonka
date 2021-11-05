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
-- Module      : Network.AWS.Synthetics.Types.CanaryLastRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Synthetics.Types.CanaryLastRun where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Synthetics.Types.CanaryRun

-- | This structure contains information about the most recent run of a
-- single canary.
--
-- /See:/ 'newCanaryLastRun' smart constructor.
data CanaryLastRun = CanaryLastRun'
  { -- | The name of the canary.
    canaryName :: Prelude.Maybe Prelude.Text,
    -- | The results from this canary\'s most recent run.
    lastRun :: Prelude.Maybe CanaryRun
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
-- 'canaryName', 'canaryLastRun_canaryName' - The name of the canary.
--
-- 'lastRun', 'canaryLastRun_lastRun' - The results from this canary\'s most recent run.
newCanaryLastRun ::
  CanaryLastRun
newCanaryLastRun =
  CanaryLastRun'
    { canaryName = Prelude.Nothing,
      lastRun = Prelude.Nothing
    }

-- | The name of the canary.
canaryLastRun_canaryName :: Lens.Lens' CanaryLastRun (Prelude.Maybe Prelude.Text)
canaryLastRun_canaryName = Lens.lens (\CanaryLastRun' {canaryName} -> canaryName) (\s@CanaryLastRun' {} a -> s {canaryName = a} :: CanaryLastRun)

-- | The results from this canary\'s most recent run.
canaryLastRun_lastRun :: Lens.Lens' CanaryLastRun (Prelude.Maybe CanaryRun)
canaryLastRun_lastRun = Lens.lens (\CanaryLastRun' {lastRun} -> lastRun) (\s@CanaryLastRun' {} a -> s {lastRun = a} :: CanaryLastRun)

instance Core.FromJSON CanaryLastRun where
  parseJSON =
    Core.withObject
      "CanaryLastRun"
      ( \x ->
          CanaryLastRun'
            Prelude.<$> (x Core..:? "CanaryName")
            Prelude.<*> (x Core..:? "LastRun")
      )

instance Prelude.Hashable CanaryLastRun

instance Prelude.NFData CanaryLastRun
