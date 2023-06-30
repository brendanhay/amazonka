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
-- Module      : Amazonka.Synthetics.Types.CanaryRunConfigOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.CanaryRunConfigOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information about a canary run.
--
-- /See:/ 'newCanaryRunConfigOutput' smart constructor.
data CanaryRunConfigOutput = CanaryRunConfigOutput'
  { -- | Displays whether this canary run used active X-Ray tracing.
    activeTracing :: Prelude.Maybe Prelude.Bool,
    -- | The maximum amount of memory available to the canary while it is
    -- running, in MB. This value must be a multiple of 64.
    memoryInMB :: Prelude.Maybe Prelude.Natural,
    -- | How long the canary is allowed to run before it must stop.
    timeoutInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CanaryRunConfigOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeTracing', 'canaryRunConfigOutput_activeTracing' - Displays whether this canary run used active X-Ray tracing.
--
-- 'memoryInMB', 'canaryRunConfigOutput_memoryInMB' - The maximum amount of memory available to the canary while it is
-- running, in MB. This value must be a multiple of 64.
--
-- 'timeoutInSeconds', 'canaryRunConfigOutput_timeoutInSeconds' - How long the canary is allowed to run before it must stop.
newCanaryRunConfigOutput ::
  CanaryRunConfigOutput
newCanaryRunConfigOutput =
  CanaryRunConfigOutput'
    { activeTracing =
        Prelude.Nothing,
      memoryInMB = Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing
    }

-- | Displays whether this canary run used active X-Ray tracing.
canaryRunConfigOutput_activeTracing :: Lens.Lens' CanaryRunConfigOutput (Prelude.Maybe Prelude.Bool)
canaryRunConfigOutput_activeTracing = Lens.lens (\CanaryRunConfigOutput' {activeTracing} -> activeTracing) (\s@CanaryRunConfigOutput' {} a -> s {activeTracing = a} :: CanaryRunConfigOutput)

-- | The maximum amount of memory available to the canary while it is
-- running, in MB. This value must be a multiple of 64.
canaryRunConfigOutput_memoryInMB :: Lens.Lens' CanaryRunConfigOutput (Prelude.Maybe Prelude.Natural)
canaryRunConfigOutput_memoryInMB = Lens.lens (\CanaryRunConfigOutput' {memoryInMB} -> memoryInMB) (\s@CanaryRunConfigOutput' {} a -> s {memoryInMB = a} :: CanaryRunConfigOutput)

-- | How long the canary is allowed to run before it must stop.
canaryRunConfigOutput_timeoutInSeconds :: Lens.Lens' CanaryRunConfigOutput (Prelude.Maybe Prelude.Natural)
canaryRunConfigOutput_timeoutInSeconds = Lens.lens (\CanaryRunConfigOutput' {timeoutInSeconds} -> timeoutInSeconds) (\s@CanaryRunConfigOutput' {} a -> s {timeoutInSeconds = a} :: CanaryRunConfigOutput)

instance Data.FromJSON CanaryRunConfigOutput where
  parseJSON =
    Data.withObject
      "CanaryRunConfigOutput"
      ( \x ->
          CanaryRunConfigOutput'
            Prelude.<$> (x Data..:? "ActiveTracing")
            Prelude.<*> (x Data..:? "MemoryInMB")
            Prelude.<*> (x Data..:? "TimeoutInSeconds")
      )

instance Prelude.Hashable CanaryRunConfigOutput where
  hashWithSalt _salt CanaryRunConfigOutput' {..} =
    _salt
      `Prelude.hashWithSalt` activeTracing
      `Prelude.hashWithSalt` memoryInMB
      `Prelude.hashWithSalt` timeoutInSeconds

instance Prelude.NFData CanaryRunConfigOutput where
  rnf CanaryRunConfigOutput' {..} =
    Prelude.rnf activeTracing
      `Prelude.seq` Prelude.rnf memoryInMB
      `Prelude.seq` Prelude.rnf timeoutInSeconds
