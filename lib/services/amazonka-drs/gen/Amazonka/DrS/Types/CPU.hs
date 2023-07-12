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
-- Module      : Amazonka.DrS.Types.CPU
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.CPU where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a server\'s CPU.
--
-- /See:/ 'newCPU' smart constructor.
data CPU = CPU'
  { -- | The number of CPU cores.
    cores :: Prelude.Maybe Prelude.Natural,
    -- | The model name of the CPU.
    modelName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CPU' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cores', 'cpu_cores' - The number of CPU cores.
--
-- 'modelName', 'cpu_modelName' - The model name of the CPU.
newCPU ::
  CPU
newCPU =
  CPU'
    { cores = Prelude.Nothing,
      modelName = Prelude.Nothing
    }

-- | The number of CPU cores.
cpu_cores :: Lens.Lens' CPU (Prelude.Maybe Prelude.Natural)
cpu_cores = Lens.lens (\CPU' {cores} -> cores) (\s@CPU' {} a -> s {cores = a} :: CPU)

-- | The model name of the CPU.
cpu_modelName :: Lens.Lens' CPU (Prelude.Maybe Prelude.Text)
cpu_modelName = Lens.lens (\CPU' {modelName} -> modelName) (\s@CPU' {} a -> s {modelName = a} :: CPU)

instance Data.FromJSON CPU where
  parseJSON =
    Data.withObject
      "CPU"
      ( \x ->
          CPU'
            Prelude.<$> (x Data..:? "cores")
            Prelude.<*> (x Data..:? "modelName")
      )

instance Prelude.Hashable CPU where
  hashWithSalt _salt CPU' {..} =
    _salt
      `Prelude.hashWithSalt` cores
      `Prelude.hashWithSalt` modelName

instance Prelude.NFData CPU where
  rnf CPU' {..} =
    Prelude.rnf cores
      `Prelude.seq` Prelude.rnf modelName
