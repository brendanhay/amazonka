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
-- Module      : Amazonka.EMRServerless.Types.InitialCapacityConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.InitialCapacityConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRServerless.Types.WorkerResourceConfig
import qualified Amazonka.Prelude as Prelude

-- | The initial capacity configuration per worker.
--
-- /See:/ 'newInitialCapacityConfig' smart constructor.
data InitialCapacityConfig = InitialCapacityConfig'
  { -- | The resource configuration of the initial capacity configuration.
    workerConfiguration :: Prelude.Maybe WorkerResourceConfig,
    -- | The number of workers in the initial capacity configuration.
    workerCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitialCapacityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workerConfiguration', 'initialCapacityConfig_workerConfiguration' - The resource configuration of the initial capacity configuration.
--
-- 'workerCount', 'initialCapacityConfig_workerCount' - The number of workers in the initial capacity configuration.
newInitialCapacityConfig ::
  -- | 'workerCount'
  Prelude.Natural ->
  InitialCapacityConfig
newInitialCapacityConfig pWorkerCount_ =
  InitialCapacityConfig'
    { workerConfiguration =
        Prelude.Nothing,
      workerCount = pWorkerCount_
    }

-- | The resource configuration of the initial capacity configuration.
initialCapacityConfig_workerConfiguration :: Lens.Lens' InitialCapacityConfig (Prelude.Maybe WorkerResourceConfig)
initialCapacityConfig_workerConfiguration = Lens.lens (\InitialCapacityConfig' {workerConfiguration} -> workerConfiguration) (\s@InitialCapacityConfig' {} a -> s {workerConfiguration = a} :: InitialCapacityConfig)

-- | The number of workers in the initial capacity configuration.
initialCapacityConfig_workerCount :: Lens.Lens' InitialCapacityConfig Prelude.Natural
initialCapacityConfig_workerCount = Lens.lens (\InitialCapacityConfig' {workerCount} -> workerCount) (\s@InitialCapacityConfig' {} a -> s {workerCount = a} :: InitialCapacityConfig)

instance Data.FromJSON InitialCapacityConfig where
  parseJSON =
    Data.withObject
      "InitialCapacityConfig"
      ( \x ->
          InitialCapacityConfig'
            Prelude.<$> (x Data..:? "workerConfiguration")
            Prelude.<*> (x Data..: "workerCount")
      )

instance Prelude.Hashable InitialCapacityConfig where
  hashWithSalt _salt InitialCapacityConfig' {..} =
    _salt
      `Prelude.hashWithSalt` workerConfiguration
      `Prelude.hashWithSalt` workerCount

instance Prelude.NFData InitialCapacityConfig where
  rnf InitialCapacityConfig' {..} =
    Prelude.rnf workerConfiguration
      `Prelude.seq` Prelude.rnf workerCount

instance Data.ToJSON InitialCapacityConfig where
  toJSON InitialCapacityConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("workerConfiguration" Data..=)
              Prelude.<$> workerConfiguration,
            Prelude.Just ("workerCount" Data..= workerCount)
          ]
      )
