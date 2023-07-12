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
-- Module      : Amazonka.MigrationHubStrategy.Types.Heterogeneous
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.Heterogeneous where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.HeterogeneousTargetDatabaseEngine
import qualified Amazonka.Prelude as Prelude

-- | The object containing details about heterogeneous database preferences.
--
-- /See:/ 'newHeterogeneous' smart constructor.
data Heterogeneous = Heterogeneous'
  { -- | The target database engine for heterogeneous database migration
    -- preference.
    targetDatabaseEngine :: Prelude.NonEmpty HeterogeneousTargetDatabaseEngine
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Heterogeneous' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetDatabaseEngine', 'heterogeneous_targetDatabaseEngine' - The target database engine for heterogeneous database migration
-- preference.
newHeterogeneous ::
  -- | 'targetDatabaseEngine'
  Prelude.NonEmpty HeterogeneousTargetDatabaseEngine ->
  Heterogeneous
newHeterogeneous pTargetDatabaseEngine_ =
  Heterogeneous'
    { targetDatabaseEngine =
        Lens.coerced Lens.# pTargetDatabaseEngine_
    }

-- | The target database engine for heterogeneous database migration
-- preference.
heterogeneous_targetDatabaseEngine :: Lens.Lens' Heterogeneous (Prelude.NonEmpty HeterogeneousTargetDatabaseEngine)
heterogeneous_targetDatabaseEngine = Lens.lens (\Heterogeneous' {targetDatabaseEngine} -> targetDatabaseEngine) (\s@Heterogeneous' {} a -> s {targetDatabaseEngine = a} :: Heterogeneous) Prelude.. Lens.coerced

instance Data.FromJSON Heterogeneous where
  parseJSON =
    Data.withObject
      "Heterogeneous"
      ( \x ->
          Heterogeneous'
            Prelude.<$> (x Data..: "targetDatabaseEngine")
      )

instance Prelude.Hashable Heterogeneous where
  hashWithSalt _salt Heterogeneous' {..} =
    _salt `Prelude.hashWithSalt` targetDatabaseEngine

instance Prelude.NFData Heterogeneous where
  rnf Heterogeneous' {..} =
    Prelude.rnf targetDatabaseEngine

instance Data.ToJSON Heterogeneous where
  toJSON Heterogeneous' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "targetDatabaseEngine"
                  Data..= targetDatabaseEngine
              )
          ]
      )
