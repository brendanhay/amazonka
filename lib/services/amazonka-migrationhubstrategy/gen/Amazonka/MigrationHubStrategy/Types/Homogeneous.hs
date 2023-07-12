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
-- Module      : Amazonka.MigrationHubStrategy.Types.Homogeneous
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.Homogeneous where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.HomogeneousTargetDatabaseEngine
import qualified Amazonka.Prelude as Prelude

-- | The object containing details about homogeneous database preferences.
--
-- /See:/ 'newHomogeneous' smart constructor.
data Homogeneous = Homogeneous'
  { -- | The target database engine for homogeneous database migration
    -- preferences.
    targetDatabaseEngine :: Prelude.Maybe [HomogeneousTargetDatabaseEngine]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Homogeneous' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetDatabaseEngine', 'homogeneous_targetDatabaseEngine' - The target database engine for homogeneous database migration
-- preferences.
newHomogeneous ::
  Homogeneous
newHomogeneous =
  Homogeneous'
    { targetDatabaseEngine =
        Prelude.Nothing
    }

-- | The target database engine for homogeneous database migration
-- preferences.
homogeneous_targetDatabaseEngine :: Lens.Lens' Homogeneous (Prelude.Maybe [HomogeneousTargetDatabaseEngine])
homogeneous_targetDatabaseEngine = Lens.lens (\Homogeneous' {targetDatabaseEngine} -> targetDatabaseEngine) (\s@Homogeneous' {} a -> s {targetDatabaseEngine = a} :: Homogeneous) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Homogeneous where
  parseJSON =
    Data.withObject
      "Homogeneous"
      ( \x ->
          Homogeneous'
            Prelude.<$> ( x
                            Data..:? "targetDatabaseEngine"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Homogeneous where
  hashWithSalt _salt Homogeneous' {..} =
    _salt `Prelude.hashWithSalt` targetDatabaseEngine

instance Prelude.NFData Homogeneous where
  rnf Homogeneous' {..} =
    Prelude.rnf targetDatabaseEngine

instance Data.ToJSON Homogeneous where
  toJSON Homogeneous' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("targetDatabaseEngine" Data..=)
              Prelude.<$> targetDatabaseEngine
          ]
      )
