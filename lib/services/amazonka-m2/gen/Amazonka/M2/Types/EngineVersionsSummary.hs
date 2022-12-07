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
-- Module      : Amazonka.M2.Types.EngineVersionsSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.EngineVersionsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A subset of information about the engine version for a specific
-- application.
--
-- /See:/ 'newEngineVersionsSummary' smart constructor.
data EngineVersionsSummary = EngineVersionsSummary'
  { -- | The type of target platform for the application.
    engineType :: Prelude.Text,
    -- | The version of the engine type used by the application.
    engineVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EngineVersionsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineType', 'engineVersionsSummary_engineType' - The type of target platform for the application.
--
-- 'engineVersion', 'engineVersionsSummary_engineVersion' - The version of the engine type used by the application.
newEngineVersionsSummary ::
  -- | 'engineType'
  Prelude.Text ->
  -- | 'engineVersion'
  Prelude.Text ->
  EngineVersionsSummary
newEngineVersionsSummary pEngineType_ pEngineVersion_ =
  EngineVersionsSummary'
    { engineType = pEngineType_,
      engineVersion = pEngineVersion_
    }

-- | The type of target platform for the application.
engineVersionsSummary_engineType :: Lens.Lens' EngineVersionsSummary Prelude.Text
engineVersionsSummary_engineType = Lens.lens (\EngineVersionsSummary' {engineType} -> engineType) (\s@EngineVersionsSummary' {} a -> s {engineType = a} :: EngineVersionsSummary)

-- | The version of the engine type used by the application.
engineVersionsSummary_engineVersion :: Lens.Lens' EngineVersionsSummary Prelude.Text
engineVersionsSummary_engineVersion = Lens.lens (\EngineVersionsSummary' {engineVersion} -> engineVersion) (\s@EngineVersionsSummary' {} a -> s {engineVersion = a} :: EngineVersionsSummary)

instance Data.FromJSON EngineVersionsSummary where
  parseJSON =
    Data.withObject
      "EngineVersionsSummary"
      ( \x ->
          EngineVersionsSummary'
            Prelude.<$> (x Data..: "engineType")
            Prelude.<*> (x Data..: "engineVersion")
      )

instance Prelude.Hashable EngineVersionsSummary where
  hashWithSalt _salt EngineVersionsSummary' {..} =
    _salt `Prelude.hashWithSalt` engineType
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData EngineVersionsSummary where
  rnf EngineVersionsSummary' {..} =
    Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf engineVersion
