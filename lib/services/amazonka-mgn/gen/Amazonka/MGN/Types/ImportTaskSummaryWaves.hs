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
-- Module      : Amazonka.MGN.Types.ImportTaskSummaryWaves
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ImportTaskSummaryWaves where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Import task summery waves.
--
-- /See:/ 'newImportTaskSummaryWaves' smart constructor.
data ImportTaskSummaryWaves = ImportTaskSummaryWaves'
  { -- | Import task summery waves created count.
    createdCount :: Prelude.Maybe Prelude.Natural,
    -- | Import task summery waves modified count.
    modifiedCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTaskSummaryWaves' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdCount', 'importTaskSummaryWaves_createdCount' - Import task summery waves created count.
--
-- 'modifiedCount', 'importTaskSummaryWaves_modifiedCount' - Import task summery waves modified count.
newImportTaskSummaryWaves ::
  ImportTaskSummaryWaves
newImportTaskSummaryWaves =
  ImportTaskSummaryWaves'
    { createdCount =
        Prelude.Nothing,
      modifiedCount = Prelude.Nothing
    }

-- | Import task summery waves created count.
importTaskSummaryWaves_createdCount :: Lens.Lens' ImportTaskSummaryWaves (Prelude.Maybe Prelude.Natural)
importTaskSummaryWaves_createdCount = Lens.lens (\ImportTaskSummaryWaves' {createdCount} -> createdCount) (\s@ImportTaskSummaryWaves' {} a -> s {createdCount = a} :: ImportTaskSummaryWaves)

-- | Import task summery waves modified count.
importTaskSummaryWaves_modifiedCount :: Lens.Lens' ImportTaskSummaryWaves (Prelude.Maybe Prelude.Natural)
importTaskSummaryWaves_modifiedCount = Lens.lens (\ImportTaskSummaryWaves' {modifiedCount} -> modifiedCount) (\s@ImportTaskSummaryWaves' {} a -> s {modifiedCount = a} :: ImportTaskSummaryWaves)

instance Data.FromJSON ImportTaskSummaryWaves where
  parseJSON =
    Data.withObject
      "ImportTaskSummaryWaves"
      ( \x ->
          ImportTaskSummaryWaves'
            Prelude.<$> (x Data..:? "createdCount")
            Prelude.<*> (x Data..:? "modifiedCount")
      )

instance Prelude.Hashable ImportTaskSummaryWaves where
  hashWithSalt _salt ImportTaskSummaryWaves' {..} =
    _salt
      `Prelude.hashWithSalt` createdCount
      `Prelude.hashWithSalt` modifiedCount

instance Prelude.NFData ImportTaskSummaryWaves where
  rnf ImportTaskSummaryWaves' {..} =
    Prelude.rnf createdCount
      `Prelude.seq` Prelude.rnf modifiedCount
