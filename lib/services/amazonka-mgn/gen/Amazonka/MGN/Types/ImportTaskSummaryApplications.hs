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
-- Module      : Amazonka.MGN.Types.ImportTaskSummaryApplications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ImportTaskSummaryApplications where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Import task summary applications.
--
-- /See:/ 'newImportTaskSummaryApplications' smart constructor.
data ImportTaskSummaryApplications = ImportTaskSummaryApplications'
  { -- | Import task summary applications created count.
    createdCount :: Prelude.Maybe Prelude.Natural,
    -- | Import task summary applications modified count.
    modifiedCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTaskSummaryApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdCount', 'importTaskSummaryApplications_createdCount' - Import task summary applications created count.
--
-- 'modifiedCount', 'importTaskSummaryApplications_modifiedCount' - Import task summary applications modified count.
newImportTaskSummaryApplications ::
  ImportTaskSummaryApplications
newImportTaskSummaryApplications =
  ImportTaskSummaryApplications'
    { createdCount =
        Prelude.Nothing,
      modifiedCount = Prelude.Nothing
    }

-- | Import task summary applications created count.
importTaskSummaryApplications_createdCount :: Lens.Lens' ImportTaskSummaryApplications (Prelude.Maybe Prelude.Natural)
importTaskSummaryApplications_createdCount = Lens.lens (\ImportTaskSummaryApplications' {createdCount} -> createdCount) (\s@ImportTaskSummaryApplications' {} a -> s {createdCount = a} :: ImportTaskSummaryApplications)

-- | Import task summary applications modified count.
importTaskSummaryApplications_modifiedCount :: Lens.Lens' ImportTaskSummaryApplications (Prelude.Maybe Prelude.Natural)
importTaskSummaryApplications_modifiedCount = Lens.lens (\ImportTaskSummaryApplications' {modifiedCount} -> modifiedCount) (\s@ImportTaskSummaryApplications' {} a -> s {modifiedCount = a} :: ImportTaskSummaryApplications)

instance Data.FromJSON ImportTaskSummaryApplications where
  parseJSON =
    Data.withObject
      "ImportTaskSummaryApplications"
      ( \x ->
          ImportTaskSummaryApplications'
            Prelude.<$> (x Data..:? "createdCount")
            Prelude.<*> (x Data..:? "modifiedCount")
      )

instance
  Prelude.Hashable
    ImportTaskSummaryApplications
  where
  hashWithSalt _salt ImportTaskSummaryApplications' {..} =
    _salt
      `Prelude.hashWithSalt` createdCount
      `Prelude.hashWithSalt` modifiedCount

instance Prelude.NFData ImportTaskSummaryApplications where
  rnf ImportTaskSummaryApplications' {..} =
    Prelude.rnf createdCount
      `Prelude.seq` Prelude.rnf modifiedCount
