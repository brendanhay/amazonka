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
-- Module      : Amazonka.MGN.Types.ImportTaskSummaryServers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ImportTaskSummaryServers where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Import task summary servers.
--
-- /See:/ 'newImportTaskSummaryServers' smart constructor.
data ImportTaskSummaryServers = ImportTaskSummaryServers'
  { -- | Import task summary servers created count.
    createdCount :: Prelude.Maybe Prelude.Natural,
    -- | Import task summary servers modified count.
    modifiedCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTaskSummaryServers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdCount', 'importTaskSummaryServers_createdCount' - Import task summary servers created count.
--
-- 'modifiedCount', 'importTaskSummaryServers_modifiedCount' - Import task summary servers modified count.
newImportTaskSummaryServers ::
  ImportTaskSummaryServers
newImportTaskSummaryServers =
  ImportTaskSummaryServers'
    { createdCount =
        Prelude.Nothing,
      modifiedCount = Prelude.Nothing
    }

-- | Import task summary servers created count.
importTaskSummaryServers_createdCount :: Lens.Lens' ImportTaskSummaryServers (Prelude.Maybe Prelude.Natural)
importTaskSummaryServers_createdCount = Lens.lens (\ImportTaskSummaryServers' {createdCount} -> createdCount) (\s@ImportTaskSummaryServers' {} a -> s {createdCount = a} :: ImportTaskSummaryServers)

-- | Import task summary servers modified count.
importTaskSummaryServers_modifiedCount :: Lens.Lens' ImportTaskSummaryServers (Prelude.Maybe Prelude.Natural)
importTaskSummaryServers_modifiedCount = Lens.lens (\ImportTaskSummaryServers' {modifiedCount} -> modifiedCount) (\s@ImportTaskSummaryServers' {} a -> s {modifiedCount = a} :: ImportTaskSummaryServers)

instance Data.FromJSON ImportTaskSummaryServers where
  parseJSON =
    Data.withObject
      "ImportTaskSummaryServers"
      ( \x ->
          ImportTaskSummaryServers'
            Prelude.<$> (x Data..:? "createdCount")
            Prelude.<*> (x Data..:? "modifiedCount")
      )

instance Prelude.Hashable ImportTaskSummaryServers where
  hashWithSalt _salt ImportTaskSummaryServers' {..} =
    _salt
      `Prelude.hashWithSalt` createdCount
      `Prelude.hashWithSalt` modifiedCount

instance Prelude.NFData ImportTaskSummaryServers where
  rnf ImportTaskSummaryServers' {..} =
    Prelude.rnf createdCount
      `Prelude.seq` Prelude.rnf modifiedCount
