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
-- Module      : Amazonka.MGN.Types.ImportTaskSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ImportTaskSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.ImportTaskSummaryApplications
import Amazonka.MGN.Types.ImportTaskSummaryServers
import Amazonka.MGN.Types.ImportTaskSummaryWaves
import qualified Amazonka.Prelude as Prelude

-- | Import task summary.
--
-- /See:/ 'newImportTaskSummary' smart constructor.
data ImportTaskSummary = ImportTaskSummary'
  { -- | Import task summary applications.
    applications :: Prelude.Maybe ImportTaskSummaryApplications,
    -- | Import task summary servers.
    servers :: Prelude.Maybe ImportTaskSummaryServers,
    -- | Import task summary waves.
    waves :: Prelude.Maybe ImportTaskSummaryWaves
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTaskSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applications', 'importTaskSummary_applications' - Import task summary applications.
--
-- 'servers', 'importTaskSummary_servers' - Import task summary servers.
--
-- 'waves', 'importTaskSummary_waves' - Import task summary waves.
newImportTaskSummary ::
  ImportTaskSummary
newImportTaskSummary =
  ImportTaskSummary'
    { applications = Prelude.Nothing,
      servers = Prelude.Nothing,
      waves = Prelude.Nothing
    }

-- | Import task summary applications.
importTaskSummary_applications :: Lens.Lens' ImportTaskSummary (Prelude.Maybe ImportTaskSummaryApplications)
importTaskSummary_applications = Lens.lens (\ImportTaskSummary' {applications} -> applications) (\s@ImportTaskSummary' {} a -> s {applications = a} :: ImportTaskSummary)

-- | Import task summary servers.
importTaskSummary_servers :: Lens.Lens' ImportTaskSummary (Prelude.Maybe ImportTaskSummaryServers)
importTaskSummary_servers = Lens.lens (\ImportTaskSummary' {servers} -> servers) (\s@ImportTaskSummary' {} a -> s {servers = a} :: ImportTaskSummary)

-- | Import task summary waves.
importTaskSummary_waves :: Lens.Lens' ImportTaskSummary (Prelude.Maybe ImportTaskSummaryWaves)
importTaskSummary_waves = Lens.lens (\ImportTaskSummary' {waves} -> waves) (\s@ImportTaskSummary' {} a -> s {waves = a} :: ImportTaskSummary)

instance Data.FromJSON ImportTaskSummary where
  parseJSON =
    Data.withObject
      "ImportTaskSummary"
      ( \x ->
          ImportTaskSummary'
            Prelude.<$> (x Data..:? "applications")
            Prelude.<*> (x Data..:? "servers")
            Prelude.<*> (x Data..:? "waves")
      )

instance Prelude.Hashable ImportTaskSummary where
  hashWithSalt _salt ImportTaskSummary' {..} =
    _salt
      `Prelude.hashWithSalt` applications
      `Prelude.hashWithSalt` servers
      `Prelude.hashWithSalt` waves

instance Prelude.NFData ImportTaskSummary where
  rnf ImportTaskSummary' {..} =
    Prelude.rnf applications
      `Prelude.seq` Prelude.rnf servers
      `Prelude.seq` Prelude.rnf waves
