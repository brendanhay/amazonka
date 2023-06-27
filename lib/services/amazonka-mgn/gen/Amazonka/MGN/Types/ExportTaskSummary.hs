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
-- Module      : Amazonka.MGN.Types.ExportTaskSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ExportTaskSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Export task summary.
--
-- /See:/ 'newExportTaskSummary' smart constructor.
data ExportTaskSummary = ExportTaskSummary'
  { -- | Export task summary applications count.
    applicationsCount :: Prelude.Maybe Prelude.Natural,
    -- | Export task summary servers count.
    serversCount :: Prelude.Maybe Prelude.Natural,
    -- | Export task summary waves count.
    wavesCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportTaskSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationsCount', 'exportTaskSummary_applicationsCount' - Export task summary applications count.
--
-- 'serversCount', 'exportTaskSummary_serversCount' - Export task summary servers count.
--
-- 'wavesCount', 'exportTaskSummary_wavesCount' - Export task summary waves count.
newExportTaskSummary ::
  ExportTaskSummary
newExportTaskSummary =
  ExportTaskSummary'
    { applicationsCount =
        Prelude.Nothing,
      serversCount = Prelude.Nothing,
      wavesCount = Prelude.Nothing
    }

-- | Export task summary applications count.
exportTaskSummary_applicationsCount :: Lens.Lens' ExportTaskSummary (Prelude.Maybe Prelude.Natural)
exportTaskSummary_applicationsCount = Lens.lens (\ExportTaskSummary' {applicationsCount} -> applicationsCount) (\s@ExportTaskSummary' {} a -> s {applicationsCount = a} :: ExportTaskSummary)

-- | Export task summary servers count.
exportTaskSummary_serversCount :: Lens.Lens' ExportTaskSummary (Prelude.Maybe Prelude.Natural)
exportTaskSummary_serversCount = Lens.lens (\ExportTaskSummary' {serversCount} -> serversCount) (\s@ExportTaskSummary' {} a -> s {serversCount = a} :: ExportTaskSummary)

-- | Export task summary waves count.
exportTaskSummary_wavesCount :: Lens.Lens' ExportTaskSummary (Prelude.Maybe Prelude.Natural)
exportTaskSummary_wavesCount = Lens.lens (\ExportTaskSummary' {wavesCount} -> wavesCount) (\s@ExportTaskSummary' {} a -> s {wavesCount = a} :: ExportTaskSummary)

instance Data.FromJSON ExportTaskSummary where
  parseJSON =
    Data.withObject
      "ExportTaskSummary"
      ( \x ->
          ExportTaskSummary'
            Prelude.<$> (x Data..:? "applicationsCount")
            Prelude.<*> (x Data..:? "serversCount")
            Prelude.<*> (x Data..:? "wavesCount")
      )

instance Prelude.Hashable ExportTaskSummary where
  hashWithSalt _salt ExportTaskSummary' {..} =
    _salt
      `Prelude.hashWithSalt` applicationsCount
      `Prelude.hashWithSalt` serversCount
      `Prelude.hashWithSalt` wavesCount

instance Prelude.NFData ExportTaskSummary where
  rnf ExportTaskSummary' {..} =
    Prelude.rnf applicationsCount
      `Prelude.seq` Prelude.rnf serversCount
      `Prelude.seq` Prelude.rnf wavesCount
