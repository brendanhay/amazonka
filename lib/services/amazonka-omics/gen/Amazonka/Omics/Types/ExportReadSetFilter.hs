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
-- Module      : Amazonka.Omics.Types.ExportReadSetFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ExportReadSetFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReadSetExportJobStatus
import qualified Amazonka.Prelude as Prelude

-- | An read set export job filter.
--
-- /See:/ 'newExportReadSetFilter' smart constructor.
data ExportReadSetFilter = ExportReadSetFilter'
  { -- | The filter\'s start date.
    createdAfter :: Prelude.Maybe Data.ISO8601,
    -- | The filter\'s end date.
    createdBefore :: Prelude.Maybe Data.ISO8601,
    -- | A status to filter on.
    status :: Prelude.Maybe ReadSetExportJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportReadSetFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'exportReadSetFilter_createdAfter' - The filter\'s start date.
--
-- 'createdBefore', 'exportReadSetFilter_createdBefore' - The filter\'s end date.
--
-- 'status', 'exportReadSetFilter_status' - A status to filter on.
newExportReadSetFilter ::
  ExportReadSetFilter
newExportReadSetFilter =
  ExportReadSetFilter'
    { createdAfter =
        Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The filter\'s start date.
exportReadSetFilter_createdAfter :: Lens.Lens' ExportReadSetFilter (Prelude.Maybe Prelude.UTCTime)
exportReadSetFilter_createdAfter = Lens.lens (\ExportReadSetFilter' {createdAfter} -> createdAfter) (\s@ExportReadSetFilter' {} a -> s {createdAfter = a} :: ExportReadSetFilter) Prelude.. Lens.mapping Data._Time

-- | The filter\'s end date.
exportReadSetFilter_createdBefore :: Lens.Lens' ExportReadSetFilter (Prelude.Maybe Prelude.UTCTime)
exportReadSetFilter_createdBefore = Lens.lens (\ExportReadSetFilter' {createdBefore} -> createdBefore) (\s@ExportReadSetFilter' {} a -> s {createdBefore = a} :: ExportReadSetFilter) Prelude.. Lens.mapping Data._Time

-- | A status to filter on.
exportReadSetFilter_status :: Lens.Lens' ExportReadSetFilter (Prelude.Maybe ReadSetExportJobStatus)
exportReadSetFilter_status = Lens.lens (\ExportReadSetFilter' {status} -> status) (\s@ExportReadSetFilter' {} a -> s {status = a} :: ExportReadSetFilter)

instance Prelude.Hashable ExportReadSetFilter where
  hashWithSalt _salt ExportReadSetFilter' {..} =
    _salt
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` status

instance Prelude.NFData ExportReadSetFilter where
  rnf ExportReadSetFilter' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON ExportReadSetFilter where
  toJSON ExportReadSetFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("createdAfter" Data..=) Prelude.<$> createdAfter,
            ("createdBefore" Data..=) Prelude.<$> createdBefore,
            ("status" Data..=) Prelude.<$> status
          ]
      )
