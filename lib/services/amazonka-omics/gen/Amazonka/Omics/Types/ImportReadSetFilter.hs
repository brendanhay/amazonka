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
-- Module      : Amazonka.Omics.Types.ImportReadSetFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ImportReadSetFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReadSetImportJobStatus
import qualified Amazonka.Prelude as Prelude

-- | A filter for import read set jobs.
--
-- /See:/ 'newImportReadSetFilter' smart constructor.
data ImportReadSetFilter = ImportReadSetFilter'
  { -- | The filter\'s start date.
    createdAfter :: Prelude.Maybe Data.ISO8601,
    -- | The filter\'s end date.
    createdBefore :: Prelude.Maybe Data.ISO8601,
    -- | A status to filter on.
    status :: Prelude.Maybe ReadSetImportJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportReadSetFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'importReadSetFilter_createdAfter' - The filter\'s start date.
--
-- 'createdBefore', 'importReadSetFilter_createdBefore' - The filter\'s end date.
--
-- 'status', 'importReadSetFilter_status' - A status to filter on.
newImportReadSetFilter ::
  ImportReadSetFilter
newImportReadSetFilter =
  ImportReadSetFilter'
    { createdAfter =
        Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The filter\'s start date.
importReadSetFilter_createdAfter :: Lens.Lens' ImportReadSetFilter (Prelude.Maybe Prelude.UTCTime)
importReadSetFilter_createdAfter = Lens.lens (\ImportReadSetFilter' {createdAfter} -> createdAfter) (\s@ImportReadSetFilter' {} a -> s {createdAfter = a} :: ImportReadSetFilter) Prelude.. Lens.mapping Data._Time

-- | The filter\'s end date.
importReadSetFilter_createdBefore :: Lens.Lens' ImportReadSetFilter (Prelude.Maybe Prelude.UTCTime)
importReadSetFilter_createdBefore = Lens.lens (\ImportReadSetFilter' {createdBefore} -> createdBefore) (\s@ImportReadSetFilter' {} a -> s {createdBefore = a} :: ImportReadSetFilter) Prelude.. Lens.mapping Data._Time

-- | A status to filter on.
importReadSetFilter_status :: Lens.Lens' ImportReadSetFilter (Prelude.Maybe ReadSetImportJobStatus)
importReadSetFilter_status = Lens.lens (\ImportReadSetFilter' {status} -> status) (\s@ImportReadSetFilter' {} a -> s {status = a} :: ImportReadSetFilter)

instance Prelude.Hashable ImportReadSetFilter where
  hashWithSalt _salt ImportReadSetFilter' {..} =
    _salt
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` status

instance Prelude.NFData ImportReadSetFilter where
  rnf ImportReadSetFilter' {..} =
    Prelude.rnf createdAfter `Prelude.seq`
      Prelude.rnf createdBefore `Prelude.seq`
        Prelude.rnf status

instance Data.ToJSON ImportReadSetFilter where
  toJSON ImportReadSetFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("createdAfter" Data..=) Prelude.<$> createdAfter,
            ("createdBefore" Data..=) Prelude.<$> createdBefore,
            ("status" Data..=) Prelude.<$> status
          ]
      )
