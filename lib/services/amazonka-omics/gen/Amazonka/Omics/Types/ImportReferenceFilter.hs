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
-- Module      : Amazonka.Omics.Types.ImportReferenceFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ImportReferenceFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReferenceImportJobStatus
import qualified Amazonka.Prelude as Prelude

-- | A filter for import references.
--
-- /See:/ 'newImportReferenceFilter' smart constructor.
data ImportReferenceFilter = ImportReferenceFilter'
  { -- | The filter\'s start date.
    createdAfter :: Prelude.Maybe Data.ISO8601,
    -- | The filter\'s end date.
    createdBefore :: Prelude.Maybe Data.ISO8601,
    -- | A status to filter on.
    status :: Prelude.Maybe ReferenceImportJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportReferenceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'importReferenceFilter_createdAfter' - The filter\'s start date.
--
-- 'createdBefore', 'importReferenceFilter_createdBefore' - The filter\'s end date.
--
-- 'status', 'importReferenceFilter_status' - A status to filter on.
newImportReferenceFilter ::
  ImportReferenceFilter
newImportReferenceFilter =
  ImportReferenceFilter'
    { createdAfter =
        Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The filter\'s start date.
importReferenceFilter_createdAfter :: Lens.Lens' ImportReferenceFilter (Prelude.Maybe Prelude.UTCTime)
importReferenceFilter_createdAfter = Lens.lens (\ImportReferenceFilter' {createdAfter} -> createdAfter) (\s@ImportReferenceFilter' {} a -> s {createdAfter = a} :: ImportReferenceFilter) Prelude.. Lens.mapping Data._Time

-- | The filter\'s end date.
importReferenceFilter_createdBefore :: Lens.Lens' ImportReferenceFilter (Prelude.Maybe Prelude.UTCTime)
importReferenceFilter_createdBefore = Lens.lens (\ImportReferenceFilter' {createdBefore} -> createdBefore) (\s@ImportReferenceFilter' {} a -> s {createdBefore = a} :: ImportReferenceFilter) Prelude.. Lens.mapping Data._Time

-- | A status to filter on.
importReferenceFilter_status :: Lens.Lens' ImportReferenceFilter (Prelude.Maybe ReferenceImportJobStatus)
importReferenceFilter_status = Lens.lens (\ImportReferenceFilter' {status} -> status) (\s@ImportReferenceFilter' {} a -> s {status = a} :: ImportReferenceFilter)

instance Prelude.Hashable ImportReferenceFilter where
  hashWithSalt _salt ImportReferenceFilter' {..} =
    _salt
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` status

instance Prelude.NFData ImportReferenceFilter where
  rnf ImportReferenceFilter' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON ImportReferenceFilter where
  toJSON ImportReferenceFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("createdAfter" Data..=) Prelude.<$> createdAfter,
            ("createdBefore" Data..=) Prelude.<$> createdBefore,
            ("status" Data..=) Prelude.<$> status
          ]
      )
