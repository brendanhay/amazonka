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
-- Module      : Amazonka.Omics.Types.ListVariantImportJobsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ListVariantImportJobsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.JobStatus
import qualified Amazonka.Prelude as Prelude

-- | A filter for variant import jobs.
--
-- /See:/ 'newListVariantImportJobsFilter' smart constructor.
data ListVariantImportJobsFilter = ListVariantImportJobsFilter'
  { -- | A status to filter on.
    status :: Prelude.Maybe JobStatus,
    -- | A store name to filter on.
    storeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVariantImportJobsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listVariantImportJobsFilter_status' - A status to filter on.
--
-- 'storeName', 'listVariantImportJobsFilter_storeName' - A store name to filter on.
newListVariantImportJobsFilter ::
  ListVariantImportJobsFilter
newListVariantImportJobsFilter =
  ListVariantImportJobsFilter'
    { status =
        Prelude.Nothing,
      storeName = Prelude.Nothing
    }

-- | A status to filter on.
listVariantImportJobsFilter_status :: Lens.Lens' ListVariantImportJobsFilter (Prelude.Maybe JobStatus)
listVariantImportJobsFilter_status = Lens.lens (\ListVariantImportJobsFilter' {status} -> status) (\s@ListVariantImportJobsFilter' {} a -> s {status = a} :: ListVariantImportJobsFilter)

-- | A store name to filter on.
listVariantImportJobsFilter_storeName :: Lens.Lens' ListVariantImportJobsFilter (Prelude.Maybe Prelude.Text)
listVariantImportJobsFilter_storeName = Lens.lens (\ListVariantImportJobsFilter' {storeName} -> storeName) (\s@ListVariantImportJobsFilter' {} a -> s {storeName = a} :: ListVariantImportJobsFilter)

instance Prelude.Hashable ListVariantImportJobsFilter where
  hashWithSalt _salt ListVariantImportJobsFilter' {..} =
    _salt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` storeName

instance Prelude.NFData ListVariantImportJobsFilter where
  rnf ListVariantImportJobsFilter' {..} =
    Prelude.rnf status `Prelude.seq`
      Prelude.rnf storeName

instance Data.ToJSON ListVariantImportJobsFilter where
  toJSON ListVariantImportJobsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("status" Data..=) Prelude.<$> status,
            ("storeName" Data..=) Prelude.<$> storeName
          ]
      )
