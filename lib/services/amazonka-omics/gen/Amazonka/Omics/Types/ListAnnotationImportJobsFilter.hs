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
-- Module      : Amazonka.Omics.Types.ListAnnotationImportJobsFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ListAnnotationImportJobsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.JobStatus
import qualified Amazonka.Prelude as Prelude

-- | A filter for annotation import jobs.
--
-- /See:/ 'newListAnnotationImportJobsFilter' smart constructor.
data ListAnnotationImportJobsFilter = ListAnnotationImportJobsFilter'
  { -- | A status to filter on.
    status :: Prelude.Maybe JobStatus,
    -- | A store name to filter on.
    storeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnnotationImportJobsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listAnnotationImportJobsFilter_status' - A status to filter on.
--
-- 'storeName', 'listAnnotationImportJobsFilter_storeName' - A store name to filter on.
newListAnnotationImportJobsFilter ::
  ListAnnotationImportJobsFilter
newListAnnotationImportJobsFilter =
  ListAnnotationImportJobsFilter'
    { status =
        Prelude.Nothing,
      storeName = Prelude.Nothing
    }

-- | A status to filter on.
listAnnotationImportJobsFilter_status :: Lens.Lens' ListAnnotationImportJobsFilter (Prelude.Maybe JobStatus)
listAnnotationImportJobsFilter_status = Lens.lens (\ListAnnotationImportJobsFilter' {status} -> status) (\s@ListAnnotationImportJobsFilter' {} a -> s {status = a} :: ListAnnotationImportJobsFilter)

-- | A store name to filter on.
listAnnotationImportJobsFilter_storeName :: Lens.Lens' ListAnnotationImportJobsFilter (Prelude.Maybe Prelude.Text)
listAnnotationImportJobsFilter_storeName = Lens.lens (\ListAnnotationImportJobsFilter' {storeName} -> storeName) (\s@ListAnnotationImportJobsFilter' {} a -> s {storeName = a} :: ListAnnotationImportJobsFilter)

instance
  Prelude.Hashable
    ListAnnotationImportJobsFilter
  where
  hashWithSalt
    _salt
    ListAnnotationImportJobsFilter' {..} =
      _salt `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` storeName

instance
  Prelude.NFData
    ListAnnotationImportJobsFilter
  where
  rnf ListAnnotationImportJobsFilter' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf storeName

instance Data.ToJSON ListAnnotationImportJobsFilter where
  toJSON ListAnnotationImportJobsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("status" Data..=) Prelude.<$> status,
            ("storeName" Data..=) Prelude.<$> storeName
          ]
      )
