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
-- Module      : Amazonka.Omics.Types.VariantImportItemDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.VariantImportItemDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.JobStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about an imported variant item.
--
-- /See:/ 'newVariantImportItemDetail' smart constructor.
data VariantImportItemDetail = VariantImportItemDetail'
  { -- | The item\'s job status.
    jobStatus :: JobStatus,
    -- | The source file\'s location in Amazon S3.
    source :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VariantImportItemDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'variantImportItemDetail_jobStatus' - The item\'s job status.
--
-- 'source', 'variantImportItemDetail_source' - The source file\'s location in Amazon S3.
newVariantImportItemDetail ::
  -- | 'jobStatus'
  JobStatus ->
  -- | 'source'
  Prelude.Text ->
  VariantImportItemDetail
newVariantImportItemDetail pJobStatus_ pSource_ =
  VariantImportItemDetail'
    { jobStatus = pJobStatus_,
      source = pSource_
    }

-- | The item\'s job status.
variantImportItemDetail_jobStatus :: Lens.Lens' VariantImportItemDetail JobStatus
variantImportItemDetail_jobStatus = Lens.lens (\VariantImportItemDetail' {jobStatus} -> jobStatus) (\s@VariantImportItemDetail' {} a -> s {jobStatus = a} :: VariantImportItemDetail)

-- | The source file\'s location in Amazon S3.
variantImportItemDetail_source :: Lens.Lens' VariantImportItemDetail Prelude.Text
variantImportItemDetail_source = Lens.lens (\VariantImportItemDetail' {source} -> source) (\s@VariantImportItemDetail' {} a -> s {source = a} :: VariantImportItemDetail)

instance Data.FromJSON VariantImportItemDetail where
  parseJSON =
    Data.withObject
      "VariantImportItemDetail"
      ( \x ->
          VariantImportItemDetail'
            Prelude.<$> (x Data..: "jobStatus")
            Prelude.<*> (x Data..: "source")
      )

instance Prelude.Hashable VariantImportItemDetail where
  hashWithSalt _salt VariantImportItemDetail' {..} =
    _salt `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` source

instance Prelude.NFData VariantImportItemDetail where
  rnf VariantImportItemDetail' {..} =
    Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf source
