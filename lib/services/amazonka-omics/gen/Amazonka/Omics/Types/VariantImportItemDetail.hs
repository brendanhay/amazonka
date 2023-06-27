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
  { -- | A message that provides additional context about a job
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The source file\'s location in Amazon S3.
    source :: Prelude.Text,
    -- | The item\'s job status.
    jobStatus :: JobStatus
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
-- 'statusMessage', 'variantImportItemDetail_statusMessage' - A message that provides additional context about a job
--
-- 'source', 'variantImportItemDetail_source' - The source file\'s location in Amazon S3.
--
-- 'jobStatus', 'variantImportItemDetail_jobStatus' - The item\'s job status.
newVariantImportItemDetail ::
  -- | 'source'
  Prelude.Text ->
  -- | 'jobStatus'
  JobStatus ->
  VariantImportItemDetail
newVariantImportItemDetail pSource_ pJobStatus_ =
  VariantImportItemDetail'
    { statusMessage =
        Prelude.Nothing,
      source = pSource_,
      jobStatus = pJobStatus_
    }

-- | A message that provides additional context about a job
variantImportItemDetail_statusMessage :: Lens.Lens' VariantImportItemDetail (Prelude.Maybe Prelude.Text)
variantImportItemDetail_statusMessage = Lens.lens (\VariantImportItemDetail' {statusMessage} -> statusMessage) (\s@VariantImportItemDetail' {} a -> s {statusMessage = a} :: VariantImportItemDetail)

-- | The source file\'s location in Amazon S3.
variantImportItemDetail_source :: Lens.Lens' VariantImportItemDetail Prelude.Text
variantImportItemDetail_source = Lens.lens (\VariantImportItemDetail' {source} -> source) (\s@VariantImportItemDetail' {} a -> s {source = a} :: VariantImportItemDetail)

-- | The item\'s job status.
variantImportItemDetail_jobStatus :: Lens.Lens' VariantImportItemDetail JobStatus
variantImportItemDetail_jobStatus = Lens.lens (\VariantImportItemDetail' {jobStatus} -> jobStatus) (\s@VariantImportItemDetail' {} a -> s {jobStatus = a} :: VariantImportItemDetail)

instance Data.FromJSON VariantImportItemDetail where
  parseJSON =
    Data.withObject
      "VariantImportItemDetail"
      ( \x ->
          VariantImportItemDetail'
            Prelude.<$> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..: "source")
            Prelude.<*> (x Data..: "jobStatus")
      )

instance Prelude.Hashable VariantImportItemDetail where
  hashWithSalt _salt VariantImportItemDetail' {..} =
    _salt
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` jobStatus

instance Prelude.NFData VariantImportItemDetail where
  rnf VariantImportItemDetail' {..} =
    Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf jobStatus
