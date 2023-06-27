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
-- Module      : Amazonka.Omics.Types.AnnotationImportItemDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.AnnotationImportItemDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.JobStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about an imported annotation item.
--
-- /See:/ 'newAnnotationImportItemDetail' smart constructor.
data AnnotationImportItemDetail = AnnotationImportItemDetail'
  { -- | The source file\'s location in Amazon S3.
    source :: Prelude.Text,
    -- | The item\'s job status.
    jobStatus :: JobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnnotationImportItemDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'annotationImportItemDetail_source' - The source file\'s location in Amazon S3.
--
-- 'jobStatus', 'annotationImportItemDetail_jobStatus' - The item\'s job status.
newAnnotationImportItemDetail ::
  -- | 'source'
  Prelude.Text ->
  -- | 'jobStatus'
  JobStatus ->
  AnnotationImportItemDetail
newAnnotationImportItemDetail pSource_ pJobStatus_ =
  AnnotationImportItemDetail'
    { source = pSource_,
      jobStatus = pJobStatus_
    }

-- | The source file\'s location in Amazon S3.
annotationImportItemDetail_source :: Lens.Lens' AnnotationImportItemDetail Prelude.Text
annotationImportItemDetail_source = Lens.lens (\AnnotationImportItemDetail' {source} -> source) (\s@AnnotationImportItemDetail' {} a -> s {source = a} :: AnnotationImportItemDetail)

-- | The item\'s job status.
annotationImportItemDetail_jobStatus :: Lens.Lens' AnnotationImportItemDetail JobStatus
annotationImportItemDetail_jobStatus = Lens.lens (\AnnotationImportItemDetail' {jobStatus} -> jobStatus) (\s@AnnotationImportItemDetail' {} a -> s {jobStatus = a} :: AnnotationImportItemDetail)

instance Data.FromJSON AnnotationImportItemDetail where
  parseJSON =
    Data.withObject
      "AnnotationImportItemDetail"
      ( \x ->
          AnnotationImportItemDetail'
            Prelude.<$> (x Data..: "source")
            Prelude.<*> (x Data..: "jobStatus")
      )

instance Prelude.Hashable AnnotationImportItemDetail where
  hashWithSalt _salt AnnotationImportItemDetail' {..} =
    _salt
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` jobStatus

instance Prelude.NFData AnnotationImportItemDetail where
  rnf AnnotationImportItemDetail' {..} =
    Prelude.rnf source
      `Prelude.seq` Prelude.rnf jobStatus
