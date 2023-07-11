{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Omics.StartAnnotationImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an annotation import job.
module Amazonka.Omics.StartAnnotationImportJob
  ( -- * Creating a Request
    StartAnnotationImportJob (..),
    newStartAnnotationImportJob,

    -- * Request Lenses
    startAnnotationImportJob_formatOptions,
    startAnnotationImportJob_runLeftNormalization,
    startAnnotationImportJob_destinationName,
    startAnnotationImportJob_items,
    startAnnotationImportJob_roleArn,

    -- * Destructuring the Response
    StartAnnotationImportJobResponse (..),
    newStartAnnotationImportJobResponse,

    -- * Response Lenses
    startAnnotationImportJobResponse_httpStatus,
    startAnnotationImportJobResponse_jobId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartAnnotationImportJob' smart constructor.
data StartAnnotationImportJob = StartAnnotationImportJob'
  { -- | Formatting options for the annotation file.
    formatOptions :: Prelude.Maybe FormatOptions,
    -- | The job\'s left normalization setting.
    runLeftNormalization :: Prelude.Maybe Prelude.Bool,
    -- | A destination annotation store for the job.
    destinationName :: Prelude.Text,
    -- | Items to import.
    items :: Prelude.NonEmpty AnnotationImportItemSource,
    -- | A service role for the job.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAnnotationImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatOptions', 'startAnnotationImportJob_formatOptions' - Formatting options for the annotation file.
--
-- 'runLeftNormalization', 'startAnnotationImportJob_runLeftNormalization' - The job\'s left normalization setting.
--
-- 'destinationName', 'startAnnotationImportJob_destinationName' - A destination annotation store for the job.
--
-- 'items', 'startAnnotationImportJob_items' - Items to import.
--
-- 'roleArn', 'startAnnotationImportJob_roleArn' - A service role for the job.
newStartAnnotationImportJob ::
  -- | 'destinationName'
  Prelude.Text ->
  -- | 'items'
  Prelude.NonEmpty AnnotationImportItemSource ->
  -- | 'roleArn'
  Prelude.Text ->
  StartAnnotationImportJob
newStartAnnotationImportJob
  pDestinationName_
  pItems_
  pRoleArn_ =
    StartAnnotationImportJob'
      { formatOptions =
          Prelude.Nothing,
        runLeftNormalization = Prelude.Nothing,
        destinationName = pDestinationName_,
        items = Lens.coerced Lens.# pItems_,
        roleArn = pRoleArn_
      }

-- | Formatting options for the annotation file.
startAnnotationImportJob_formatOptions :: Lens.Lens' StartAnnotationImportJob (Prelude.Maybe FormatOptions)
startAnnotationImportJob_formatOptions = Lens.lens (\StartAnnotationImportJob' {formatOptions} -> formatOptions) (\s@StartAnnotationImportJob' {} a -> s {formatOptions = a} :: StartAnnotationImportJob)

-- | The job\'s left normalization setting.
startAnnotationImportJob_runLeftNormalization :: Lens.Lens' StartAnnotationImportJob (Prelude.Maybe Prelude.Bool)
startAnnotationImportJob_runLeftNormalization = Lens.lens (\StartAnnotationImportJob' {runLeftNormalization} -> runLeftNormalization) (\s@StartAnnotationImportJob' {} a -> s {runLeftNormalization = a} :: StartAnnotationImportJob)

-- | A destination annotation store for the job.
startAnnotationImportJob_destinationName :: Lens.Lens' StartAnnotationImportJob Prelude.Text
startAnnotationImportJob_destinationName = Lens.lens (\StartAnnotationImportJob' {destinationName} -> destinationName) (\s@StartAnnotationImportJob' {} a -> s {destinationName = a} :: StartAnnotationImportJob)

-- | Items to import.
startAnnotationImportJob_items :: Lens.Lens' StartAnnotationImportJob (Prelude.NonEmpty AnnotationImportItemSource)
startAnnotationImportJob_items = Lens.lens (\StartAnnotationImportJob' {items} -> items) (\s@StartAnnotationImportJob' {} a -> s {items = a} :: StartAnnotationImportJob) Prelude.. Lens.coerced

-- | A service role for the job.
startAnnotationImportJob_roleArn :: Lens.Lens' StartAnnotationImportJob Prelude.Text
startAnnotationImportJob_roleArn = Lens.lens (\StartAnnotationImportJob' {roleArn} -> roleArn) (\s@StartAnnotationImportJob' {} a -> s {roleArn = a} :: StartAnnotationImportJob)

instance Core.AWSRequest StartAnnotationImportJob where
  type
    AWSResponse StartAnnotationImportJob =
      StartAnnotationImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAnnotationImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "jobId")
      )

instance Prelude.Hashable StartAnnotationImportJob where
  hashWithSalt _salt StartAnnotationImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` formatOptions
      `Prelude.hashWithSalt` runLeftNormalization
      `Prelude.hashWithSalt` destinationName
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData StartAnnotationImportJob where
  rnf StartAnnotationImportJob' {..} =
    Prelude.rnf formatOptions
      `Prelude.seq` Prelude.rnf runLeftNormalization
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf items
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders StartAnnotationImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartAnnotationImportJob where
  toJSON StartAnnotationImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("formatOptions" Data..=) Prelude.<$> formatOptions,
            ("runLeftNormalization" Data..=)
              Prelude.<$> runLeftNormalization,
            Prelude.Just
              ("destinationName" Data..= destinationName),
            Prelude.Just ("items" Data..= items),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath StartAnnotationImportJob where
  toPath = Prelude.const "/import/annotation"

instance Data.ToQuery StartAnnotationImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAnnotationImportJobResponse' smart constructor.
data StartAnnotationImportJobResponse = StartAnnotationImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The job\'s ID.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAnnotationImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startAnnotationImportJobResponse_httpStatus' - The response's http status code.
--
-- 'jobId', 'startAnnotationImportJobResponse_jobId' - The job\'s ID.
newStartAnnotationImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobId'
  Prelude.Text ->
  StartAnnotationImportJobResponse
newStartAnnotationImportJobResponse
  pHttpStatus_
  pJobId_ =
    StartAnnotationImportJobResponse'
      { httpStatus =
          pHttpStatus_,
        jobId = pJobId_
      }

-- | The response's http status code.
startAnnotationImportJobResponse_httpStatus :: Lens.Lens' StartAnnotationImportJobResponse Prelude.Int
startAnnotationImportJobResponse_httpStatus = Lens.lens (\StartAnnotationImportJobResponse' {httpStatus} -> httpStatus) (\s@StartAnnotationImportJobResponse' {} a -> s {httpStatus = a} :: StartAnnotationImportJobResponse)

-- | The job\'s ID.
startAnnotationImportJobResponse_jobId :: Lens.Lens' StartAnnotationImportJobResponse Prelude.Text
startAnnotationImportJobResponse_jobId = Lens.lens (\StartAnnotationImportJobResponse' {jobId} -> jobId) (\s@StartAnnotationImportJobResponse' {} a -> s {jobId = a} :: StartAnnotationImportJobResponse)

instance
  Prelude.NFData
    StartAnnotationImportJobResponse
  where
  rnf StartAnnotationImportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobId
