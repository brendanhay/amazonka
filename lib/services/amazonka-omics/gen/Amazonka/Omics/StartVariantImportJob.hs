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
-- Module      : Amazonka.Omics.StartVariantImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a variant import job.
module Amazonka.Omics.StartVariantImportJob
  ( -- * Creating a Request
    StartVariantImportJob (..),
    newStartVariantImportJob,

    -- * Request Lenses
    startVariantImportJob_runLeftNormalization,
    startVariantImportJob_destinationName,
    startVariantImportJob_items,
    startVariantImportJob_roleArn,

    -- * Destructuring the Response
    StartVariantImportJobResponse (..),
    newStartVariantImportJobResponse,

    -- * Response Lenses
    startVariantImportJobResponse_httpStatus,
    startVariantImportJobResponse_jobId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartVariantImportJob' smart constructor.
data StartVariantImportJob = StartVariantImportJob'
  { -- | The job\'s left normalization setting.
    runLeftNormalization :: Prelude.Maybe Prelude.Bool,
    -- | The destination variant store for the job.
    destinationName :: Prelude.Text,
    -- | Items to import.
    items :: Prelude.NonEmpty VariantImportItemSource,
    -- | A service role for the job.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartVariantImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runLeftNormalization', 'startVariantImportJob_runLeftNormalization' - The job\'s left normalization setting.
--
-- 'destinationName', 'startVariantImportJob_destinationName' - The destination variant store for the job.
--
-- 'items', 'startVariantImportJob_items' - Items to import.
--
-- 'roleArn', 'startVariantImportJob_roleArn' - A service role for the job.
newStartVariantImportJob ::
  -- | 'destinationName'
  Prelude.Text ->
  -- | 'items'
  Prelude.NonEmpty VariantImportItemSource ->
  -- | 'roleArn'
  Prelude.Text ->
  StartVariantImportJob
newStartVariantImportJob
  pDestinationName_
  pItems_
  pRoleArn_ =
    StartVariantImportJob'
      { runLeftNormalization =
          Prelude.Nothing,
        destinationName = pDestinationName_,
        items = Lens.coerced Lens.# pItems_,
        roleArn = pRoleArn_
      }

-- | The job\'s left normalization setting.
startVariantImportJob_runLeftNormalization :: Lens.Lens' StartVariantImportJob (Prelude.Maybe Prelude.Bool)
startVariantImportJob_runLeftNormalization = Lens.lens (\StartVariantImportJob' {runLeftNormalization} -> runLeftNormalization) (\s@StartVariantImportJob' {} a -> s {runLeftNormalization = a} :: StartVariantImportJob)

-- | The destination variant store for the job.
startVariantImportJob_destinationName :: Lens.Lens' StartVariantImportJob Prelude.Text
startVariantImportJob_destinationName = Lens.lens (\StartVariantImportJob' {destinationName} -> destinationName) (\s@StartVariantImportJob' {} a -> s {destinationName = a} :: StartVariantImportJob)

-- | Items to import.
startVariantImportJob_items :: Lens.Lens' StartVariantImportJob (Prelude.NonEmpty VariantImportItemSource)
startVariantImportJob_items = Lens.lens (\StartVariantImportJob' {items} -> items) (\s@StartVariantImportJob' {} a -> s {items = a} :: StartVariantImportJob) Prelude.. Lens.coerced

-- | A service role for the job.
startVariantImportJob_roleArn :: Lens.Lens' StartVariantImportJob Prelude.Text
startVariantImportJob_roleArn = Lens.lens (\StartVariantImportJob' {roleArn} -> roleArn) (\s@StartVariantImportJob' {} a -> s {roleArn = a} :: StartVariantImportJob)

instance Core.AWSRequest StartVariantImportJob where
  type
    AWSResponse StartVariantImportJob =
      StartVariantImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartVariantImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "jobId")
      )

instance Prelude.Hashable StartVariantImportJob where
  hashWithSalt _salt StartVariantImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` runLeftNormalization
      `Prelude.hashWithSalt` destinationName
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData StartVariantImportJob where
  rnf StartVariantImportJob' {..} =
    Prelude.rnf runLeftNormalization `Prelude.seq`
      Prelude.rnf destinationName `Prelude.seq`
        Prelude.rnf items `Prelude.seq`
          Prelude.rnf roleArn

instance Data.ToHeaders StartVariantImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartVariantImportJob where
  toJSON StartVariantImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("runLeftNormalization" Data..=)
              Prelude.<$> runLeftNormalization,
            Prelude.Just
              ("destinationName" Data..= destinationName),
            Prelude.Just ("items" Data..= items),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath StartVariantImportJob where
  toPath = Prelude.const "/import/variant"

instance Data.ToQuery StartVariantImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartVariantImportJobResponse' smart constructor.
data StartVariantImportJobResponse = StartVariantImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The job\'s ID.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartVariantImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startVariantImportJobResponse_httpStatus' - The response's http status code.
--
-- 'jobId', 'startVariantImportJobResponse_jobId' - The job\'s ID.
newStartVariantImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobId'
  Prelude.Text ->
  StartVariantImportJobResponse
newStartVariantImportJobResponse pHttpStatus_ pJobId_ =
  StartVariantImportJobResponse'
    { httpStatus =
        pHttpStatus_,
      jobId = pJobId_
    }

-- | The response's http status code.
startVariantImportJobResponse_httpStatus :: Lens.Lens' StartVariantImportJobResponse Prelude.Int
startVariantImportJobResponse_httpStatus = Lens.lens (\StartVariantImportJobResponse' {httpStatus} -> httpStatus) (\s@StartVariantImportJobResponse' {} a -> s {httpStatus = a} :: StartVariantImportJobResponse)

-- | The job\'s ID.
startVariantImportJobResponse_jobId :: Lens.Lens' StartVariantImportJobResponse Prelude.Text
startVariantImportJobResponse_jobId = Lens.lens (\StartVariantImportJobResponse' {jobId} -> jobId) (\s@StartVariantImportJobResponse' {} a -> s {jobId = a} :: StartVariantImportJobResponse)

instance Prelude.NFData StartVariantImportJobResponse where
  rnf StartVariantImportJobResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf jobId
