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
-- Module      : Amazonka.Kendra.StartDataSourceSyncJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a synchronization job for a data source connector. If a
-- synchronization job is already in progress, Amazon Kendra returns a
-- @ResourceInUseException@ exception.
module Amazonka.Kendra.StartDataSourceSyncJob
  ( -- * Creating a Request
    StartDataSourceSyncJob (..),
    newStartDataSourceSyncJob,

    -- * Request Lenses
    startDataSourceSyncJob_id,
    startDataSourceSyncJob_indexId,

    -- * Destructuring the Response
    StartDataSourceSyncJobResponse (..),
    newStartDataSourceSyncJobResponse,

    -- * Response Lenses
    startDataSourceSyncJobResponse_executionId,
    startDataSourceSyncJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartDataSourceSyncJob' smart constructor.
data StartDataSourceSyncJob = StartDataSourceSyncJob'
  { -- | The identifier of the data source connector to synchronize.
    id :: Prelude.Text,
    -- | The identifier of the index used with the data source connector.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDataSourceSyncJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'startDataSourceSyncJob_id' - The identifier of the data source connector to synchronize.
--
-- 'indexId', 'startDataSourceSyncJob_indexId' - The identifier of the index used with the data source connector.
newStartDataSourceSyncJob ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  StartDataSourceSyncJob
newStartDataSourceSyncJob pId_ pIndexId_ =
  StartDataSourceSyncJob'
    { id = pId_,
      indexId = pIndexId_
    }

-- | The identifier of the data source connector to synchronize.
startDataSourceSyncJob_id :: Lens.Lens' StartDataSourceSyncJob Prelude.Text
startDataSourceSyncJob_id = Lens.lens (\StartDataSourceSyncJob' {id} -> id) (\s@StartDataSourceSyncJob' {} a -> s {id = a} :: StartDataSourceSyncJob)

-- | The identifier of the index used with the data source connector.
startDataSourceSyncJob_indexId :: Lens.Lens' StartDataSourceSyncJob Prelude.Text
startDataSourceSyncJob_indexId = Lens.lens (\StartDataSourceSyncJob' {indexId} -> indexId) (\s@StartDataSourceSyncJob' {} a -> s {indexId = a} :: StartDataSourceSyncJob)

instance Core.AWSRequest StartDataSourceSyncJob where
  type
    AWSResponse StartDataSourceSyncJob =
      StartDataSourceSyncJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDataSourceSyncJobResponse'
            Prelude.<$> (x Data..?> "ExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartDataSourceSyncJob where
  hashWithSalt _salt StartDataSourceSyncJob' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData StartDataSourceSyncJob where
  rnf StartDataSourceSyncJob' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders StartDataSourceSyncJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.StartDataSourceSyncJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartDataSourceSyncJob where
  toJSON StartDataSourceSyncJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Data..= id),
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath StartDataSourceSyncJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartDataSourceSyncJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDataSourceSyncJobResponse' smart constructor.
data StartDataSourceSyncJobResponse = StartDataSourceSyncJobResponse'
  { -- | Identifies a particular synchronization job.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDataSourceSyncJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionId', 'startDataSourceSyncJobResponse_executionId' - Identifies a particular synchronization job.
--
-- 'httpStatus', 'startDataSourceSyncJobResponse_httpStatus' - The response's http status code.
newStartDataSourceSyncJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDataSourceSyncJobResponse
newStartDataSourceSyncJobResponse pHttpStatus_ =
  StartDataSourceSyncJobResponse'
    { executionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies a particular synchronization job.
startDataSourceSyncJobResponse_executionId :: Lens.Lens' StartDataSourceSyncJobResponse (Prelude.Maybe Prelude.Text)
startDataSourceSyncJobResponse_executionId = Lens.lens (\StartDataSourceSyncJobResponse' {executionId} -> executionId) (\s@StartDataSourceSyncJobResponse' {} a -> s {executionId = a} :: StartDataSourceSyncJobResponse)

-- | The response's http status code.
startDataSourceSyncJobResponse_httpStatus :: Lens.Lens' StartDataSourceSyncJobResponse Prelude.Int
startDataSourceSyncJobResponse_httpStatus = Lens.lens (\StartDataSourceSyncJobResponse' {httpStatus} -> httpStatus) (\s@StartDataSourceSyncJobResponse' {} a -> s {httpStatus = a} :: StartDataSourceSyncJobResponse)

instance
  Prelude.NFData
    StartDataSourceSyncJobResponse
  where
  rnf StartDataSourceSyncJobResponse' {..} =
    Prelude.rnf executionId `Prelude.seq`
      Prelude.rnf httpStatus
