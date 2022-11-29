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
-- Module      : Amazonka.Kendra.StopDataSourceSyncJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a synchronization job that is currently running. You can\'t stop a
-- scheduled synchronization job.
module Amazonka.Kendra.StopDataSourceSyncJob
  ( -- * Creating a Request
    StopDataSourceSyncJob (..),
    newStopDataSourceSyncJob,

    -- * Request Lenses
    stopDataSourceSyncJob_id,
    stopDataSourceSyncJob_indexId,

    -- * Destructuring the Response
    StopDataSourceSyncJobResponse (..),
    newStopDataSourceSyncJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopDataSourceSyncJob' smart constructor.
data StopDataSourceSyncJob = StopDataSourceSyncJob'
  { -- | The identifier of the data source connector for which to stop the
    -- synchronization jobs.
    id :: Prelude.Text,
    -- | The identifier of the index used with the data source connector.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDataSourceSyncJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'stopDataSourceSyncJob_id' - The identifier of the data source connector for which to stop the
-- synchronization jobs.
--
-- 'indexId', 'stopDataSourceSyncJob_indexId' - The identifier of the index used with the data source connector.
newStopDataSourceSyncJob ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  StopDataSourceSyncJob
newStopDataSourceSyncJob pId_ pIndexId_ =
  StopDataSourceSyncJob'
    { id = pId_,
      indexId = pIndexId_
    }

-- | The identifier of the data source connector for which to stop the
-- synchronization jobs.
stopDataSourceSyncJob_id :: Lens.Lens' StopDataSourceSyncJob Prelude.Text
stopDataSourceSyncJob_id = Lens.lens (\StopDataSourceSyncJob' {id} -> id) (\s@StopDataSourceSyncJob' {} a -> s {id = a} :: StopDataSourceSyncJob)

-- | The identifier of the index used with the data source connector.
stopDataSourceSyncJob_indexId :: Lens.Lens' StopDataSourceSyncJob Prelude.Text
stopDataSourceSyncJob_indexId = Lens.lens (\StopDataSourceSyncJob' {indexId} -> indexId) (\s@StopDataSourceSyncJob' {} a -> s {indexId = a} :: StopDataSourceSyncJob)

instance Core.AWSRequest StopDataSourceSyncJob where
  type
    AWSResponse StopDataSourceSyncJob =
      StopDataSourceSyncJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StopDataSourceSyncJobResponse'

instance Prelude.Hashable StopDataSourceSyncJob where
  hashWithSalt _salt StopDataSourceSyncJob' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData StopDataSourceSyncJob where
  rnf StopDataSourceSyncJob' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf indexId

instance Core.ToHeaders StopDataSourceSyncJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.StopDataSourceSyncJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopDataSourceSyncJob where
  toJSON StopDataSourceSyncJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Core..= id),
            Prelude.Just ("IndexId" Core..= indexId)
          ]
      )

instance Core.ToPath StopDataSourceSyncJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StopDataSourceSyncJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopDataSourceSyncJobResponse' smart constructor.
data StopDataSourceSyncJobResponse = StopDataSourceSyncJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDataSourceSyncJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopDataSourceSyncJobResponse ::
  StopDataSourceSyncJobResponse
newStopDataSourceSyncJobResponse =
  StopDataSourceSyncJobResponse'

instance Prelude.NFData StopDataSourceSyncJobResponse where
  rnf _ = ()
