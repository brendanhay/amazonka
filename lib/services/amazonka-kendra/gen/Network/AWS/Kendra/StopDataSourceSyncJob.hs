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
-- Module      : Network.AWS.Kendra.StopDataSourceSyncJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running synchronization job. You can\'t stop a scheduled
-- synchronization job.
module Network.AWS.Kendra.StopDataSourceSyncJob
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

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopDataSourceSyncJob' smart constructor.
data StopDataSourceSyncJob = StopDataSourceSyncJob'
  { -- | The identifier of the data source for which to stop the synchronization
    -- jobs.
    id :: Prelude.Text,
    -- | The identifier of the index that contains the data source.
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
-- 'id', 'stopDataSourceSyncJob_id' - The identifier of the data source for which to stop the synchronization
-- jobs.
--
-- 'indexId', 'stopDataSourceSyncJob_indexId' - The identifier of the index that contains the data source.
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

-- | The identifier of the data source for which to stop the synchronization
-- jobs.
stopDataSourceSyncJob_id :: Lens.Lens' StopDataSourceSyncJob Prelude.Text
stopDataSourceSyncJob_id = Lens.lens (\StopDataSourceSyncJob' {id} -> id) (\s@StopDataSourceSyncJob' {} a -> s {id = a} :: StopDataSourceSyncJob)

-- | The identifier of the index that contains the data source.
stopDataSourceSyncJob_indexId :: Lens.Lens' StopDataSourceSyncJob Prelude.Text
stopDataSourceSyncJob_indexId = Lens.lens (\StopDataSourceSyncJob' {indexId} -> indexId) (\s@StopDataSourceSyncJob' {} a -> s {indexId = a} :: StopDataSourceSyncJob)

instance Core.AWSRequest StopDataSourceSyncJob where
  type
    AWSResponse StopDataSourceSyncJob =
      StopDataSourceSyncJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull StopDataSourceSyncJobResponse'

instance Prelude.Hashable StopDataSourceSyncJob

instance Prelude.NFData StopDataSourceSyncJob

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

instance Prelude.NFData StopDataSourceSyncJobResponse
