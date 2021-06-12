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
-- Module      : Network.AWS.SMS.StartOnDemandReplicationRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand replication run for the specified replication job.
-- This replication run starts immediately. This replication run is in
-- addition to the ones already scheduled.
--
-- There is a limit on the number of on-demand replications runs that you
-- can request in a 24-hour period.
module Network.AWS.SMS.StartOnDemandReplicationRun
  ( -- * Creating a Request
    StartOnDemandReplicationRun (..),
    newStartOnDemandReplicationRun,

    -- * Request Lenses
    startOnDemandReplicationRun_description,
    startOnDemandReplicationRun_replicationJobId,

    -- * Destructuring the Response
    StartOnDemandReplicationRunResponse (..),
    newStartOnDemandReplicationRunResponse,

    -- * Response Lenses
    startOnDemandReplicationRunResponse_replicationRunId,
    startOnDemandReplicationRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newStartOnDemandReplicationRun' smart constructor.
data StartOnDemandReplicationRun = StartOnDemandReplicationRun'
  { -- | The description of the replication run.
    description :: Core.Maybe Core.Text,
    -- | The ID of the replication job.
    replicationJobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartOnDemandReplicationRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'startOnDemandReplicationRun_description' - The description of the replication run.
--
-- 'replicationJobId', 'startOnDemandReplicationRun_replicationJobId' - The ID of the replication job.
newStartOnDemandReplicationRun ::
  -- | 'replicationJobId'
  Core.Text ->
  StartOnDemandReplicationRun
newStartOnDemandReplicationRun pReplicationJobId_ =
  StartOnDemandReplicationRun'
    { description =
        Core.Nothing,
      replicationJobId = pReplicationJobId_
    }

-- | The description of the replication run.
startOnDemandReplicationRun_description :: Lens.Lens' StartOnDemandReplicationRun (Core.Maybe Core.Text)
startOnDemandReplicationRun_description = Lens.lens (\StartOnDemandReplicationRun' {description} -> description) (\s@StartOnDemandReplicationRun' {} a -> s {description = a} :: StartOnDemandReplicationRun)

-- | The ID of the replication job.
startOnDemandReplicationRun_replicationJobId :: Lens.Lens' StartOnDemandReplicationRun Core.Text
startOnDemandReplicationRun_replicationJobId = Lens.lens (\StartOnDemandReplicationRun' {replicationJobId} -> replicationJobId) (\s@StartOnDemandReplicationRun' {} a -> s {replicationJobId = a} :: StartOnDemandReplicationRun)

instance Core.AWSRequest StartOnDemandReplicationRun where
  type
    AWSResponse StartOnDemandReplicationRun =
      StartOnDemandReplicationRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartOnDemandReplicationRunResponse'
            Core.<$> (x Core..?> "replicationRunId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartOnDemandReplicationRun

instance Core.NFData StartOnDemandReplicationRun

instance Core.ToHeaders StartOnDemandReplicationRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.StartOnDemandReplicationRun" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartOnDemandReplicationRun where
  toJSON StartOnDemandReplicationRun' {..} =
    Core.object
      ( Core.catMaybes
          [ ("description" Core..=) Core.<$> description,
            Core.Just
              ("replicationJobId" Core..= replicationJobId)
          ]
      )

instance Core.ToPath StartOnDemandReplicationRun where
  toPath = Core.const "/"

instance Core.ToQuery StartOnDemandReplicationRun where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartOnDemandReplicationRunResponse' smart constructor.
data StartOnDemandReplicationRunResponse = StartOnDemandReplicationRunResponse'
  { -- | The ID of the replication run.
    replicationRunId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartOnDemandReplicationRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationRunId', 'startOnDemandReplicationRunResponse_replicationRunId' - The ID of the replication run.
--
-- 'httpStatus', 'startOnDemandReplicationRunResponse_httpStatus' - The response's http status code.
newStartOnDemandReplicationRunResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartOnDemandReplicationRunResponse
newStartOnDemandReplicationRunResponse pHttpStatus_ =
  StartOnDemandReplicationRunResponse'
    { replicationRunId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the replication run.
startOnDemandReplicationRunResponse_replicationRunId :: Lens.Lens' StartOnDemandReplicationRunResponse (Core.Maybe Core.Text)
startOnDemandReplicationRunResponse_replicationRunId = Lens.lens (\StartOnDemandReplicationRunResponse' {replicationRunId} -> replicationRunId) (\s@StartOnDemandReplicationRunResponse' {} a -> s {replicationRunId = a} :: StartOnDemandReplicationRunResponse)

-- | The response's http status code.
startOnDemandReplicationRunResponse_httpStatus :: Lens.Lens' StartOnDemandReplicationRunResponse Core.Int
startOnDemandReplicationRunResponse_httpStatus = Lens.lens (\StartOnDemandReplicationRunResponse' {httpStatus} -> httpStatus) (\s@StartOnDemandReplicationRunResponse' {} a -> s {httpStatus = a} :: StartOnDemandReplicationRunResponse)

instance
  Core.NFData
    StartOnDemandReplicationRunResponse
