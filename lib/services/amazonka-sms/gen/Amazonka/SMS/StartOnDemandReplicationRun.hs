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
-- Module      : Amazonka.SMS.StartOnDemandReplicationRun
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.SMS.StartOnDemandReplicationRun
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newStartOnDemandReplicationRun' smart constructor.
data StartOnDemandReplicationRun = StartOnDemandReplicationRun'
  { -- | The description of the replication run.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the replication job.
    replicationJobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StartOnDemandReplicationRun
newStartOnDemandReplicationRun pReplicationJobId_ =
  StartOnDemandReplicationRun'
    { description =
        Prelude.Nothing,
      replicationJobId = pReplicationJobId_
    }

-- | The description of the replication run.
startOnDemandReplicationRun_description :: Lens.Lens' StartOnDemandReplicationRun (Prelude.Maybe Prelude.Text)
startOnDemandReplicationRun_description = Lens.lens (\StartOnDemandReplicationRun' {description} -> description) (\s@StartOnDemandReplicationRun' {} a -> s {description = a} :: StartOnDemandReplicationRun)

-- | The ID of the replication job.
startOnDemandReplicationRun_replicationJobId :: Lens.Lens' StartOnDemandReplicationRun Prelude.Text
startOnDemandReplicationRun_replicationJobId = Lens.lens (\StartOnDemandReplicationRun' {replicationJobId} -> replicationJobId) (\s@StartOnDemandReplicationRun' {} a -> s {replicationJobId = a} :: StartOnDemandReplicationRun)

instance Core.AWSRequest StartOnDemandReplicationRun where
  type
    AWSResponse StartOnDemandReplicationRun =
      StartOnDemandReplicationRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartOnDemandReplicationRunResponse'
            Prelude.<$> (x Core..?> "replicationRunId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartOnDemandReplicationRun where
  hashWithSalt _salt StartOnDemandReplicationRun' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` replicationJobId

instance Prelude.NFData StartOnDemandReplicationRun where
  rnf StartOnDemandReplicationRun' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf replicationJobId

instance Core.ToHeaders StartOnDemandReplicationRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.StartOnDemandReplicationRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartOnDemandReplicationRun where
  toJSON StartOnDemandReplicationRun' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("description" Core..=) Prelude.<$> description,
            Prelude.Just
              ("replicationJobId" Core..= replicationJobId)
          ]
      )

instance Core.ToPath StartOnDemandReplicationRun where
  toPath = Prelude.const "/"

instance Core.ToQuery StartOnDemandReplicationRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartOnDemandReplicationRunResponse' smart constructor.
data StartOnDemandReplicationRunResponse = StartOnDemandReplicationRunResponse'
  { -- | The ID of the replication run.
    replicationRunId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartOnDemandReplicationRunResponse
newStartOnDemandReplicationRunResponse pHttpStatus_ =
  StartOnDemandReplicationRunResponse'
    { replicationRunId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the replication run.
startOnDemandReplicationRunResponse_replicationRunId :: Lens.Lens' StartOnDemandReplicationRunResponse (Prelude.Maybe Prelude.Text)
startOnDemandReplicationRunResponse_replicationRunId = Lens.lens (\StartOnDemandReplicationRunResponse' {replicationRunId} -> replicationRunId) (\s@StartOnDemandReplicationRunResponse' {} a -> s {replicationRunId = a} :: StartOnDemandReplicationRunResponse)

-- | The response's http status code.
startOnDemandReplicationRunResponse_httpStatus :: Lens.Lens' StartOnDemandReplicationRunResponse Prelude.Int
startOnDemandReplicationRunResponse_httpStatus = Lens.lens (\StartOnDemandReplicationRunResponse' {httpStatus} -> httpStatus) (\s@StartOnDemandReplicationRunResponse' {} a -> s {httpStatus = a} :: StartOnDemandReplicationRunResponse)

instance
  Prelude.NFData
    StartOnDemandReplicationRunResponse
  where
  rnf StartOnDemandReplicationRunResponse' {..} =
    Prelude.rnf replicationRunId
      `Prelude.seq` Prelude.rnf httpStatus
