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
-- Module      : Amazonka.DMS.StopReplicationTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the replication task.
module Amazonka.DMS.StopReplicationTask
  ( -- * Creating a Request
    StopReplicationTask (..),
    newStopReplicationTask,

    -- * Request Lenses
    stopReplicationTask_replicationTaskArn,

    -- * Destructuring the Response
    StopReplicationTaskResponse (..),
    newStopReplicationTaskResponse,

    -- * Response Lenses
    stopReplicationTaskResponse_replicationTask,
    stopReplicationTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newStopReplicationTask' smart constructor.
data StopReplicationTask = StopReplicationTask'
  { -- | The Amazon Resource Name(ARN) of the replication task to be stopped.
    replicationTaskArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopReplicationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskArn', 'stopReplicationTask_replicationTaskArn' - The Amazon Resource Name(ARN) of the replication task to be stopped.
newStopReplicationTask ::
  -- | 'replicationTaskArn'
  Prelude.Text ->
  StopReplicationTask
newStopReplicationTask pReplicationTaskArn_ =
  StopReplicationTask'
    { replicationTaskArn =
        pReplicationTaskArn_
    }

-- | The Amazon Resource Name(ARN) of the replication task to be stopped.
stopReplicationTask_replicationTaskArn :: Lens.Lens' StopReplicationTask Prelude.Text
stopReplicationTask_replicationTaskArn = Lens.lens (\StopReplicationTask' {replicationTaskArn} -> replicationTaskArn) (\s@StopReplicationTask' {} a -> s {replicationTaskArn = a} :: StopReplicationTask)

instance Core.AWSRequest StopReplicationTask where
  type
    AWSResponse StopReplicationTask =
      StopReplicationTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopReplicationTaskResponse'
            Prelude.<$> (x Data..?> "ReplicationTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopReplicationTask where
  hashWithSalt _salt StopReplicationTask' {..} =
    _salt `Prelude.hashWithSalt` replicationTaskArn

instance Prelude.NFData StopReplicationTask where
  rnf StopReplicationTask' {..} =
    Prelude.rnf replicationTaskArn

instance Data.ToHeaders StopReplicationTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.StopReplicationTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopReplicationTask where
  toJSON StopReplicationTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ReplicationTaskArn" Data..= replicationTaskArn)
          ]
      )

instance Data.ToPath StopReplicationTask where
  toPath = Prelude.const "/"

instance Data.ToQuery StopReplicationTask where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newStopReplicationTaskResponse' smart constructor.
data StopReplicationTaskResponse = StopReplicationTaskResponse'
  { -- | The replication task stopped.
    replicationTask :: Prelude.Maybe ReplicationTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopReplicationTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTask', 'stopReplicationTaskResponse_replicationTask' - The replication task stopped.
--
-- 'httpStatus', 'stopReplicationTaskResponse_httpStatus' - The response's http status code.
newStopReplicationTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopReplicationTaskResponse
newStopReplicationTaskResponse pHttpStatus_ =
  StopReplicationTaskResponse'
    { replicationTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication task stopped.
stopReplicationTaskResponse_replicationTask :: Lens.Lens' StopReplicationTaskResponse (Prelude.Maybe ReplicationTask)
stopReplicationTaskResponse_replicationTask = Lens.lens (\StopReplicationTaskResponse' {replicationTask} -> replicationTask) (\s@StopReplicationTaskResponse' {} a -> s {replicationTask = a} :: StopReplicationTaskResponse)

-- | The response's http status code.
stopReplicationTaskResponse_httpStatus :: Lens.Lens' StopReplicationTaskResponse Prelude.Int
stopReplicationTaskResponse_httpStatus = Lens.lens (\StopReplicationTaskResponse' {httpStatus} -> httpStatus) (\s@StopReplicationTaskResponse' {} a -> s {httpStatus = a} :: StopReplicationTaskResponse)

instance Prelude.NFData StopReplicationTaskResponse where
  rnf StopReplicationTaskResponse' {..} =
    Prelude.rnf replicationTask
      `Prelude.seq` Prelude.rnf httpStatus
