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
-- Module      : Network.AWS.DMS.StartReplicationTaskAssessment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the replication task assessment for unsupported data types in the
-- source database.
module Network.AWS.DMS.StartReplicationTaskAssessment
  ( -- * Creating a Request
    StartReplicationTaskAssessment (..),
    newStartReplicationTaskAssessment,

    -- * Request Lenses
    startReplicationTaskAssessment_replicationTaskArn,

    -- * Destructuring the Response
    StartReplicationTaskAssessmentResponse (..),
    newStartReplicationTaskAssessmentResponse,

    -- * Response Lenses
    startReplicationTaskAssessmentResponse_replicationTask,
    startReplicationTaskAssessmentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newStartReplicationTaskAssessment' smart constructor.
data StartReplicationTaskAssessment = StartReplicationTaskAssessment'
  { -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReplicationTaskAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskArn', 'startReplicationTaskAssessment_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task.
newStartReplicationTaskAssessment ::
  -- | 'replicationTaskArn'
  Prelude.Text ->
  StartReplicationTaskAssessment
newStartReplicationTaskAssessment
  pReplicationTaskArn_ =
    StartReplicationTaskAssessment'
      { replicationTaskArn =
          pReplicationTaskArn_
      }

-- | The Amazon Resource Name (ARN) of the replication task.
startReplicationTaskAssessment_replicationTaskArn :: Lens.Lens' StartReplicationTaskAssessment Prelude.Text
startReplicationTaskAssessment_replicationTaskArn = Lens.lens (\StartReplicationTaskAssessment' {replicationTaskArn} -> replicationTaskArn) (\s@StartReplicationTaskAssessment' {} a -> s {replicationTaskArn = a} :: StartReplicationTaskAssessment)

instance
  Core.AWSRequest
    StartReplicationTaskAssessment
  where
  type
    AWSResponse StartReplicationTaskAssessment =
      StartReplicationTaskAssessmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReplicationTaskAssessmentResponse'
            Prelude.<$> (x Core..?> "ReplicationTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartReplicationTaskAssessment

instance
  Prelude.NFData
    StartReplicationTaskAssessment

instance
  Core.ToHeaders
    StartReplicationTaskAssessment
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.StartReplicationTaskAssessment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartReplicationTaskAssessment where
  toJSON StartReplicationTaskAssessment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ReplicationTaskArn" Core..= replicationTaskArn)
          ]
      )

instance Core.ToPath StartReplicationTaskAssessment where
  toPath = Prelude.const "/"

instance Core.ToQuery StartReplicationTaskAssessment where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newStartReplicationTaskAssessmentResponse' smart constructor.
data StartReplicationTaskAssessmentResponse = StartReplicationTaskAssessmentResponse'
  { -- | The assessed replication task.
    replicationTask :: Prelude.Maybe ReplicationTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReplicationTaskAssessmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTask', 'startReplicationTaskAssessmentResponse_replicationTask' - The assessed replication task.
--
-- 'httpStatus', 'startReplicationTaskAssessmentResponse_httpStatus' - The response's http status code.
newStartReplicationTaskAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartReplicationTaskAssessmentResponse
newStartReplicationTaskAssessmentResponse
  pHttpStatus_ =
    StartReplicationTaskAssessmentResponse'
      { replicationTask =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The assessed replication task.
startReplicationTaskAssessmentResponse_replicationTask :: Lens.Lens' StartReplicationTaskAssessmentResponse (Prelude.Maybe ReplicationTask)
startReplicationTaskAssessmentResponse_replicationTask = Lens.lens (\StartReplicationTaskAssessmentResponse' {replicationTask} -> replicationTask) (\s@StartReplicationTaskAssessmentResponse' {} a -> s {replicationTask = a} :: StartReplicationTaskAssessmentResponse)

-- | The response's http status code.
startReplicationTaskAssessmentResponse_httpStatus :: Lens.Lens' StartReplicationTaskAssessmentResponse Prelude.Int
startReplicationTaskAssessmentResponse_httpStatus = Lens.lens (\StartReplicationTaskAssessmentResponse' {httpStatus} -> httpStatus) (\s@StartReplicationTaskAssessmentResponse' {} a -> s {httpStatus = a} :: StartReplicationTaskAssessmentResponse)

instance
  Prelude.NFData
    StartReplicationTaskAssessmentResponse
