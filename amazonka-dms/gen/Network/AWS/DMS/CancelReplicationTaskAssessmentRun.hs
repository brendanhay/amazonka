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
-- Module      : Network.AWS.DMS.CancelReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a single premigration assessment run.
--
-- This operation prevents any individual assessments from running if they
-- haven\'t started running. It also attempts to cancel any individual
-- assessments that are currently running.
module Network.AWS.DMS.CancelReplicationTaskAssessmentRun
  ( -- * Creating a Request
    CancelReplicationTaskAssessmentRun (..),
    newCancelReplicationTaskAssessmentRun,

    -- * Request Lenses
    cancelReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn,

    -- * Destructuring the Response
    CancelReplicationTaskAssessmentRunResponse (..),
    newCancelReplicationTaskAssessmentRunResponse,

    -- * Response Lenses
    cancelReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun,
    cancelReplicationTaskAssessmentRunResponse_httpStatus,
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
-- /See:/ 'newCancelReplicationTaskAssessmentRun' smart constructor.
data CancelReplicationTaskAssessmentRun = CancelReplicationTaskAssessmentRun'
  { -- | Amazon Resource Name (ARN) of the premigration assessment run to be
    -- canceled.
    replicationTaskAssessmentRunArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelReplicationTaskAssessmentRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskAssessmentRunArn', 'cancelReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn' - Amazon Resource Name (ARN) of the premigration assessment run to be
-- canceled.
newCancelReplicationTaskAssessmentRun ::
  -- | 'replicationTaskAssessmentRunArn'
  Prelude.Text ->
  CancelReplicationTaskAssessmentRun
newCancelReplicationTaskAssessmentRun
  pReplicationTaskAssessmentRunArn_ =
    CancelReplicationTaskAssessmentRun'
      { replicationTaskAssessmentRunArn =
          pReplicationTaskAssessmentRunArn_
      }

-- | Amazon Resource Name (ARN) of the premigration assessment run to be
-- canceled.
cancelReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn :: Lens.Lens' CancelReplicationTaskAssessmentRun Prelude.Text
cancelReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn = Lens.lens (\CancelReplicationTaskAssessmentRun' {replicationTaskAssessmentRunArn} -> replicationTaskAssessmentRunArn) (\s@CancelReplicationTaskAssessmentRun' {} a -> s {replicationTaskAssessmentRunArn = a} :: CancelReplicationTaskAssessmentRun)

instance
  Core.AWSRequest
    CancelReplicationTaskAssessmentRun
  where
  type
    AWSResponse CancelReplicationTaskAssessmentRun =
      CancelReplicationTaskAssessmentRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelReplicationTaskAssessmentRunResponse'
            Prelude.<$> (x Core..?> "ReplicationTaskAssessmentRun")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CancelReplicationTaskAssessmentRun

instance
  Prelude.NFData
    CancelReplicationTaskAssessmentRun

instance
  Core.ToHeaders
    CancelReplicationTaskAssessmentRun
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.CancelReplicationTaskAssessmentRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    CancelReplicationTaskAssessmentRun
  where
  toJSON CancelReplicationTaskAssessmentRun' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ReplicationTaskAssessmentRunArn"
                  Core..= replicationTaskAssessmentRunArn
              )
          ]
      )

instance
  Core.ToPath
    CancelReplicationTaskAssessmentRun
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    CancelReplicationTaskAssessmentRun
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newCancelReplicationTaskAssessmentRunResponse' smart constructor.
data CancelReplicationTaskAssessmentRunResponse = CancelReplicationTaskAssessmentRunResponse'
  { -- | The @ReplicationTaskAssessmentRun@ object for the canceled assessment
    -- run.
    replicationTaskAssessmentRun :: Prelude.Maybe ReplicationTaskAssessmentRun,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelReplicationTaskAssessmentRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskAssessmentRun', 'cancelReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun' - The @ReplicationTaskAssessmentRun@ object for the canceled assessment
-- run.
--
-- 'httpStatus', 'cancelReplicationTaskAssessmentRunResponse_httpStatus' - The response's http status code.
newCancelReplicationTaskAssessmentRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelReplicationTaskAssessmentRunResponse
newCancelReplicationTaskAssessmentRunResponse
  pHttpStatus_ =
    CancelReplicationTaskAssessmentRunResponse'
      { replicationTaskAssessmentRun =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @ReplicationTaskAssessmentRun@ object for the canceled assessment
-- run.
cancelReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun :: Lens.Lens' CancelReplicationTaskAssessmentRunResponse (Prelude.Maybe ReplicationTaskAssessmentRun)
cancelReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun = Lens.lens (\CancelReplicationTaskAssessmentRunResponse' {replicationTaskAssessmentRun} -> replicationTaskAssessmentRun) (\s@CancelReplicationTaskAssessmentRunResponse' {} a -> s {replicationTaskAssessmentRun = a} :: CancelReplicationTaskAssessmentRunResponse)

-- | The response's http status code.
cancelReplicationTaskAssessmentRunResponse_httpStatus :: Lens.Lens' CancelReplicationTaskAssessmentRunResponse Prelude.Int
cancelReplicationTaskAssessmentRunResponse_httpStatus = Lens.lens (\CancelReplicationTaskAssessmentRunResponse' {httpStatus} -> httpStatus) (\s@CancelReplicationTaskAssessmentRunResponse' {} a -> s {httpStatus = a} :: CancelReplicationTaskAssessmentRunResponse)

instance
  Prelude.NFData
    CancelReplicationTaskAssessmentRunResponse
