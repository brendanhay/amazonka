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
-- Module      : Amazonka.DMS.CancelReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a single premigration assessment run.
--
-- This operation prevents any individual assessments from running if they
-- haven\'t started running. It also attempts to cancel any individual
-- assessments that are currently running.
module Amazonka.DMS.CancelReplicationTaskAssessmentRun
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelReplicationTaskAssessmentRunResponse'
            Prelude.<$> (x Data..?> "ReplicationTaskAssessmentRun")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CancelReplicationTaskAssessmentRun
  where
  hashWithSalt
    _salt
    CancelReplicationTaskAssessmentRun' {..} =
      _salt
        `Prelude.hashWithSalt` replicationTaskAssessmentRunArn

instance
  Prelude.NFData
    CancelReplicationTaskAssessmentRun
  where
  rnf CancelReplicationTaskAssessmentRun' {..} =
    Prelude.rnf replicationTaskAssessmentRunArn

instance
  Data.ToHeaders
    CancelReplicationTaskAssessmentRun
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.CancelReplicationTaskAssessmentRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    CancelReplicationTaskAssessmentRun
  where
  toJSON CancelReplicationTaskAssessmentRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ReplicationTaskAssessmentRunArn"
                  Data..= replicationTaskAssessmentRunArn
              )
          ]
      )

instance
  Data.ToPath
    CancelReplicationTaskAssessmentRun
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
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
  where
  rnf CancelReplicationTaskAssessmentRunResponse' {..} =
    Prelude.rnf replicationTaskAssessmentRun
      `Prelude.seq` Prelude.rnf httpStatus
