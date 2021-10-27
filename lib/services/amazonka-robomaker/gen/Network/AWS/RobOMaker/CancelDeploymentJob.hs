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
-- Module      : Network.AWS.RobOMaker.CancelDeploymentJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified deployment job.
module Network.AWS.RobOMaker.CancelDeploymentJob
  ( -- * Creating a Request
    CancelDeploymentJob (..),
    newCancelDeploymentJob,

    -- * Request Lenses
    cancelDeploymentJob_job,

    -- * Destructuring the Response
    CancelDeploymentJobResponse (..),
    newCancelDeploymentJobResponse,

    -- * Response Lenses
    cancelDeploymentJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.RobOMaker.Types

-- | /See:/ 'newCancelDeploymentJob' smart constructor.
data CancelDeploymentJob = CancelDeploymentJob'
  { -- | The deployment job ARN to cancel.
    job :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelDeploymentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'cancelDeploymentJob_job' - The deployment job ARN to cancel.
newCancelDeploymentJob ::
  -- | 'job'
  Prelude.Text ->
  CancelDeploymentJob
newCancelDeploymentJob pJob_ =
  CancelDeploymentJob' {job = pJob_}

-- | The deployment job ARN to cancel.
cancelDeploymentJob_job :: Lens.Lens' CancelDeploymentJob Prelude.Text
cancelDeploymentJob_job = Lens.lens (\CancelDeploymentJob' {job} -> job) (\s@CancelDeploymentJob' {} a -> s {job = a} :: CancelDeploymentJob)

instance Core.AWSRequest CancelDeploymentJob where
  type
    AWSResponse CancelDeploymentJob =
      CancelDeploymentJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelDeploymentJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelDeploymentJob

instance Prelude.NFData CancelDeploymentJob

instance Core.ToHeaders CancelDeploymentJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelDeploymentJob where
  toJSON CancelDeploymentJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("job" Core..= job)]
      )

instance Core.ToPath CancelDeploymentJob where
  toPath = Prelude.const "/cancelDeploymentJob"

instance Core.ToQuery CancelDeploymentJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelDeploymentJobResponse' smart constructor.
data CancelDeploymentJobResponse = CancelDeploymentJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelDeploymentJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelDeploymentJobResponse_httpStatus' - The response's http status code.
newCancelDeploymentJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelDeploymentJobResponse
newCancelDeploymentJobResponse pHttpStatus_ =
  CancelDeploymentJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelDeploymentJobResponse_httpStatus :: Lens.Lens' CancelDeploymentJobResponse Prelude.Int
cancelDeploymentJobResponse_httpStatus = Lens.lens (\CancelDeploymentJobResponse' {httpStatus} -> httpStatus) (\s@CancelDeploymentJobResponse' {} a -> s {httpStatus = a} :: CancelDeploymentJobResponse)

instance Prelude.NFData CancelDeploymentJobResponse
