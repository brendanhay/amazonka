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
-- Module      : Network.AWS.CognitoIdentityProvider.StopUserImportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the user import job.
module Network.AWS.CognitoIdentityProvider.StopUserImportJob
  ( -- * Creating a Request
    StopUserImportJob (..),
    newStopUserImportJob,

    -- * Request Lenses
    stopUserImportJob_userPoolId,
    stopUserImportJob_jobId,

    -- * Destructuring the Response
    StopUserImportJobResponse (..),
    newStopUserImportJobResponse,

    -- * Response Lenses
    stopUserImportJobResponse_userImportJob,
    stopUserImportJobResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to stop the user import job.
--
-- /See:/ 'newStopUserImportJob' smart constructor.
data StopUserImportJob = StopUserImportJob'
  { -- | The user pool ID for the user pool that the users are being imported
    -- into.
    userPoolId :: Core.Text,
    -- | The job ID for the user import job.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopUserImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'stopUserImportJob_userPoolId' - The user pool ID for the user pool that the users are being imported
-- into.
--
-- 'jobId', 'stopUserImportJob_jobId' - The job ID for the user import job.
newStopUserImportJob ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'jobId'
  Core.Text ->
  StopUserImportJob
newStopUserImportJob pUserPoolId_ pJobId_ =
  StopUserImportJob'
    { userPoolId = pUserPoolId_,
      jobId = pJobId_
    }

-- | The user pool ID for the user pool that the users are being imported
-- into.
stopUserImportJob_userPoolId :: Lens.Lens' StopUserImportJob Core.Text
stopUserImportJob_userPoolId = Lens.lens (\StopUserImportJob' {userPoolId} -> userPoolId) (\s@StopUserImportJob' {} a -> s {userPoolId = a} :: StopUserImportJob)

-- | The job ID for the user import job.
stopUserImportJob_jobId :: Lens.Lens' StopUserImportJob Core.Text
stopUserImportJob_jobId = Lens.lens (\StopUserImportJob' {jobId} -> jobId) (\s@StopUserImportJob' {} a -> s {jobId = a} :: StopUserImportJob)

instance Core.AWSRequest StopUserImportJob where
  type
    AWSResponse StopUserImportJob =
      StopUserImportJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopUserImportJobResponse'
            Core.<$> (x Core..?> "UserImportJob")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopUserImportJob

instance Core.NFData StopUserImportJob

instance Core.ToHeaders StopUserImportJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.StopUserImportJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopUserImportJob where
  toJSON StopUserImportJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("JobId" Core..= jobId)
          ]
      )

instance Core.ToPath StopUserImportJob where
  toPath = Core.const "/"

instance Core.ToQuery StopUserImportJob where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server to the request to stop the user
-- import job.
--
-- /See:/ 'newStopUserImportJobResponse' smart constructor.
data StopUserImportJobResponse = StopUserImportJobResponse'
  { -- | The job object that represents the user import job.
    userImportJob :: Core.Maybe UserImportJobType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopUserImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userImportJob', 'stopUserImportJobResponse_userImportJob' - The job object that represents the user import job.
--
-- 'httpStatus', 'stopUserImportJobResponse_httpStatus' - The response's http status code.
newStopUserImportJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopUserImportJobResponse
newStopUserImportJobResponse pHttpStatus_ =
  StopUserImportJobResponse'
    { userImportJob =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The job object that represents the user import job.
stopUserImportJobResponse_userImportJob :: Lens.Lens' StopUserImportJobResponse (Core.Maybe UserImportJobType)
stopUserImportJobResponse_userImportJob = Lens.lens (\StopUserImportJobResponse' {userImportJob} -> userImportJob) (\s@StopUserImportJobResponse' {} a -> s {userImportJob = a} :: StopUserImportJobResponse)

-- | The response's http status code.
stopUserImportJobResponse_httpStatus :: Lens.Lens' StopUserImportJobResponse Core.Int
stopUserImportJobResponse_httpStatus = Lens.lens (\StopUserImportJobResponse' {httpStatus} -> httpStatus) (\s@StopUserImportJobResponse' {} a -> s {httpStatus = a} :: StopUserImportJobResponse)

instance Core.NFData StopUserImportJobResponse
