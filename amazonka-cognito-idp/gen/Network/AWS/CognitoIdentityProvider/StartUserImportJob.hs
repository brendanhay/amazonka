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
-- Module      : Network.AWS.CognitoIdentityProvider.StartUserImportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the user import.
module Network.AWS.CognitoIdentityProvider.StartUserImportJob
  ( -- * Creating a Request
    StartUserImportJob (..),
    newStartUserImportJob,

    -- * Request Lenses
    startUserImportJob_userPoolId,
    startUserImportJob_jobId,

    -- * Destructuring the Response
    StartUserImportJobResponse (..),
    newStartUserImportJobResponse,

    -- * Response Lenses
    startUserImportJobResponse_userImportJob,
    startUserImportJobResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to start the user import job.
--
-- /See:/ 'newStartUserImportJob' smart constructor.
data StartUserImportJob = StartUserImportJob'
  { -- | The user pool ID for the user pool that the users are being imported
    -- into.
    userPoolId :: Prelude.Text,
    -- | The job ID for the user import job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartUserImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'startUserImportJob_userPoolId' - The user pool ID for the user pool that the users are being imported
-- into.
--
-- 'jobId', 'startUserImportJob_jobId' - The job ID for the user import job.
newStartUserImportJob ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  StartUserImportJob
newStartUserImportJob pUserPoolId_ pJobId_ =
  StartUserImportJob'
    { userPoolId = pUserPoolId_,
      jobId = pJobId_
    }

-- | The user pool ID for the user pool that the users are being imported
-- into.
startUserImportJob_userPoolId :: Lens.Lens' StartUserImportJob Prelude.Text
startUserImportJob_userPoolId = Lens.lens (\StartUserImportJob' {userPoolId} -> userPoolId) (\s@StartUserImportJob' {} a -> s {userPoolId = a} :: StartUserImportJob)

-- | The job ID for the user import job.
startUserImportJob_jobId :: Lens.Lens' StartUserImportJob Prelude.Text
startUserImportJob_jobId = Lens.lens (\StartUserImportJob' {jobId} -> jobId) (\s@StartUserImportJob' {} a -> s {jobId = a} :: StartUserImportJob)

instance Core.AWSRequest StartUserImportJob where
  type
    AWSResponse StartUserImportJob =
      StartUserImportJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartUserImportJobResponse'
            Prelude.<$> (x Core..?> "UserImportJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartUserImportJob

instance Prelude.NFData StartUserImportJob

instance Core.ToHeaders StartUserImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.StartUserImportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartUserImportJob where
  toJSON StartUserImportJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Core..= userPoolId),
            Prelude.Just ("JobId" Core..= jobId)
          ]
      )

instance Core.ToPath StartUserImportJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StartUserImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to the request to start the user
-- import job.
--
-- /See:/ 'newStartUserImportJobResponse' smart constructor.
data StartUserImportJobResponse = StartUserImportJobResponse'
  { -- | The job object that represents the user import job.
    userImportJob :: Prelude.Maybe UserImportJobType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartUserImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userImportJob', 'startUserImportJobResponse_userImportJob' - The job object that represents the user import job.
--
-- 'httpStatus', 'startUserImportJobResponse_httpStatus' - The response's http status code.
newStartUserImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartUserImportJobResponse
newStartUserImportJobResponse pHttpStatus_ =
  StartUserImportJobResponse'
    { userImportJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The job object that represents the user import job.
startUserImportJobResponse_userImportJob :: Lens.Lens' StartUserImportJobResponse (Prelude.Maybe UserImportJobType)
startUserImportJobResponse_userImportJob = Lens.lens (\StartUserImportJobResponse' {userImportJob} -> userImportJob) (\s@StartUserImportJobResponse' {} a -> s {userImportJob = a} :: StartUserImportJobResponse)

-- | The response's http status code.
startUserImportJobResponse_httpStatus :: Lens.Lens' StartUserImportJobResponse Prelude.Int
startUserImportJobResponse_httpStatus = Lens.lens (\StartUserImportJobResponse' {httpStatus} -> httpStatus) (\s@StartUserImportJobResponse' {} a -> s {httpStatus = a} :: StartUserImportJobResponse)

instance Prelude.NFData StartUserImportJobResponse
