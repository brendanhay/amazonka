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
-- Module      : Network.AWS.CognitoIdentityProvider.CreateUserImportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the user import job.
module Network.AWS.CognitoIdentityProvider.CreateUserImportJob
  ( -- * Creating a Request
    CreateUserImportJob (..),
    newCreateUserImportJob,

    -- * Request Lenses
    createUserImportJob_jobName,
    createUserImportJob_userPoolId,
    createUserImportJob_cloudWatchLogsRoleArn,

    -- * Destructuring the Response
    CreateUserImportJobResponse (..),
    newCreateUserImportJobResponse,

    -- * Response Lenses
    createUserImportJobResponse_userImportJob,
    createUserImportJobResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to create the user import job.
--
-- /See:/ 'newCreateUserImportJob' smart constructor.
data CreateUserImportJob = CreateUserImportJob'
  { -- | The job name for the user import job.
    jobName :: Prelude.Text,
    -- | The user pool ID for the user pool that the users are being imported
    -- into.
    userPoolId :: Prelude.Text,
    -- | The role ARN for the Amazon CloudWatch Logging role for the user import
    -- job.
    cloudWatchLogsRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'createUserImportJob_jobName' - The job name for the user import job.
--
-- 'userPoolId', 'createUserImportJob_userPoolId' - The user pool ID for the user pool that the users are being imported
-- into.
--
-- 'cloudWatchLogsRoleArn', 'createUserImportJob_cloudWatchLogsRoleArn' - The role ARN for the Amazon CloudWatch Logging role for the user import
-- job.
newCreateUserImportJob ::
  -- | 'jobName'
  Prelude.Text ->
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'cloudWatchLogsRoleArn'
  Prelude.Text ->
  CreateUserImportJob
newCreateUserImportJob
  pJobName_
  pUserPoolId_
  pCloudWatchLogsRoleArn_ =
    CreateUserImportJob'
      { jobName = pJobName_,
        userPoolId = pUserPoolId_,
        cloudWatchLogsRoleArn = pCloudWatchLogsRoleArn_
      }

-- | The job name for the user import job.
createUserImportJob_jobName :: Lens.Lens' CreateUserImportJob Prelude.Text
createUserImportJob_jobName = Lens.lens (\CreateUserImportJob' {jobName} -> jobName) (\s@CreateUserImportJob' {} a -> s {jobName = a} :: CreateUserImportJob)

-- | The user pool ID for the user pool that the users are being imported
-- into.
createUserImportJob_userPoolId :: Lens.Lens' CreateUserImportJob Prelude.Text
createUserImportJob_userPoolId = Lens.lens (\CreateUserImportJob' {userPoolId} -> userPoolId) (\s@CreateUserImportJob' {} a -> s {userPoolId = a} :: CreateUserImportJob)

-- | The role ARN for the Amazon CloudWatch Logging role for the user import
-- job.
createUserImportJob_cloudWatchLogsRoleArn :: Lens.Lens' CreateUserImportJob Prelude.Text
createUserImportJob_cloudWatchLogsRoleArn = Lens.lens (\CreateUserImportJob' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@CreateUserImportJob' {} a -> s {cloudWatchLogsRoleArn = a} :: CreateUserImportJob)

instance Core.AWSRequest CreateUserImportJob where
  type
    AWSResponse CreateUserImportJob =
      CreateUserImportJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserImportJobResponse'
            Prelude.<$> (x Core..?> "UserImportJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserImportJob

instance Prelude.NFData CreateUserImportJob

instance Core.ToHeaders CreateUserImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.CreateUserImportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateUserImportJob where
  toJSON CreateUserImportJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("JobName" Core..= jobName),
            Prelude.Just ("UserPoolId" Core..= userPoolId),
            Prelude.Just
              ( "CloudWatchLogsRoleArn"
                  Core..= cloudWatchLogsRoleArn
              )
          ]
      )

instance Core.ToPath CreateUserImportJob where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateUserImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to the request to create the
-- user import job.
--
-- /See:/ 'newCreateUserImportJobResponse' smart constructor.
data CreateUserImportJobResponse = CreateUserImportJobResponse'
  { -- | The job object that represents the user import job.
    userImportJob :: Prelude.Maybe UserImportJobType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userImportJob', 'createUserImportJobResponse_userImportJob' - The job object that represents the user import job.
--
-- 'httpStatus', 'createUserImportJobResponse_httpStatus' - The response's http status code.
newCreateUserImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUserImportJobResponse
newCreateUserImportJobResponse pHttpStatus_ =
  CreateUserImportJobResponse'
    { userImportJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The job object that represents the user import job.
createUserImportJobResponse_userImportJob :: Lens.Lens' CreateUserImportJobResponse (Prelude.Maybe UserImportJobType)
createUserImportJobResponse_userImportJob = Lens.lens (\CreateUserImportJobResponse' {userImportJob} -> userImportJob) (\s@CreateUserImportJobResponse' {} a -> s {userImportJob = a} :: CreateUserImportJobResponse)

-- | The response's http status code.
createUserImportJobResponse_httpStatus :: Lens.Lens' CreateUserImportJobResponse Prelude.Int
createUserImportJobResponse_httpStatus = Lens.lens (\CreateUserImportJobResponse' {httpStatus} -> httpStatus) (\s@CreateUserImportJobResponse' {} a -> s {httpStatus = a} :: CreateUserImportJobResponse)

instance Prelude.NFData CreateUserImportJobResponse
