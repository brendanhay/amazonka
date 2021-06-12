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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserImportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the user import job.
module Network.AWS.CognitoIdentityProvider.DescribeUserImportJob
  ( -- * Creating a Request
    DescribeUserImportJob (..),
    newDescribeUserImportJob,

    -- * Request Lenses
    describeUserImportJob_userPoolId,
    describeUserImportJob_jobId,

    -- * Destructuring the Response
    DescribeUserImportJobResponse (..),
    newDescribeUserImportJobResponse,

    -- * Response Lenses
    describeUserImportJobResponse_userImportJob,
    describeUserImportJobResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to describe the user import job.
--
-- /See:/ 'newDescribeUserImportJob' smart constructor.
data DescribeUserImportJob = DescribeUserImportJob'
  { -- | The user pool ID for the user pool that the users are being imported
    -- into.
    userPoolId :: Core.Text,
    -- | The job ID for the user import job.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'describeUserImportJob_userPoolId' - The user pool ID for the user pool that the users are being imported
-- into.
--
-- 'jobId', 'describeUserImportJob_jobId' - The job ID for the user import job.
newDescribeUserImportJob ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'jobId'
  Core.Text ->
  DescribeUserImportJob
newDescribeUserImportJob pUserPoolId_ pJobId_ =
  DescribeUserImportJob'
    { userPoolId = pUserPoolId_,
      jobId = pJobId_
    }

-- | The user pool ID for the user pool that the users are being imported
-- into.
describeUserImportJob_userPoolId :: Lens.Lens' DescribeUserImportJob Core.Text
describeUserImportJob_userPoolId = Lens.lens (\DescribeUserImportJob' {userPoolId} -> userPoolId) (\s@DescribeUserImportJob' {} a -> s {userPoolId = a} :: DescribeUserImportJob)

-- | The job ID for the user import job.
describeUserImportJob_jobId :: Lens.Lens' DescribeUserImportJob Core.Text
describeUserImportJob_jobId = Lens.lens (\DescribeUserImportJob' {jobId} -> jobId) (\s@DescribeUserImportJob' {} a -> s {jobId = a} :: DescribeUserImportJob)

instance Core.AWSRequest DescribeUserImportJob where
  type
    AWSResponse DescribeUserImportJob =
      DescribeUserImportJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserImportJobResponse'
            Core.<$> (x Core..?> "UserImportJob")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeUserImportJob

instance Core.NFData DescribeUserImportJob

instance Core.ToHeaders DescribeUserImportJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.DescribeUserImportJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeUserImportJob where
  toJSON DescribeUserImportJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("JobId" Core..= jobId)
          ]
      )

instance Core.ToPath DescribeUserImportJob where
  toPath = Core.const "/"

instance Core.ToQuery DescribeUserImportJob where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server to the request to describe the
-- user import job.
--
-- /See:/ 'newDescribeUserImportJobResponse' smart constructor.
data DescribeUserImportJobResponse = DescribeUserImportJobResponse'
  { -- | The job object that represents the user import job.
    userImportJob :: Core.Maybe UserImportJobType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userImportJob', 'describeUserImportJobResponse_userImportJob' - The job object that represents the user import job.
--
-- 'httpStatus', 'describeUserImportJobResponse_httpStatus' - The response's http status code.
newDescribeUserImportJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeUserImportJobResponse
newDescribeUserImportJobResponse pHttpStatus_ =
  DescribeUserImportJobResponse'
    { userImportJob =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The job object that represents the user import job.
describeUserImportJobResponse_userImportJob :: Lens.Lens' DescribeUserImportJobResponse (Core.Maybe UserImportJobType)
describeUserImportJobResponse_userImportJob = Lens.lens (\DescribeUserImportJobResponse' {userImportJob} -> userImportJob) (\s@DescribeUserImportJobResponse' {} a -> s {userImportJob = a} :: DescribeUserImportJobResponse)

-- | The response's http status code.
describeUserImportJobResponse_httpStatus :: Lens.Lens' DescribeUserImportJobResponse Core.Int
describeUserImportJobResponse_httpStatus = Lens.lens (\DescribeUserImportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeUserImportJobResponse' {} a -> s {httpStatus = a} :: DescribeUserImportJobResponse)

instance Core.NFData DescribeUserImportJobResponse
