{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to describe the user import job.
--
-- /See:/ 'newDescribeUserImportJob' smart constructor.
data DescribeUserImportJob = DescribeUserImportJob'
  { -- | The user pool ID for the user pool that the users are being imported
    -- into.
    userPoolId :: Prelude.Text,
    -- | The job ID for the user import job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  DescribeUserImportJob
newDescribeUserImportJob pUserPoolId_ pJobId_ =
  DescribeUserImportJob'
    { userPoolId = pUserPoolId_,
      jobId = pJobId_
    }

-- | The user pool ID for the user pool that the users are being imported
-- into.
describeUserImportJob_userPoolId :: Lens.Lens' DescribeUserImportJob Prelude.Text
describeUserImportJob_userPoolId = Lens.lens (\DescribeUserImportJob' {userPoolId} -> userPoolId) (\s@DescribeUserImportJob' {} a -> s {userPoolId = a} :: DescribeUserImportJob)

-- | The job ID for the user import job.
describeUserImportJob_jobId :: Lens.Lens' DescribeUserImportJob Prelude.Text
describeUserImportJob_jobId = Lens.lens (\DescribeUserImportJob' {jobId} -> jobId) (\s@DescribeUserImportJob' {} a -> s {jobId = a} :: DescribeUserImportJob)

instance Prelude.AWSRequest DescribeUserImportJob where
  type
    Rs DescribeUserImportJob =
      DescribeUserImportJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserImportJobResponse'
            Prelude.<$> (x Prelude..?> "UserImportJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUserImportJob

instance Prelude.NFData DescribeUserImportJob

instance Prelude.ToHeaders DescribeUserImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.DescribeUserImportJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeUserImportJob where
  toJSON DescribeUserImportJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("JobId" Prelude..= jobId)
          ]
      )

instance Prelude.ToPath DescribeUserImportJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeUserImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to the request to describe the
-- user import job.
--
-- /See:/ 'newDescribeUserImportJobResponse' smart constructor.
data DescribeUserImportJobResponse = DescribeUserImportJobResponse'
  { -- | The job object that represents the user import job.
    userImportJob :: Prelude.Maybe UserImportJobType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeUserImportJobResponse
newDescribeUserImportJobResponse pHttpStatus_ =
  DescribeUserImportJobResponse'
    { userImportJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The job object that represents the user import job.
describeUserImportJobResponse_userImportJob :: Lens.Lens' DescribeUserImportJobResponse (Prelude.Maybe UserImportJobType)
describeUserImportJobResponse_userImportJob = Lens.lens (\DescribeUserImportJobResponse' {userImportJob} -> userImportJob) (\s@DescribeUserImportJobResponse' {} a -> s {userImportJob = a} :: DescribeUserImportJobResponse)

-- | The response's http status code.
describeUserImportJobResponse_httpStatus :: Lens.Lens' DescribeUserImportJobResponse Prelude.Int
describeUserImportJobResponse_httpStatus = Lens.lens (\DescribeUserImportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeUserImportJobResponse' {} a -> s {httpStatus = a} :: DescribeUserImportJobResponse)

instance Prelude.NFData DescribeUserImportJobResponse
