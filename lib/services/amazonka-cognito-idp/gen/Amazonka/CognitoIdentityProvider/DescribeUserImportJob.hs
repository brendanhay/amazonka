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
-- Module      : Amazonka.CognitoIdentityProvider.DescribeUserImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the user import job.
module Amazonka.CognitoIdentityProvider.DescribeUserImportJob
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DescribeUserImportJob where
  type
    AWSResponse DescribeUserImportJob =
      DescribeUserImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserImportJobResponse'
            Prelude.<$> (x Data..?> "UserImportJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUserImportJob where
  hashWithSalt _salt DescribeUserImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeUserImportJob where
  rnf DescribeUserImportJob' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders DescribeUserImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.DescribeUserImportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeUserImportJob where
  toJSON DescribeUserImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("JobId" Data..= jobId)
          ]
      )

instance Data.ToPath DescribeUserImportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeUserImportJob where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DescribeUserImportJobResponse where
  rnf DescribeUserImportJobResponse' {..} =
    Prelude.rnf userImportJob
      `Prelude.seq` Prelude.rnf httpStatus
