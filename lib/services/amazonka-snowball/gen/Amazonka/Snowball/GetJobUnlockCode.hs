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
-- Module      : Amazonka.Snowball.GetJobUnlockCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @UnlockCode@ code value for the specified job. A particular
-- @UnlockCode@ value can be accessed for up to 360 days after the
-- associated job has been created.
--
-- The @UnlockCode@ value is a 29-character code with 25 alphanumeric
-- characters and 4 hyphens. This code is used to decrypt the manifest file
-- when it is passed along with the manifest to the Snow device through the
-- Snowball client when the client is started for the first time. The only
-- valid status for calling this API is @WithCustomer@ as the manifest and
-- @Unlock@ code values are used for securing your device and should only
-- be used when you have the device.
--
-- As a best practice, we recommend that you don\'t save a copy of the
-- @UnlockCode@ in the same location as the manifest file for that job.
-- Saving these separately helps prevent unauthorized parties from gaining
-- access to the Snow device associated with that job.
module Amazonka.Snowball.GetJobUnlockCode
  ( -- * Creating a Request
    GetJobUnlockCode (..),
    newGetJobUnlockCode,

    -- * Request Lenses
    getJobUnlockCode_jobId,

    -- * Destructuring the Response
    GetJobUnlockCodeResponse (..),
    newGetJobUnlockCodeResponse,

    -- * Response Lenses
    getJobUnlockCodeResponse_unlockCode,
    getJobUnlockCodeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newGetJobUnlockCode' smart constructor.
data GetJobUnlockCode = GetJobUnlockCode'
  { -- | The ID for the job that you want to get the @UnlockCode@ value for, for
    -- example @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobUnlockCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'getJobUnlockCode_jobId' - The ID for the job that you want to get the @UnlockCode@ value for, for
-- example @JID123e4567-e89b-12d3-a456-426655440000@.
newGetJobUnlockCode ::
  -- | 'jobId'
  Prelude.Text ->
  GetJobUnlockCode
newGetJobUnlockCode pJobId_ =
  GetJobUnlockCode' {jobId = pJobId_}

-- | The ID for the job that you want to get the @UnlockCode@ value for, for
-- example @JID123e4567-e89b-12d3-a456-426655440000@.
getJobUnlockCode_jobId :: Lens.Lens' GetJobUnlockCode Prelude.Text
getJobUnlockCode_jobId = Lens.lens (\GetJobUnlockCode' {jobId} -> jobId) (\s@GetJobUnlockCode' {} a -> s {jobId = a} :: GetJobUnlockCode)

instance Core.AWSRequest GetJobUnlockCode where
  type
    AWSResponse GetJobUnlockCode =
      GetJobUnlockCodeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobUnlockCodeResponse'
            Prelude.<$> (x Data..?> "UnlockCode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJobUnlockCode where
  hashWithSalt _salt GetJobUnlockCode' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetJobUnlockCode where
  rnf GetJobUnlockCode' {..} = Prelude.rnf jobId

instance Data.ToHeaders GetJobUnlockCode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.GetJobUnlockCode" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetJobUnlockCode where
  toJSON GetJobUnlockCode' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath GetJobUnlockCode where
  toPath = Prelude.const "/"

instance Data.ToQuery GetJobUnlockCode where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobUnlockCodeResponse' smart constructor.
data GetJobUnlockCodeResponse = GetJobUnlockCodeResponse'
  { -- | The @UnlockCode@ value for the specified job. The @UnlockCode@ value can
    -- be accessed for up to 360 days after the job has been created.
    unlockCode :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobUnlockCodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unlockCode', 'getJobUnlockCodeResponse_unlockCode' - The @UnlockCode@ value for the specified job. The @UnlockCode@ value can
-- be accessed for up to 360 days after the job has been created.
--
-- 'httpStatus', 'getJobUnlockCodeResponse_httpStatus' - The response's http status code.
newGetJobUnlockCodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetJobUnlockCodeResponse
newGetJobUnlockCodeResponse pHttpStatus_ =
  GetJobUnlockCodeResponse'
    { unlockCode =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @UnlockCode@ value for the specified job. The @UnlockCode@ value can
-- be accessed for up to 360 days after the job has been created.
getJobUnlockCodeResponse_unlockCode :: Lens.Lens' GetJobUnlockCodeResponse (Prelude.Maybe Prelude.Text)
getJobUnlockCodeResponse_unlockCode = Lens.lens (\GetJobUnlockCodeResponse' {unlockCode} -> unlockCode) (\s@GetJobUnlockCodeResponse' {} a -> s {unlockCode = a} :: GetJobUnlockCodeResponse)

-- | The response's http status code.
getJobUnlockCodeResponse_httpStatus :: Lens.Lens' GetJobUnlockCodeResponse Prelude.Int
getJobUnlockCodeResponse_httpStatus = Lens.lens (\GetJobUnlockCodeResponse' {httpStatus} -> httpStatus) (\s@GetJobUnlockCodeResponse' {} a -> s {httpStatus = a} :: GetJobUnlockCodeResponse)

instance Prelude.NFData GetJobUnlockCodeResponse where
  rnf GetJobUnlockCodeResponse' {..} =
    Prelude.rnf unlockCode `Prelude.seq`
      Prelude.rnf httpStatus
