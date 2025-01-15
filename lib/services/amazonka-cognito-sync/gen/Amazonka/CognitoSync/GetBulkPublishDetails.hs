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
-- Module      : Amazonka.CognitoSync.GetBulkPublishDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the status of the last BulkPublish operation for an identity pool.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
module Amazonka.CognitoSync.GetBulkPublishDetails
  ( -- * Creating a Request
    GetBulkPublishDetails (..),
    newGetBulkPublishDetails,

    -- * Request Lenses
    getBulkPublishDetails_identityPoolId,

    -- * Destructuring the Response
    GetBulkPublishDetailsResponse (..),
    newGetBulkPublishDetailsResponse,

    -- * Response Lenses
    getBulkPublishDetailsResponse_bulkPublishCompleteTime,
    getBulkPublishDetailsResponse_bulkPublishStartTime,
    getBulkPublishDetailsResponse_bulkPublishStatus,
    getBulkPublishDetailsResponse_failureMessage,
    getBulkPublishDetailsResponse_identityPoolId,
    getBulkPublishDetailsResponse_httpStatus,
  )
where

import Amazonka.CognitoSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the GetBulkPublishDetails operation.
--
-- /See:/ 'newGetBulkPublishDetails' smart constructor.
data GetBulkPublishDetails = GetBulkPublishDetails'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBulkPublishDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'getBulkPublishDetails_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
newGetBulkPublishDetails ::
  -- | 'identityPoolId'
  Prelude.Text ->
  GetBulkPublishDetails
newGetBulkPublishDetails pIdentityPoolId_ =
  GetBulkPublishDetails'
    { identityPoolId =
        pIdentityPoolId_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
getBulkPublishDetails_identityPoolId :: Lens.Lens' GetBulkPublishDetails Prelude.Text
getBulkPublishDetails_identityPoolId = Lens.lens (\GetBulkPublishDetails' {identityPoolId} -> identityPoolId) (\s@GetBulkPublishDetails' {} a -> s {identityPoolId = a} :: GetBulkPublishDetails)

instance Core.AWSRequest GetBulkPublishDetails where
  type
    AWSResponse GetBulkPublishDetails =
      GetBulkPublishDetailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBulkPublishDetailsResponse'
            Prelude.<$> (x Data..?> "BulkPublishCompleteTime")
            Prelude.<*> (x Data..?> "BulkPublishStartTime")
            Prelude.<*> (x Data..?> "BulkPublishStatus")
            Prelude.<*> (x Data..?> "FailureMessage")
            Prelude.<*> (x Data..?> "IdentityPoolId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBulkPublishDetails where
  hashWithSalt _salt GetBulkPublishDetails' {..} =
    _salt `Prelude.hashWithSalt` identityPoolId

instance Prelude.NFData GetBulkPublishDetails where
  rnf GetBulkPublishDetails' {..} =
    Prelude.rnf identityPoolId

instance Data.ToHeaders GetBulkPublishDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetBulkPublishDetails where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetBulkPublishDetails where
  toPath GetBulkPublishDetails' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Data.toBS identityPoolId,
        "/getBulkPublishDetails"
      ]

instance Data.ToQuery GetBulkPublishDetails where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the GetBulkPublishDetails operation.
--
-- /See:/ 'newGetBulkPublishDetailsResponse' smart constructor.
data GetBulkPublishDetailsResponse = GetBulkPublishDetailsResponse'
  { -- | If BulkPublishStatus is SUCCEEDED, the time the last bulk publish
    -- operation completed.
    bulkPublishCompleteTime :: Prelude.Maybe Data.POSIX,
    -- | The date\/time at which the last bulk publish was initiated.
    bulkPublishStartTime :: Prelude.Maybe Data.POSIX,
    -- | Status of the last bulk publish operation, valid values are:
    --
    -- NOT_STARTED - No bulk publish has been requested for this identity pool
    --
    -- IN_PROGRESS - Data is being published to the configured stream
    --
    -- SUCCEEDED - All data for the identity pool has been published to the
    -- configured stream
    --
    -- FAILED - Some portion of the data has failed to publish, check
    -- FailureMessage for the cause.
    bulkPublishStatus :: Prelude.Maybe BulkPublishStatus,
    -- | If BulkPublishStatus is FAILED this field will contain the error message
    -- that caused the bulk publish to fail.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBulkPublishDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bulkPublishCompleteTime', 'getBulkPublishDetailsResponse_bulkPublishCompleteTime' - If BulkPublishStatus is SUCCEEDED, the time the last bulk publish
-- operation completed.
--
-- 'bulkPublishStartTime', 'getBulkPublishDetailsResponse_bulkPublishStartTime' - The date\/time at which the last bulk publish was initiated.
--
-- 'bulkPublishStatus', 'getBulkPublishDetailsResponse_bulkPublishStatus' - Status of the last bulk publish operation, valid values are:
--
-- NOT_STARTED - No bulk publish has been requested for this identity pool
--
-- IN_PROGRESS - Data is being published to the configured stream
--
-- SUCCEEDED - All data for the identity pool has been published to the
-- configured stream
--
-- FAILED - Some portion of the data has failed to publish, check
-- FailureMessage for the cause.
--
-- 'failureMessage', 'getBulkPublishDetailsResponse_failureMessage' - If BulkPublishStatus is FAILED this field will contain the error message
-- that caused the bulk publish to fail.
--
-- 'identityPoolId', 'getBulkPublishDetailsResponse_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'httpStatus', 'getBulkPublishDetailsResponse_httpStatus' - The response's http status code.
newGetBulkPublishDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBulkPublishDetailsResponse
newGetBulkPublishDetailsResponse pHttpStatus_ =
  GetBulkPublishDetailsResponse'
    { bulkPublishCompleteTime =
        Prelude.Nothing,
      bulkPublishStartTime = Prelude.Nothing,
      bulkPublishStatus = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      identityPoolId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If BulkPublishStatus is SUCCEEDED, the time the last bulk publish
-- operation completed.
getBulkPublishDetailsResponse_bulkPublishCompleteTime :: Lens.Lens' GetBulkPublishDetailsResponse (Prelude.Maybe Prelude.UTCTime)
getBulkPublishDetailsResponse_bulkPublishCompleteTime = Lens.lens (\GetBulkPublishDetailsResponse' {bulkPublishCompleteTime} -> bulkPublishCompleteTime) (\s@GetBulkPublishDetailsResponse' {} a -> s {bulkPublishCompleteTime = a} :: GetBulkPublishDetailsResponse) Prelude.. Lens.mapping Data._Time

-- | The date\/time at which the last bulk publish was initiated.
getBulkPublishDetailsResponse_bulkPublishStartTime :: Lens.Lens' GetBulkPublishDetailsResponse (Prelude.Maybe Prelude.UTCTime)
getBulkPublishDetailsResponse_bulkPublishStartTime = Lens.lens (\GetBulkPublishDetailsResponse' {bulkPublishStartTime} -> bulkPublishStartTime) (\s@GetBulkPublishDetailsResponse' {} a -> s {bulkPublishStartTime = a} :: GetBulkPublishDetailsResponse) Prelude.. Lens.mapping Data._Time

-- | Status of the last bulk publish operation, valid values are:
--
-- NOT_STARTED - No bulk publish has been requested for this identity pool
--
-- IN_PROGRESS - Data is being published to the configured stream
--
-- SUCCEEDED - All data for the identity pool has been published to the
-- configured stream
--
-- FAILED - Some portion of the data has failed to publish, check
-- FailureMessage for the cause.
getBulkPublishDetailsResponse_bulkPublishStatus :: Lens.Lens' GetBulkPublishDetailsResponse (Prelude.Maybe BulkPublishStatus)
getBulkPublishDetailsResponse_bulkPublishStatus = Lens.lens (\GetBulkPublishDetailsResponse' {bulkPublishStatus} -> bulkPublishStatus) (\s@GetBulkPublishDetailsResponse' {} a -> s {bulkPublishStatus = a} :: GetBulkPublishDetailsResponse)

-- | If BulkPublishStatus is FAILED this field will contain the error message
-- that caused the bulk publish to fail.
getBulkPublishDetailsResponse_failureMessage :: Lens.Lens' GetBulkPublishDetailsResponse (Prelude.Maybe Prelude.Text)
getBulkPublishDetailsResponse_failureMessage = Lens.lens (\GetBulkPublishDetailsResponse' {failureMessage} -> failureMessage) (\s@GetBulkPublishDetailsResponse' {} a -> s {failureMessage = a} :: GetBulkPublishDetailsResponse)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
getBulkPublishDetailsResponse_identityPoolId :: Lens.Lens' GetBulkPublishDetailsResponse (Prelude.Maybe Prelude.Text)
getBulkPublishDetailsResponse_identityPoolId = Lens.lens (\GetBulkPublishDetailsResponse' {identityPoolId} -> identityPoolId) (\s@GetBulkPublishDetailsResponse' {} a -> s {identityPoolId = a} :: GetBulkPublishDetailsResponse)

-- | The response's http status code.
getBulkPublishDetailsResponse_httpStatus :: Lens.Lens' GetBulkPublishDetailsResponse Prelude.Int
getBulkPublishDetailsResponse_httpStatus = Lens.lens (\GetBulkPublishDetailsResponse' {httpStatus} -> httpStatus) (\s@GetBulkPublishDetailsResponse' {} a -> s {httpStatus = a} :: GetBulkPublishDetailsResponse)

instance Prelude.NFData GetBulkPublishDetailsResponse where
  rnf GetBulkPublishDetailsResponse' {..} =
    Prelude.rnf bulkPublishCompleteTime `Prelude.seq`
      Prelude.rnf bulkPublishStartTime `Prelude.seq`
        Prelude.rnf bulkPublishStatus `Prelude.seq`
          Prelude.rnf failureMessage `Prelude.seq`
            Prelude.rnf identityPoolId `Prelude.seq`
              Prelude.rnf httpStatus
