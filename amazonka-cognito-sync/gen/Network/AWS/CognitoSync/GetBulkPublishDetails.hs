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
-- Module      : Network.AWS.CognitoSync.GetBulkPublishDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the status of the last BulkPublish operation for an identity pool.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
module Network.AWS.CognitoSync.GetBulkPublishDetails
  ( -- * Creating a Request
    GetBulkPublishDetails (..),
    newGetBulkPublishDetails,

    -- * Request Lenses
    getBulkPublishDetails_identityPoolId,

    -- * Destructuring the Response
    GetBulkPublishDetailsResponse (..),
    newGetBulkPublishDetailsResponse,

    -- * Response Lenses
    getBulkPublishDetailsResponse_identityPoolId,
    getBulkPublishDetailsResponse_bulkPublishStartTime,
    getBulkPublishDetailsResponse_failureMessage,
    getBulkPublishDetailsResponse_bulkPublishCompleteTime,
    getBulkPublishDetailsResponse_bulkPublishStatus,
    getBulkPublishDetailsResponse_httpStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetBulkPublishDetails operation.
--
-- /See:/ 'newGetBulkPublishDetails' smart constructor.
data GetBulkPublishDetails = GetBulkPublishDetails'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetBulkPublishDetails
newGetBulkPublishDetails pIdentityPoolId_ =
  GetBulkPublishDetails'
    { identityPoolId =
        pIdentityPoolId_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
getBulkPublishDetails_identityPoolId :: Lens.Lens' GetBulkPublishDetails Core.Text
getBulkPublishDetails_identityPoolId = Lens.lens (\GetBulkPublishDetails' {identityPoolId} -> identityPoolId) (\s@GetBulkPublishDetails' {} a -> s {identityPoolId = a} :: GetBulkPublishDetails)

instance Core.AWSRequest GetBulkPublishDetails where
  type
    AWSResponse GetBulkPublishDetails =
      GetBulkPublishDetailsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBulkPublishDetailsResponse'
            Core.<$> (x Core..?> "IdentityPoolId")
            Core.<*> (x Core..?> "BulkPublishStartTime")
            Core.<*> (x Core..?> "FailureMessage")
            Core.<*> (x Core..?> "BulkPublishCompleteTime")
            Core.<*> (x Core..?> "BulkPublishStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBulkPublishDetails

instance Core.NFData GetBulkPublishDetails

instance Core.ToHeaders GetBulkPublishDetails where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetBulkPublishDetails where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetBulkPublishDetails where
  toPath GetBulkPublishDetails' {..} =
    Core.mconcat
      [ "/identitypools/",
        Core.toBS identityPoolId,
        "/getBulkPublishDetails"
      ]

instance Core.ToQuery GetBulkPublishDetails where
  toQuery = Core.const Core.mempty

-- | The output for the GetBulkPublishDetails operation.
--
-- /See:/ 'newGetBulkPublishDetailsResponse' smart constructor.
data GetBulkPublishDetailsResponse = GetBulkPublishDetailsResponse'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Core.Maybe Core.Text,
    -- | The date\/time at which the last bulk publish was initiated.
    bulkPublishStartTime :: Core.Maybe Core.POSIX,
    -- | If BulkPublishStatus is FAILED this field will contain the error message
    -- that caused the bulk publish to fail.
    failureMessage :: Core.Maybe Core.Text,
    -- | If BulkPublishStatus is SUCCEEDED, the time the last bulk publish
    -- operation completed.
    bulkPublishCompleteTime :: Core.Maybe Core.POSIX,
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
    bulkPublishStatus :: Core.Maybe BulkPublishStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBulkPublishDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'getBulkPublishDetailsResponse_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'bulkPublishStartTime', 'getBulkPublishDetailsResponse_bulkPublishStartTime' - The date\/time at which the last bulk publish was initiated.
--
-- 'failureMessage', 'getBulkPublishDetailsResponse_failureMessage' - If BulkPublishStatus is FAILED this field will contain the error message
-- that caused the bulk publish to fail.
--
-- 'bulkPublishCompleteTime', 'getBulkPublishDetailsResponse_bulkPublishCompleteTime' - If BulkPublishStatus is SUCCEEDED, the time the last bulk publish
-- operation completed.
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
-- 'httpStatus', 'getBulkPublishDetailsResponse_httpStatus' - The response's http status code.
newGetBulkPublishDetailsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBulkPublishDetailsResponse
newGetBulkPublishDetailsResponse pHttpStatus_ =
  GetBulkPublishDetailsResponse'
    { identityPoolId =
        Core.Nothing,
      bulkPublishStartTime = Core.Nothing,
      failureMessage = Core.Nothing,
      bulkPublishCompleteTime = Core.Nothing,
      bulkPublishStatus = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
getBulkPublishDetailsResponse_identityPoolId :: Lens.Lens' GetBulkPublishDetailsResponse (Core.Maybe Core.Text)
getBulkPublishDetailsResponse_identityPoolId = Lens.lens (\GetBulkPublishDetailsResponse' {identityPoolId} -> identityPoolId) (\s@GetBulkPublishDetailsResponse' {} a -> s {identityPoolId = a} :: GetBulkPublishDetailsResponse)

-- | The date\/time at which the last bulk publish was initiated.
getBulkPublishDetailsResponse_bulkPublishStartTime :: Lens.Lens' GetBulkPublishDetailsResponse (Core.Maybe Core.UTCTime)
getBulkPublishDetailsResponse_bulkPublishStartTime = Lens.lens (\GetBulkPublishDetailsResponse' {bulkPublishStartTime} -> bulkPublishStartTime) (\s@GetBulkPublishDetailsResponse' {} a -> s {bulkPublishStartTime = a} :: GetBulkPublishDetailsResponse) Core.. Lens.mapping Core._Time

-- | If BulkPublishStatus is FAILED this field will contain the error message
-- that caused the bulk publish to fail.
getBulkPublishDetailsResponse_failureMessage :: Lens.Lens' GetBulkPublishDetailsResponse (Core.Maybe Core.Text)
getBulkPublishDetailsResponse_failureMessage = Lens.lens (\GetBulkPublishDetailsResponse' {failureMessage} -> failureMessage) (\s@GetBulkPublishDetailsResponse' {} a -> s {failureMessage = a} :: GetBulkPublishDetailsResponse)

-- | If BulkPublishStatus is SUCCEEDED, the time the last bulk publish
-- operation completed.
getBulkPublishDetailsResponse_bulkPublishCompleteTime :: Lens.Lens' GetBulkPublishDetailsResponse (Core.Maybe Core.UTCTime)
getBulkPublishDetailsResponse_bulkPublishCompleteTime = Lens.lens (\GetBulkPublishDetailsResponse' {bulkPublishCompleteTime} -> bulkPublishCompleteTime) (\s@GetBulkPublishDetailsResponse' {} a -> s {bulkPublishCompleteTime = a} :: GetBulkPublishDetailsResponse) Core.. Lens.mapping Core._Time

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
getBulkPublishDetailsResponse_bulkPublishStatus :: Lens.Lens' GetBulkPublishDetailsResponse (Core.Maybe BulkPublishStatus)
getBulkPublishDetailsResponse_bulkPublishStatus = Lens.lens (\GetBulkPublishDetailsResponse' {bulkPublishStatus} -> bulkPublishStatus) (\s@GetBulkPublishDetailsResponse' {} a -> s {bulkPublishStatus = a} :: GetBulkPublishDetailsResponse)

-- | The response's http status code.
getBulkPublishDetailsResponse_httpStatus :: Lens.Lens' GetBulkPublishDetailsResponse Core.Int
getBulkPublishDetailsResponse_httpStatus = Lens.lens (\GetBulkPublishDetailsResponse' {httpStatus} -> httpStatus) (\s@GetBulkPublishDetailsResponse' {} a -> s {httpStatus = a} :: GetBulkPublishDetailsResponse)

instance Core.NFData GetBulkPublishDetailsResponse
