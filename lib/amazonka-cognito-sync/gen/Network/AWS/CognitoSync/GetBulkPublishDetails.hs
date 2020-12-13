{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.GetBulkPublishDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the status of the last BulkPublish operation for an identity pool.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.GetBulkPublishDetails
  ( -- * Creating a request
    GetBulkPublishDetails (..),
    mkGetBulkPublishDetails,

    -- ** Request lenses
    gbpdIdentityPoolId,

    -- * Destructuring the response
    GetBulkPublishDetailsResponse (..),
    mkGetBulkPublishDetailsResponse,

    -- ** Response lenses
    gbpdrsBulkPublishStartTime,
    gbpdrsIdentityPoolId,
    gbpdrsBulkPublishCompleteTime,
    gbpdrsFailureMessage,
    gbpdrsBulkPublishStatus,
    gbpdrsResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the GetBulkPublishDetails operation.
--
-- /See:/ 'mkGetBulkPublishDetails' smart constructor.
newtype GetBulkPublishDetails = GetBulkPublishDetails'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBulkPublishDetails' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
mkGetBulkPublishDetails ::
  -- | 'identityPoolId'
  Lude.Text ->
  GetBulkPublishDetails
mkGetBulkPublishDetails pIdentityPoolId_ =
  GetBulkPublishDetails' {identityPoolId = pIdentityPoolId_}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdIdentityPoolId :: Lens.Lens' GetBulkPublishDetails Lude.Text
gbpdIdentityPoolId = Lens.lens (identityPoolId :: GetBulkPublishDetails -> Lude.Text) (\s a -> s {identityPoolId = a} :: GetBulkPublishDetails)
{-# DEPRECATED gbpdIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Lude.AWSRequest GetBulkPublishDetails where
  type Rs GetBulkPublishDetails = GetBulkPublishDetailsResponse
  request = Req.postJSON cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBulkPublishDetailsResponse'
            Lude.<$> (x Lude..?> "BulkPublishStartTime")
            Lude.<*> (x Lude..?> "IdentityPoolId")
            Lude.<*> (x Lude..?> "BulkPublishCompleteTime")
            Lude.<*> (x Lude..?> "FailureMessage")
            Lude.<*> (x Lude..?> "BulkPublishStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBulkPublishDetails where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetBulkPublishDetails where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetBulkPublishDetails where
  toPath GetBulkPublishDetails' {..} =
    Lude.mconcat
      [ "/identitypools/",
        Lude.toBS identityPoolId,
        "/getBulkPublishDetails"
      ]

instance Lude.ToQuery GetBulkPublishDetails where
  toQuery = Lude.const Lude.mempty

-- | The output for the GetBulkPublishDetails operation.
--
-- /See:/ 'mkGetBulkPublishDetailsResponse' smart constructor.
data GetBulkPublishDetailsResponse = GetBulkPublishDetailsResponse'
  { -- | The date/time at which the last bulk publish was initiated.
    bulkPublishStartTime :: Lude.Maybe Lude.Timestamp,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Lude.Maybe Lude.Text,
    -- | If BulkPublishStatus is SUCCEEDED, the time the last bulk publish operation completed.
    bulkPublishCompleteTime :: Lude.Maybe Lude.Timestamp,
    -- | If BulkPublishStatus is FAILED this field will contain the error message that caused the bulk publish to fail.
    failureMessage :: Lude.Maybe Lude.Text,
    -- | Status of the last bulk publish operation, valid values are: NOT_STARTED - No bulk publish has been requested for this identity pool
    --
    -- IN_PROGRESS - Data is being published to the configured stream
    -- SUCCEEDED - All data for the identity pool has been published to the configured stream
    -- FAILED - Some portion of the data has failed to publish, check FailureMessage for the cause.
    bulkPublishStatus :: Lude.Maybe BulkPublishStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBulkPublishDetailsResponse' with the minimum fields required to make a request.
--
-- * 'bulkPublishStartTime' - The date/time at which the last bulk publish was initiated.
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'bulkPublishCompleteTime' - If BulkPublishStatus is SUCCEEDED, the time the last bulk publish operation completed.
-- * 'failureMessage' - If BulkPublishStatus is FAILED this field will contain the error message that caused the bulk publish to fail.
-- * 'bulkPublishStatus' - Status of the last bulk publish operation, valid values are: NOT_STARTED - No bulk publish has been requested for this identity pool
--
-- IN_PROGRESS - Data is being published to the configured stream
-- SUCCEEDED - All data for the identity pool has been published to the configured stream
-- FAILED - Some portion of the data has failed to publish, check FailureMessage for the cause.
-- * 'responseStatus' - The response status code.
mkGetBulkPublishDetailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBulkPublishDetailsResponse
mkGetBulkPublishDetailsResponse pResponseStatus_ =
  GetBulkPublishDetailsResponse'
    { bulkPublishStartTime =
        Lude.Nothing,
      identityPoolId = Lude.Nothing,
      bulkPublishCompleteTime = Lude.Nothing,
      failureMessage = Lude.Nothing,
      bulkPublishStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date/time at which the last bulk publish was initiated.
--
-- /Note:/ Consider using 'bulkPublishStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdrsBulkPublishStartTime :: Lens.Lens' GetBulkPublishDetailsResponse (Lude.Maybe Lude.Timestamp)
gbpdrsBulkPublishStartTime = Lens.lens (bulkPublishStartTime :: GetBulkPublishDetailsResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {bulkPublishStartTime = a} :: GetBulkPublishDetailsResponse)
{-# DEPRECATED gbpdrsBulkPublishStartTime "Use generic-lens or generic-optics with 'bulkPublishStartTime' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdrsIdentityPoolId :: Lens.Lens' GetBulkPublishDetailsResponse (Lude.Maybe Lude.Text)
gbpdrsIdentityPoolId = Lens.lens (identityPoolId :: GetBulkPublishDetailsResponse -> Lude.Maybe Lude.Text) (\s a -> s {identityPoolId = a} :: GetBulkPublishDetailsResponse)
{-# DEPRECATED gbpdrsIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | If BulkPublishStatus is SUCCEEDED, the time the last bulk publish operation completed.
--
-- /Note:/ Consider using 'bulkPublishCompleteTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdrsBulkPublishCompleteTime :: Lens.Lens' GetBulkPublishDetailsResponse (Lude.Maybe Lude.Timestamp)
gbpdrsBulkPublishCompleteTime = Lens.lens (bulkPublishCompleteTime :: GetBulkPublishDetailsResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {bulkPublishCompleteTime = a} :: GetBulkPublishDetailsResponse)
{-# DEPRECATED gbpdrsBulkPublishCompleteTime "Use generic-lens or generic-optics with 'bulkPublishCompleteTime' instead." #-}

-- | If BulkPublishStatus is FAILED this field will contain the error message that caused the bulk publish to fail.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdrsFailureMessage :: Lens.Lens' GetBulkPublishDetailsResponse (Lude.Maybe Lude.Text)
gbpdrsFailureMessage = Lens.lens (failureMessage :: GetBulkPublishDetailsResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureMessage = a} :: GetBulkPublishDetailsResponse)
{-# DEPRECATED gbpdrsFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | Status of the last bulk publish operation, valid values are: NOT_STARTED - No bulk publish has been requested for this identity pool
--
-- IN_PROGRESS - Data is being published to the configured stream
-- SUCCEEDED - All data for the identity pool has been published to the configured stream
-- FAILED - Some portion of the data has failed to publish, check FailureMessage for the cause.
--
-- /Note:/ Consider using 'bulkPublishStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdrsBulkPublishStatus :: Lens.Lens' GetBulkPublishDetailsResponse (Lude.Maybe BulkPublishStatus)
gbpdrsBulkPublishStatus = Lens.lens (bulkPublishStatus :: GetBulkPublishDetailsResponse -> Lude.Maybe BulkPublishStatus) (\s a -> s {bulkPublishStatus = a} :: GetBulkPublishDetailsResponse)
{-# DEPRECATED gbpdrsBulkPublishStatus "Use generic-lens or generic-optics with 'bulkPublishStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdrsResponseStatus :: Lens.Lens' GetBulkPublishDetailsResponse Lude.Int
gbpdrsResponseStatus = Lens.lens (responseStatus :: GetBulkPublishDetailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBulkPublishDetailsResponse)
{-# DEPRECATED gbpdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
