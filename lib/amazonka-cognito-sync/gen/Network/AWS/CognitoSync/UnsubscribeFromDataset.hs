{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.UnsubscribeFromDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unsubscribes from receiving notifications when a dataset is modified by another device.
--
-- This API can only be called with temporary credentials provided by Cognito Identity. You cannot call this API with developer credentials.
module Network.AWS.CognitoSync.UnsubscribeFromDataset
  ( -- * Creating a request
    UnsubscribeFromDataset (..),
    mkUnsubscribeFromDataset,

    -- ** Request lenses
    ufdIdentityPoolId,
    ufdIdentityId,
    ufdDatasetName,
    ufdDeviceId,

    -- * Destructuring the response
    UnsubscribeFromDatasetResponse (..),
    mkUnsubscribeFromDatasetResponse,

    -- ** Response lenses
    ufdrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to UnsubscribeFromDataset.
--
-- /See:/ 'mkUnsubscribeFromDataset' smart constructor.
data UnsubscribeFromDataset = UnsubscribeFromDataset'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. The ID of the pool to which this identity belongs.
    identityPoolId :: Types.IdentityPoolId,
    -- | Unique ID for this identity.
    identityId :: Types.IdentityId,
    -- | The name of the dataset from which to unsubcribe.
    datasetName :: Types.DatasetName,
    -- | The unique ID generated for this device by Cognito.
    deviceId :: Types.DeviceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnsubscribeFromDataset' value with any optional fields omitted.
mkUnsubscribeFromDataset ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  -- | 'identityId'
  Types.IdentityId ->
  -- | 'datasetName'
  Types.DatasetName ->
  -- | 'deviceId'
  Types.DeviceId ->
  UnsubscribeFromDataset
mkUnsubscribeFromDataset
  identityPoolId
  identityId
  datasetName
  deviceId =
    UnsubscribeFromDataset'
      { identityPoolId,
        identityId,
        datasetName,
        deviceId
      }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. The ID of the pool to which this identity belongs.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdIdentityPoolId :: Lens.Lens' UnsubscribeFromDataset Types.IdentityPoolId
ufdIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED ufdIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | Unique ID for this identity.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdIdentityId :: Lens.Lens' UnsubscribeFromDataset Types.IdentityId
ufdIdentityId = Lens.field @"identityId"
{-# DEPRECATED ufdIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The name of the dataset from which to unsubcribe.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdDatasetName :: Lens.Lens' UnsubscribeFromDataset Types.DatasetName
ufdDatasetName = Lens.field @"datasetName"
{-# DEPRECATED ufdDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | The unique ID generated for this device by Cognito.
--
-- /Note:/ Consider using 'deviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdDeviceId :: Lens.Lens' UnsubscribeFromDataset Types.DeviceId
ufdDeviceId = Lens.field @"deviceId"
{-# DEPRECATED ufdDeviceId "Use generic-lens or generic-optics with 'deviceId' instead." #-}

instance Core.AWSRequest UnsubscribeFromDataset where
  type Rs UnsubscribeFromDataset = UnsubscribeFromDatasetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/identitypools/" Core.<> (Core.toText identityPoolId)
                Core.<> ("/identities/")
                Core.<> (Core.toText identityId)
                Core.<> ("/datasets/")
                Core.<> (Core.toText datasetName)
                Core.<> ("/subscriptions/")
                Core.<> (Core.toText deviceId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UnsubscribeFromDatasetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Response to an UnsubscribeFromDataset request.
--
-- /See:/ 'mkUnsubscribeFromDatasetResponse' smart constructor.
newtype UnsubscribeFromDatasetResponse = UnsubscribeFromDatasetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UnsubscribeFromDatasetResponse' value with any optional fields omitted.
mkUnsubscribeFromDatasetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UnsubscribeFromDatasetResponse
mkUnsubscribeFromDatasetResponse responseStatus =
  UnsubscribeFromDatasetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdrrsResponseStatus :: Lens.Lens' UnsubscribeFromDatasetResponse Core.Int
ufdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ufdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
