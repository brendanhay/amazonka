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
    ufdDatasetName,
    ufdDeviceId,
    ufdIdentityId,

    -- * Destructuring the response
    UnsubscribeFromDatasetResponse (..),
    mkUnsubscribeFromDatasetResponse,

    -- ** Response lenses
    ufdrsResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to UnsubscribeFromDataset.
--
-- /See:/ 'mkUnsubscribeFromDataset' smart constructor.
data UnsubscribeFromDataset = UnsubscribeFromDataset'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. The ID of the pool to which this identity belongs.
    identityPoolId :: Lude.Text,
    -- | The name of the dataset from which to unsubcribe.
    datasetName :: Lude.Text,
    -- | The unique ID generated for this device by Cognito.
    deviceId :: Lude.Text,
    -- | Unique ID for this identity.
    identityId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnsubscribeFromDataset' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. The ID of the pool to which this identity belongs.
-- * 'datasetName' - The name of the dataset from which to unsubcribe.
-- * 'deviceId' - The unique ID generated for this device by Cognito.
-- * 'identityId' - Unique ID for this identity.
mkUnsubscribeFromDataset ::
  -- | 'identityPoolId'
  Lude.Text ->
  -- | 'datasetName'
  Lude.Text ->
  -- | 'deviceId'
  Lude.Text ->
  -- | 'identityId'
  Lude.Text ->
  UnsubscribeFromDataset
mkUnsubscribeFromDataset
  pIdentityPoolId_
  pDatasetName_
  pDeviceId_
  pIdentityId_ =
    UnsubscribeFromDataset'
      { identityPoolId = pIdentityPoolId_,
        datasetName = pDatasetName_,
        deviceId = pDeviceId_,
        identityId = pIdentityId_
      }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. The ID of the pool to which this identity belongs.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdIdentityPoolId :: Lens.Lens' UnsubscribeFromDataset Lude.Text
ufdIdentityPoolId = Lens.lens (identityPoolId :: UnsubscribeFromDataset -> Lude.Text) (\s a -> s {identityPoolId = a} :: UnsubscribeFromDataset)
{-# DEPRECATED ufdIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | The name of the dataset from which to unsubcribe.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdDatasetName :: Lens.Lens' UnsubscribeFromDataset Lude.Text
ufdDatasetName = Lens.lens (datasetName :: UnsubscribeFromDataset -> Lude.Text) (\s a -> s {datasetName = a} :: UnsubscribeFromDataset)
{-# DEPRECATED ufdDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | The unique ID generated for this device by Cognito.
--
-- /Note:/ Consider using 'deviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdDeviceId :: Lens.Lens' UnsubscribeFromDataset Lude.Text
ufdDeviceId = Lens.lens (deviceId :: UnsubscribeFromDataset -> Lude.Text) (\s a -> s {deviceId = a} :: UnsubscribeFromDataset)
{-# DEPRECATED ufdDeviceId "Use generic-lens or generic-optics with 'deviceId' instead." #-}

-- | Unique ID for this identity.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdIdentityId :: Lens.Lens' UnsubscribeFromDataset Lude.Text
ufdIdentityId = Lens.lens (identityId :: UnsubscribeFromDataset -> Lude.Text) (\s a -> s {identityId = a} :: UnsubscribeFromDataset)
{-# DEPRECATED ufdIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

instance Lude.AWSRequest UnsubscribeFromDataset where
  type Rs UnsubscribeFromDataset = UnsubscribeFromDatasetResponse
  request = Req.delete cognitoSyncService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UnsubscribeFromDatasetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UnsubscribeFromDataset where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath UnsubscribeFromDataset where
  toPath UnsubscribeFromDataset' {..} =
    Lude.mconcat
      [ "/identitypools/",
        Lude.toBS identityPoolId,
        "/identities/",
        Lude.toBS identityId,
        "/datasets/",
        Lude.toBS datasetName,
        "/subscriptions/",
        Lude.toBS deviceId
      ]

instance Lude.ToQuery UnsubscribeFromDataset where
  toQuery = Lude.const Lude.mempty

-- | Response to an UnsubscribeFromDataset request.
--
-- /See:/ 'mkUnsubscribeFromDatasetResponse' smart constructor.
newtype UnsubscribeFromDatasetResponse = UnsubscribeFromDatasetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnsubscribeFromDatasetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUnsubscribeFromDatasetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UnsubscribeFromDatasetResponse
mkUnsubscribeFromDatasetResponse pResponseStatus_ =
  UnsubscribeFromDatasetResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdrsResponseStatus :: Lens.Lens' UnsubscribeFromDatasetResponse Lude.Int
ufdrsResponseStatus = Lens.lens (responseStatus :: UnsubscribeFromDatasetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UnsubscribeFromDatasetResponse)
{-# DEPRECATED ufdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
