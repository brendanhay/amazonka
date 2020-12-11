{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.SubscribeToDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Subscribes to receive notifications when a dataset is modified by another device.
--
-- This API can only be called with temporary credentials provided by Cognito Identity. You cannot call this API with developer credentials.
module Network.AWS.CognitoSync.SubscribeToDataset
  ( -- * Creating a request
    SubscribeToDataset (..),
    mkSubscribeToDataset,

    -- ** Request lenses
    stdIdentityPoolId,
    stdIdentityId,
    stdDatasetName,
    stdDeviceId,

    -- * Destructuring the response
    SubscribeToDatasetResponse (..),
    mkSubscribeToDatasetResponse,

    -- ** Response lenses
    stdrsResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to SubscribeToDatasetRequest.
--
-- /See:/ 'mkSubscribeToDataset' smart constructor.
data SubscribeToDataset = SubscribeToDataset'
  { identityPoolId ::
      Lude.Text,
    identityId :: Lude.Text,
    datasetName :: Lude.Text,
    deviceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubscribeToDataset' with the minimum fields required to make a request.
--
-- * 'datasetName' - The name of the dataset to subcribe to.
-- * 'deviceId' - The unique ID generated for this device by Cognito.
-- * 'identityId' - Unique ID for this identity.
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. The ID of the pool to which the identity belongs.
mkSubscribeToDataset ::
  -- | 'identityPoolId'
  Lude.Text ->
  -- | 'identityId'
  Lude.Text ->
  -- | 'datasetName'
  Lude.Text ->
  -- | 'deviceId'
  Lude.Text ->
  SubscribeToDataset
mkSubscribeToDataset
  pIdentityPoolId_
  pIdentityId_
  pDatasetName_
  pDeviceId_ =
    SubscribeToDataset'
      { identityPoolId = pIdentityPoolId_,
        identityId = pIdentityId_,
        datasetName = pDatasetName_,
        deviceId = pDeviceId_
      }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. The ID of the pool to which the identity belongs.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdIdentityPoolId :: Lens.Lens' SubscribeToDataset Lude.Text
stdIdentityPoolId = Lens.lens (identityPoolId :: SubscribeToDataset -> Lude.Text) (\s a -> s {identityPoolId = a} :: SubscribeToDataset)
{-# DEPRECATED stdIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | Unique ID for this identity.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdIdentityId :: Lens.Lens' SubscribeToDataset Lude.Text
stdIdentityId = Lens.lens (identityId :: SubscribeToDataset -> Lude.Text) (\s a -> s {identityId = a} :: SubscribeToDataset)
{-# DEPRECATED stdIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The name of the dataset to subcribe to.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdDatasetName :: Lens.Lens' SubscribeToDataset Lude.Text
stdDatasetName = Lens.lens (datasetName :: SubscribeToDataset -> Lude.Text) (\s a -> s {datasetName = a} :: SubscribeToDataset)
{-# DEPRECATED stdDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | The unique ID generated for this device by Cognito.
--
-- /Note:/ Consider using 'deviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdDeviceId :: Lens.Lens' SubscribeToDataset Lude.Text
stdDeviceId = Lens.lens (deviceId :: SubscribeToDataset -> Lude.Text) (\s a -> s {deviceId = a} :: SubscribeToDataset)
{-# DEPRECATED stdDeviceId "Use generic-lens or generic-optics with 'deviceId' instead." #-}

instance Lude.AWSRequest SubscribeToDataset where
  type Rs SubscribeToDataset = SubscribeToDatasetResponse
  request = Req.postJSON cognitoSyncService
  response =
    Res.receiveEmpty
      ( \s h x ->
          SubscribeToDatasetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SubscribeToDataset where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SubscribeToDataset where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath SubscribeToDataset where
  toPath SubscribeToDataset' {..} =
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

instance Lude.ToQuery SubscribeToDataset where
  toQuery = Lude.const Lude.mempty

-- | Response to a SubscribeToDataset request.
--
-- /See:/ 'mkSubscribeToDatasetResponse' smart constructor.
newtype SubscribeToDatasetResponse = SubscribeToDatasetResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubscribeToDatasetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSubscribeToDatasetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SubscribeToDatasetResponse
mkSubscribeToDatasetResponse pResponseStatus_ =
  SubscribeToDatasetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdrsResponseStatus :: Lens.Lens' SubscribeToDatasetResponse Lude.Int
stdrsResponseStatus = Lens.lens (responseStatus :: SubscribeToDatasetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SubscribeToDatasetResponse)
{-# DEPRECATED stdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
