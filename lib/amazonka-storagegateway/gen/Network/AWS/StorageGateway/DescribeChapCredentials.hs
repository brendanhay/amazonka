{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeChapCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of Challenge-Handshake Authentication Protocol (CHAP) credentials information for a specified iSCSI target, one for each target-initiator pair. This operation is supported in the volume and tape gateway types.
module Network.AWS.StorageGateway.DescribeChapCredentials
  ( -- * Creating a request
    DescribeChapCredentials (..),
    mkDescribeChapCredentials,

    -- ** Request lenses
    dTargetARN,

    -- * Destructuring the response
    DescribeChapCredentialsResponse (..),
    mkDescribeChapCredentialsResponse,

    -- ** Response lenses
    dccsrsChapCredentials,
    dccsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the iSCSI volume target.
--
-- /See:/ 'mkDescribeChapCredentials' smart constructor.
newtype DescribeChapCredentials = DescribeChapCredentials'
  { -- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
    targetARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeChapCredentials' with the minimum fields required to make a request.
--
-- * 'targetARN' - The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
mkDescribeChapCredentials ::
  -- | 'targetARN'
  Lude.Text ->
  DescribeChapCredentials
mkDescribeChapCredentials pTargetARN_ =
  DescribeChapCredentials' {targetARN = pTargetARN_}

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTargetARN :: Lens.Lens' DescribeChapCredentials Lude.Text
dTargetARN = Lens.lens (targetARN :: DescribeChapCredentials -> Lude.Text) (\s a -> s {targetARN = a} :: DescribeChapCredentials)
{-# DEPRECATED dTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

instance Lude.AWSRequest DescribeChapCredentials where
  type Rs DescribeChapCredentials = DescribeChapCredentialsResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeChapCredentialsResponse'
            Lude.<$> (x Lude..?> "ChapCredentials" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeChapCredentials where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeChapCredentials" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeChapCredentials where
  toJSON DescribeChapCredentials' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TargetARN" Lude..= targetARN)])

instance Lude.ToPath DescribeChapCredentials where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeChapCredentials where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkDescribeChapCredentialsResponse' smart constructor.
data DescribeChapCredentialsResponse = DescribeChapCredentialsResponse'
  { -- | An array of 'ChapInfo' objects that represent CHAP credentials. Each object in the array contains CHAP credential information for one target-initiator pair. If no CHAP credentials are set, an empty array is returned. CHAP credential information is provided in a JSON object with the following fields:
    --
    --
    --     * __InitiatorName__ : The iSCSI initiator that connects to the target.
    --
    --
    --     * __SecretToAuthenticateInitiator__ : The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
    --
    --
    --     * __SecretToAuthenticateTarget__ : The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g. Windows client).
    --
    --
    --     * __TargetARN__ : The Amazon Resource Name (ARN) of the storage volume.
    chapCredentials :: Lude.Maybe [ChapInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeChapCredentialsResponse' with the minimum fields required to make a request.
--
-- * 'chapCredentials' - An array of 'ChapInfo' objects that represent CHAP credentials. Each object in the array contains CHAP credential information for one target-initiator pair. If no CHAP credentials are set, an empty array is returned. CHAP credential information is provided in a JSON object with the following fields:
--
--
--     * __InitiatorName__ : The iSCSI initiator that connects to the target.
--
--
--     * __SecretToAuthenticateInitiator__ : The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
--
--
--     * __SecretToAuthenticateTarget__ : The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g. Windows client).
--
--
--     * __TargetARN__ : The Amazon Resource Name (ARN) of the storage volume.
--
--
-- * 'responseStatus' - The response status code.
mkDescribeChapCredentialsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeChapCredentialsResponse
mkDescribeChapCredentialsResponse pResponseStatus_ =
  DescribeChapCredentialsResponse'
    { chapCredentials = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'ChapInfo' objects that represent CHAP credentials. Each object in the array contains CHAP credential information for one target-initiator pair. If no CHAP credentials are set, an empty array is returned. CHAP credential information is provided in a JSON object with the following fields:
--
--
--     * __InitiatorName__ : The iSCSI initiator that connects to the target.
--
--
--     * __SecretToAuthenticateInitiator__ : The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
--
--
--     * __SecretToAuthenticateTarget__ : The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g. Windows client).
--
--
--     * __TargetARN__ : The Amazon Resource Name (ARN) of the storage volume.
--
--
--
-- /Note:/ Consider using 'chapCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccsrsChapCredentials :: Lens.Lens' DescribeChapCredentialsResponse (Lude.Maybe [ChapInfo])
dccsrsChapCredentials = Lens.lens (chapCredentials :: DescribeChapCredentialsResponse -> Lude.Maybe [ChapInfo]) (\s a -> s {chapCredentials = a} :: DescribeChapCredentialsResponse)
{-# DEPRECATED dccsrsChapCredentials "Use generic-lens or generic-optics with 'chapCredentials' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccsrsResponseStatus :: Lens.Lens' DescribeChapCredentialsResponse Lude.Int
dccsrsResponseStatus = Lens.lens (responseStatus :: DescribeChapCredentialsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeChapCredentialsResponse)
{-# DEPRECATED dccsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
