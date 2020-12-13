{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteChapCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes Challenge-Handshake Authentication Protocol (CHAP) credentials for a specified iSCSI target and initiator pair. This operation is supported in volume and tape gateway types.
module Network.AWS.StorageGateway.DeleteChapCredentials
  ( -- * Creating a request
    DeleteChapCredentials (..),
    mkDeleteChapCredentials,

    -- ** Request lenses
    dccTargetARN,
    dccInitiatorName,

    -- * Destructuring the response
    DeleteChapCredentialsResponse (..),
    mkDeleteChapCredentialsResponse,

    -- ** Response lenses
    dccrsTargetARN,
    dccrsInitiatorName,
    dccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'DeleteChapCredentialsInput$InitiatorName'
--
--
--     * 'DeleteChapCredentialsInput$TargetARN'
--
--
--
-- /See:/ 'mkDeleteChapCredentials' smart constructor.
data DeleteChapCredentials = DeleteChapCredentials'
  { -- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
    targetARN :: Lude.Text,
    -- | The iSCSI initiator that connects to the target.
    initiatorName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteChapCredentials' with the minimum fields required to make a request.
--
-- * 'targetARN' - The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
-- * 'initiatorName' - The iSCSI initiator that connects to the target.
mkDeleteChapCredentials ::
  -- | 'targetARN'
  Lude.Text ->
  -- | 'initiatorName'
  Lude.Text ->
  DeleteChapCredentials
mkDeleteChapCredentials pTargetARN_ pInitiatorName_ =
  DeleteChapCredentials'
    { targetARN = pTargetARN_,
      initiatorName = pInitiatorName_
    }

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccTargetARN :: Lens.Lens' DeleteChapCredentials Lude.Text
dccTargetARN = Lens.lens (targetARN :: DeleteChapCredentials -> Lude.Text) (\s a -> s {targetARN = a} :: DeleteChapCredentials)
{-# DEPRECATED dccTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The iSCSI initiator that connects to the target.
--
-- /Note:/ Consider using 'initiatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccInitiatorName :: Lens.Lens' DeleteChapCredentials Lude.Text
dccInitiatorName = Lens.lens (initiatorName :: DeleteChapCredentials -> Lude.Text) (\s a -> s {initiatorName = a} :: DeleteChapCredentials)
{-# DEPRECATED dccInitiatorName "Use generic-lens or generic-optics with 'initiatorName' instead." #-}

instance Lude.AWSRequest DeleteChapCredentials where
  type Rs DeleteChapCredentials = DeleteChapCredentialsResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteChapCredentialsResponse'
            Lude.<$> (x Lude..?> "TargetARN")
            Lude.<*> (x Lude..?> "InitiatorName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteChapCredentials where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DeleteChapCredentials" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteChapCredentials where
  toJSON DeleteChapCredentials' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TargetARN" Lude..= targetARN),
            Lude.Just ("InitiatorName" Lude..= initiatorName)
          ]
      )

instance Lude.ToPath DeleteChapCredentials where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteChapCredentials where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkDeleteChapCredentialsResponse' smart constructor.
data DeleteChapCredentialsResponse = DeleteChapCredentialsResponse'
  { -- | The Amazon Resource Name (ARN) of the target.
    targetARN :: Lude.Maybe Lude.Text,
    -- | The iSCSI initiator that connects to the target.
    initiatorName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteChapCredentialsResponse' with the minimum fields required to make a request.
--
-- * 'targetARN' - The Amazon Resource Name (ARN) of the target.
-- * 'initiatorName' - The iSCSI initiator that connects to the target.
-- * 'responseStatus' - The response status code.
mkDeleteChapCredentialsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteChapCredentialsResponse
mkDeleteChapCredentialsResponse pResponseStatus_ =
  DeleteChapCredentialsResponse'
    { targetARN = Lude.Nothing,
      initiatorName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrsTargetARN :: Lens.Lens' DeleteChapCredentialsResponse (Lude.Maybe Lude.Text)
dccrsTargetARN = Lens.lens (targetARN :: DeleteChapCredentialsResponse -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: DeleteChapCredentialsResponse)
{-# DEPRECATED dccrsTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The iSCSI initiator that connects to the target.
--
-- /Note:/ Consider using 'initiatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrsInitiatorName :: Lens.Lens' DeleteChapCredentialsResponse (Lude.Maybe Lude.Text)
dccrsInitiatorName = Lens.lens (initiatorName :: DeleteChapCredentialsResponse -> Lude.Maybe Lude.Text) (\s a -> s {initiatorName = a} :: DeleteChapCredentialsResponse)
{-# DEPRECATED dccrsInitiatorName "Use generic-lens or generic-optics with 'initiatorName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrsResponseStatus :: Lens.Lens' DeleteChapCredentialsResponse Lude.Int
dccrsResponseStatus = Lens.lens (responseStatus :: DeleteChapCredentialsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteChapCredentialsResponse)
{-# DEPRECATED dccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
