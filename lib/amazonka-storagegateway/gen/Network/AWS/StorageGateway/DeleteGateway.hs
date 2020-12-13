{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a gateway. To specify which gateway to delete, use the Amazon Resource Name (ARN) of the gateway in your request. The operation deletes the gateway; however, it does not delete the gateway virtual machine (VM) from your host computer.
--
-- After you delete a gateway, you cannot reactivate it. Completed snapshots of the gateway volumes are not deleted upon deleting the gateway, however, pending snapshots will not complete. After you delete a gateway, your next step is to remove it from your environment.
-- /Important:/ You no longer pay software charges after the gateway is deleted; however, your existing Amazon EBS snapshots persist and you will continue to be billed for these snapshots. You can choose to remove all remaining Amazon EBS snapshots by canceling your Amazon EC2 subscription.  If you prefer not to cancel your Amazon EC2 subscription, you can delete your snapshots using the Amazon EC2 console. For more information, see the <http://aws.amazon.com/storagegateway AWS Storage Gateway detail page> .
module Network.AWS.StorageGateway.DeleteGateway
  ( -- * Creating a request
    DeleteGateway (..),
    mkDeleteGateway,

    -- ** Request lenses
    dgGatewayARN,

    -- * Destructuring the response
    DeleteGatewayResponse (..),
    mkDeleteGatewayResponse,

    -- ** Response lenses
    dgfrsGatewayARN,
    dgfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the ID of the gateway to delete.
--
-- /See:/ 'mkDeleteGateway' smart constructor.
newtype DeleteGateway = DeleteGateway'
  { gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGateway' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
mkDeleteGateway ::
  -- | 'gatewayARN'
  Lude.Text ->
  DeleteGateway
mkDeleteGateway pGatewayARN_ =
  DeleteGateway' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGatewayARN :: Lens.Lens' DeleteGateway Lude.Text
dgGatewayARN = Lens.lens (gatewayARN :: DeleteGateway -> Lude.Text) (\s a -> s {gatewayARN = a} :: DeleteGateway)
{-# DEPRECATED dgGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest DeleteGateway where
  type Rs DeleteGateway = DeleteGatewayResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteGatewayResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteGateway where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.DeleteGateway" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteGateway where
  toJSON DeleteGateway' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath DeleteGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteGateway where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the ID of the deleted gateway.
--
-- /See:/ 'mkDeleteGatewayResponse' smart constructor.
data DeleteGatewayResponse = DeleteGatewayResponse'
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGatewayResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'responseStatus' - The response status code.
mkDeleteGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteGatewayResponse
mkDeleteGatewayResponse pResponseStatus_ =
  DeleteGatewayResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgfrsGatewayARN :: Lens.Lens' DeleteGatewayResponse (Lude.Maybe Lude.Text)
dgfrsGatewayARN = Lens.lens (gatewayARN :: DeleteGatewayResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DeleteGatewayResponse)
{-# DEPRECATED dgfrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgfrsResponseStatus :: Lens.Lens' DeleteGatewayResponse Lude.Int
dgfrsResponseStatus = Lens.lens (responseStatus :: DeleteGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteGatewayResponse)
{-# DEPRECATED dgfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
