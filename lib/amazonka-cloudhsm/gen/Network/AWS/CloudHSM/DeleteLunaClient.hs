{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DeleteLunaClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Deletes a client.
module Network.AWS.CloudHSM.DeleteLunaClient
  ( -- * Creating a request
    DeleteLunaClient (..),
    mkDeleteLunaClient,

    -- ** Request lenses
    dlcClientARN,

    -- * Destructuring the response
    DeleteLunaClientResponse (..),
    mkDeleteLunaClientResponse,

    -- ** Response lenses
    dlcrsStatus,
    dlcrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLunaClient' smart constructor.
newtype DeleteLunaClient = DeleteLunaClient'
  { -- | The ARN of the client to delete.
    clientARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLunaClient' with the minimum fields required to make a request.
--
-- * 'clientARN' - The ARN of the client to delete.
mkDeleteLunaClient ::
  -- | 'clientARN'
  Lude.Text ->
  DeleteLunaClient
mkDeleteLunaClient pClientARN_ =
  DeleteLunaClient' {clientARN = pClientARN_}

-- | The ARN of the client to delete.
--
-- /Note:/ Consider using 'clientARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcClientARN :: Lens.Lens' DeleteLunaClient Lude.Text
dlcClientARN = Lens.lens (clientARN :: DeleteLunaClient -> Lude.Text) (\s a -> s {clientARN = a} :: DeleteLunaClient)
{-# DEPRECATED dlcClientARN "Use generic-lens or generic-optics with 'clientARN' instead." #-}

instance Lude.AWSRequest DeleteLunaClient where
  type Rs DeleteLunaClient = DeleteLunaClientResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteLunaClientResponse'
            Lude.<$> (x Lude..:> "Status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLunaClient where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.DeleteLunaClient" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteLunaClient where
  toJSON DeleteLunaClient' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ClientArn" Lude..= clientARN)])

instance Lude.ToPath DeleteLunaClient where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLunaClient where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteLunaClientResponse' smart constructor.
data DeleteLunaClientResponse = DeleteLunaClientResponse'
  { -- | The status of the action.
    status :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLunaClientResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the action.
-- * 'responseStatus' - The response status code.
mkDeleteLunaClientResponse ::
  -- | 'status'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLunaClientResponse
mkDeleteLunaClientResponse pStatus_ pResponseStatus_ =
  DeleteLunaClientResponse'
    { status = pStatus_,
      responseStatus = pResponseStatus_
    }

-- | The status of the action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrsStatus :: Lens.Lens' DeleteLunaClientResponse Lude.Text
dlcrsStatus = Lens.lens (status :: DeleteLunaClientResponse -> Lude.Text) (\s a -> s {status = a} :: DeleteLunaClientResponse)
{-# DEPRECATED dlcrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrsResponseStatus :: Lens.Lens' DeleteLunaClientResponse Lude.Int
dlcrsResponseStatus = Lens.lens (responseStatus :: DeleteLunaClientResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLunaClientResponse)
{-# DEPRECATED dlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
