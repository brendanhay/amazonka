{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DeleteHAPG
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Deletes a high-availability partition group.
module Network.AWS.CloudHSM.DeleteHAPG
  ( -- * Creating a request
    DeleteHAPG (..),
    mkDeleteHAPG,

    -- ** Request lenses
    dhapgHAPGARN,

    -- * Destructuring the response
    DeleteHAPGResponse (..),
    mkDeleteHAPGResponse,

    -- ** Response lenses
    dhrsStatus,
    dhrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'DeleteHapg' action.
--
-- /See:/ 'mkDeleteHAPG' smart constructor.
newtype DeleteHAPG = DeleteHAPG'
  { -- | The ARN of the high-availability partition group to delete.
    hapgARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHAPG' with the minimum fields required to make a request.
--
-- * 'hapgARN' - The ARN of the high-availability partition group to delete.
mkDeleteHAPG ::
  -- | 'hapgARN'
  Lude.Text ->
  DeleteHAPG
mkDeleteHAPG pHAPGARN_ = DeleteHAPG' {hapgARN = pHAPGARN_}

-- | The ARN of the high-availability partition group to delete.
--
-- /Note:/ Consider using 'hapgARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhapgHAPGARN :: Lens.Lens' DeleteHAPG Lude.Text
dhapgHAPGARN = Lens.lens (hapgARN :: DeleteHAPG -> Lude.Text) (\s a -> s {hapgARN = a} :: DeleteHAPG)
{-# DEPRECATED dhapgHAPGARN "Use generic-lens or generic-optics with 'hapgARN' instead." #-}

instance Lude.AWSRequest DeleteHAPG where
  type Rs DeleteHAPG = DeleteHAPGResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteHAPGResponse'
            Lude.<$> (x Lude..:> "Status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteHAPG where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.DeleteHapg" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteHAPG where
  toJSON DeleteHAPG' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("HapgArn" Lude..= hapgARN)])

instance Lude.ToPath DeleteHAPG where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteHAPG where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of the 'DeleteHapg' action.
--
-- /See:/ 'mkDeleteHAPGResponse' smart constructor.
data DeleteHAPGResponse = DeleteHAPGResponse'
  { -- | The status of the action.
    status :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHAPGResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the action.
-- * 'responseStatus' - The response status code.
mkDeleteHAPGResponse ::
  -- | 'status'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteHAPGResponse
mkDeleteHAPGResponse pStatus_ pResponseStatus_ =
  DeleteHAPGResponse'
    { status = pStatus_,
      responseStatus = pResponseStatus_
    }

-- | The status of the action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrsStatus :: Lens.Lens' DeleteHAPGResponse Lude.Text
dhrsStatus = Lens.lens (status :: DeleteHAPGResponse -> Lude.Text) (\s a -> s {status = a} :: DeleteHAPGResponse)
{-# DEPRECATED dhrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrsResponseStatus :: Lens.Lens' DeleteHAPGResponse Lude.Int
dhrsResponseStatus = Lens.lens (responseStatus :: DeleteHAPGResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteHAPGResponse)
{-# DEPRECATED dhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
