{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DeleteHSM
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Deletes an HSM. After completion, this operation cannot be undone and your key material cannot be recovered.
module Network.AWS.CloudHSM.DeleteHSM
  ( -- * Creating a request
    DeleteHSM (..),
    mkDeleteHSM,

    -- ** Request lenses
    dhHSMARN,

    -- * Destructuring the response
    DeleteHSMResponse (..),
    mkDeleteHSMResponse,

    -- ** Response lenses
    dhsmrsStatus,
    dhsmrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'DeleteHsm' operation.
--
-- /See:/ 'mkDeleteHSM' smart constructor.
newtype DeleteHSM = DeleteHSM'
  { -- | The ARN of the HSM to delete.
    hsmARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHSM' with the minimum fields required to make a request.
--
-- * 'hsmARN' - The ARN of the HSM to delete.
mkDeleteHSM ::
  -- | 'hsmARN'
  Lude.Text ->
  DeleteHSM
mkDeleteHSM pHSMARN_ = DeleteHSM' {hsmARN = pHSMARN_}

-- | The ARN of the HSM to delete.
--
-- /Note:/ Consider using 'hsmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhHSMARN :: Lens.Lens' DeleteHSM Lude.Text
dhHSMARN = Lens.lens (hsmARN :: DeleteHSM -> Lude.Text) (\s a -> s {hsmARN = a} :: DeleteHSM)
{-# DEPRECATED dhHSMARN "Use generic-lens or generic-optics with 'hsmARN' instead." #-}

instance Lude.AWSRequest DeleteHSM where
  type Rs DeleteHSM = DeleteHSMResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteHSMResponse'
            Lude.<$> (x Lude..:> "Status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteHSM where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.DeleteHsm" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteHSM where
  toJSON DeleteHSM' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("HsmArn" Lude..= hsmARN)])

instance Lude.ToPath DeleteHSM where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteHSM where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of the 'DeleteHsm' operation.
--
-- /See:/ 'mkDeleteHSMResponse' smart constructor.
data DeleteHSMResponse = DeleteHSMResponse'
  { -- | The status of the operation.
    status :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHSMResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the operation.
-- * 'responseStatus' - The response status code.
mkDeleteHSMResponse ::
  -- | 'status'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteHSMResponse
mkDeleteHSMResponse pStatus_ pResponseStatus_ =
  DeleteHSMResponse'
    { status = pStatus_,
      responseStatus = pResponseStatus_
    }

-- | The status of the operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhsmrsStatus :: Lens.Lens' DeleteHSMResponse Lude.Text
dhsmrsStatus = Lens.lens (status :: DeleteHSMResponse -> Lude.Text) (\s a -> s {status = a} :: DeleteHSMResponse)
{-# DEPRECATED dhsmrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhsmrsResponseStatus :: Lens.Lens' DeleteHSMResponse Lude.Int
dhsmrsResponseStatus = Lens.lens (responseStatus :: DeleteHSMResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteHSMResponse)
{-# DEPRECATED dhsmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
