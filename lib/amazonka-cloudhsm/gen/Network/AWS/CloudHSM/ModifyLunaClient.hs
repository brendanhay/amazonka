{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ModifyLunaClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Modifies the certificate used by the client.
-- This action can potentially start a workflow to install the new certificate on the client's HSMs.
module Network.AWS.CloudHSM.ModifyLunaClient
  ( -- * Creating a request
    ModifyLunaClient (..),
    mkModifyLunaClient,

    -- ** Request lenses
    mlcClientARN,
    mlcCertificate,

    -- * Destructuring the response
    ModifyLunaClientResponse (..),
    mkModifyLunaClientResponse,

    -- ** Response lenses
    mlcrsClientARN,
    mlcrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyLunaClient' smart constructor.
data ModifyLunaClient = ModifyLunaClient'
  { -- | The ARN of the client.
    clientARN :: Lude.Text,
    -- | The new certificate for the client.
    certificate :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyLunaClient' with the minimum fields required to make a request.
--
-- * 'clientARN' - The ARN of the client.
-- * 'certificate' - The new certificate for the client.
mkModifyLunaClient ::
  -- | 'clientARN'
  Lude.Text ->
  -- | 'certificate'
  Lude.Text ->
  ModifyLunaClient
mkModifyLunaClient pClientARN_ pCertificate_ =
  ModifyLunaClient'
    { clientARN = pClientARN_,
      certificate = pCertificate_
    }

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlcClientARN :: Lens.Lens' ModifyLunaClient Lude.Text
mlcClientARN = Lens.lens (clientARN :: ModifyLunaClient -> Lude.Text) (\s a -> s {clientARN = a} :: ModifyLunaClient)
{-# DEPRECATED mlcClientARN "Use generic-lens or generic-optics with 'clientARN' instead." #-}

-- | The new certificate for the client.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlcCertificate :: Lens.Lens' ModifyLunaClient Lude.Text
mlcCertificate = Lens.lens (certificate :: ModifyLunaClient -> Lude.Text) (\s a -> s {certificate = a} :: ModifyLunaClient)
{-# DEPRECATED mlcCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

instance Lude.AWSRequest ModifyLunaClient where
  type Rs ModifyLunaClient = ModifyLunaClientResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          ModifyLunaClientResponse'
            Lude.<$> (x Lude..?> "ClientArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyLunaClient where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.ModifyLunaClient" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyLunaClient where
  toJSON ModifyLunaClient' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClientArn" Lude..= clientARN),
            Lude.Just ("Certificate" Lude..= certificate)
          ]
      )

instance Lude.ToPath ModifyLunaClient where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyLunaClient where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyLunaClientResponse' smart constructor.
data ModifyLunaClientResponse = ModifyLunaClientResponse'
  { -- | The ARN of the client.
    clientARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyLunaClientResponse' with the minimum fields required to make a request.
--
-- * 'clientARN' - The ARN of the client.
-- * 'responseStatus' - The response status code.
mkModifyLunaClientResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyLunaClientResponse
mkModifyLunaClientResponse pResponseStatus_ =
  ModifyLunaClientResponse'
    { clientARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlcrsClientARN :: Lens.Lens' ModifyLunaClientResponse (Lude.Maybe Lude.Text)
mlcrsClientARN = Lens.lens (clientARN :: ModifyLunaClientResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientARN = a} :: ModifyLunaClientResponse)
{-# DEPRECATED mlcrsClientARN "Use generic-lens or generic-optics with 'clientARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlcrsResponseStatus :: Lens.Lens' ModifyLunaClientResponse Lude.Int
mlcrsResponseStatus = Lens.lens (responseStatus :: ModifyLunaClientResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyLunaClientResponse)
{-# DEPRECATED mlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
