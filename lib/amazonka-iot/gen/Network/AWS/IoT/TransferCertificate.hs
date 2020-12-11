{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.TransferCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transfers the specified certificate to the specified AWS account.
--
-- You can cancel the transfer until it is acknowledged by the recipient.
-- No notification is sent to the transfer destination's account. It is up to the caller to notify the transfer target.
-- The certificate being transferred must not be in the ACTIVE state. You can use the UpdateCertificate API to deactivate it.
-- The certificate must not have any policies attached to it. You can use the DetachPrincipalPolicy API to detach them.
module Network.AWS.IoT.TransferCertificate
  ( -- * Creating a request
    TransferCertificate (..),
    mkTransferCertificate,

    -- ** Request lenses
    tcTransferMessage,
    tcCertificateId,
    tcTargetAWSAccount,

    -- * Destructuring the response
    TransferCertificateResponse (..),
    mkTransferCertificateResponse,

    -- ** Response lenses
    tcrsTransferredCertificateARN,
    tcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the TransferCertificate operation.
--
-- /See:/ 'mkTransferCertificate' smart constructor.
data TransferCertificate = TransferCertificate'
  { transferMessage ::
      Lude.Maybe Lude.Text,
    certificateId :: Lude.Text,
    targetAWSAccount :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransferCertificate' with the minimum fields required to make a request.
--
-- * 'certificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
-- * 'targetAWSAccount' - The AWS account.
-- * 'transferMessage' - The transfer message.
mkTransferCertificate ::
  -- | 'certificateId'
  Lude.Text ->
  -- | 'targetAWSAccount'
  Lude.Text ->
  TransferCertificate
mkTransferCertificate pCertificateId_ pTargetAWSAccount_ =
  TransferCertificate'
    { transferMessage = Lude.Nothing,
      certificateId = pCertificateId_,
      targetAWSAccount = pTargetAWSAccount_
    }

-- | The transfer message.
--
-- /Note:/ Consider using 'transferMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTransferMessage :: Lens.Lens' TransferCertificate (Lude.Maybe Lude.Text)
tcTransferMessage = Lens.lens (transferMessage :: TransferCertificate -> Lude.Maybe Lude.Text) (\s a -> s {transferMessage = a} :: TransferCertificate)
{-# DEPRECATED tcTransferMessage "Use generic-lens or generic-optics with 'transferMessage' instead." #-}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcCertificateId :: Lens.Lens' TransferCertificate Lude.Text
tcCertificateId = Lens.lens (certificateId :: TransferCertificate -> Lude.Text) (\s a -> s {certificateId = a} :: TransferCertificate)
{-# DEPRECATED tcCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The AWS account.
--
-- /Note:/ Consider using 'targetAWSAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTargetAWSAccount :: Lens.Lens' TransferCertificate Lude.Text
tcTargetAWSAccount = Lens.lens (targetAWSAccount :: TransferCertificate -> Lude.Text) (\s a -> s {targetAWSAccount = a} :: TransferCertificate)
{-# DEPRECATED tcTargetAWSAccount "Use generic-lens or generic-optics with 'targetAWSAccount' instead." #-}

instance Lude.AWSRequest TransferCertificate where
  type Rs TransferCertificate = TransferCertificateResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          TransferCertificateResponse'
            Lude.<$> (x Lude..?> "transferredCertificateArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TransferCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON TransferCertificate where
  toJSON TransferCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [("transferMessage" Lude..=) Lude.<$> transferMessage]
      )

instance Lude.ToPath TransferCertificate where
  toPath TransferCertificate' {..} =
    Lude.mconcat ["/transfer-certificate/", Lude.toBS certificateId]

instance Lude.ToQuery TransferCertificate where
  toQuery TransferCertificate' {..} =
    Lude.mconcat ["targetAwsAccount" Lude.=: targetAWSAccount]

-- | The output from the TransferCertificate operation.
--
-- /See:/ 'mkTransferCertificateResponse' smart constructor.
data TransferCertificateResponse = TransferCertificateResponse'
  { transferredCertificateARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransferCertificateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transferredCertificateARN' - The ARN of the certificate.
mkTransferCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TransferCertificateResponse
mkTransferCertificateResponse pResponseStatus_ =
  TransferCertificateResponse'
    { transferredCertificateARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the certificate.
--
-- /Note:/ Consider using 'transferredCertificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrsTransferredCertificateARN :: Lens.Lens' TransferCertificateResponse (Lude.Maybe Lude.Text)
tcrsTransferredCertificateARN = Lens.lens (transferredCertificateARN :: TransferCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {transferredCertificateARN = a} :: TransferCertificateResponse)
{-# DEPRECATED tcrsTransferredCertificateARN "Use generic-lens or generic-optics with 'transferredCertificateARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrsResponseStatus :: Lens.Lens' TransferCertificateResponse Lude.Int
tcrsResponseStatus = Lens.lens (responseStatus :: TransferCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TransferCertificateResponse)
{-# DEPRECATED tcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
