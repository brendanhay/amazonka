{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.DisassociateCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an association between the Amazon Resource Name (ARN) of an AWS Certificate Manager (ACM) certificate and an AWS Elemental MediaConvert resource.
module Network.AWS.MediaConvert.DisassociateCertificate
  ( -- * Creating a request
    DisassociateCertificate (..),
    mkDisassociateCertificate,

    -- ** Request lenses
    dcARN,

    -- * Destructuring the response
    DisassociateCertificateResponse (..),
    mkDisassociateCertificateResponse,

    -- ** Response lenses
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateCertificate' smart constructor.
newtype DisassociateCertificate = DisassociateCertificate'
  { -- | The ARN of the ACM certificate that you want to disassociate from your MediaConvert resource.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateCertificate' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the ACM certificate that you want to disassociate from your MediaConvert resource.
mkDisassociateCertificate ::
  -- | 'arn'
  Lude.Text ->
  DisassociateCertificate
mkDisassociateCertificate pARN_ =
  DisassociateCertificate' {arn = pARN_}

-- | The ARN of the ACM certificate that you want to disassociate from your MediaConvert resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcARN :: Lens.Lens' DisassociateCertificate Lude.Text
dcARN = Lens.lens (arn :: DisassociateCertificate -> Lude.Text) (\s a -> s {arn = a} :: DisassociateCertificate)
{-# DEPRECATED dcARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest DisassociateCertificate where
  type Rs DisassociateCertificate = DisassociateCertificateResponse
  request = Req.delete mediaConvertService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateCertificateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DisassociateCertificate where
  toPath DisassociateCertificate' {..} =
    Lude.mconcat ["/2017-08-29/certificates/", Lude.toBS arn]

instance Lude.ToQuery DisassociateCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateCertificateResponse' smart constructor.
newtype DisassociateCertificateResponse = DisassociateCertificateResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateCertificateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateCertificateResponse
mkDisassociateCertificateResponse pResponseStatus_ =
  DisassociateCertificateResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DisassociateCertificateResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DisassociateCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateCertificateResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
