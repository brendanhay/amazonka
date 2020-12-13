{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.AssociateCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an AWS Certificate Manager (ACM) Amazon Resource Name (ARN) with AWS Elemental MediaConvert.
module Network.AWS.MediaConvert.AssociateCertificate
  ( -- * Creating a request
    AssociateCertificate (..),
    mkAssociateCertificate,

    -- ** Request lenses
    acARN,

    -- * Destructuring the response
    AssociateCertificateResponse (..),
    mkAssociateCertificateResponse,

    -- ** Response lenses
    acrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateCertificate' smart constructor.
newtype AssociateCertificate = AssociateCertificate'
  { -- | The ARN of the ACM certificate that you want to associate with your MediaConvert resource.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateCertificate' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the ACM certificate that you want to associate with your MediaConvert resource.
mkAssociateCertificate ::
  -- | 'arn'
  Lude.Text ->
  AssociateCertificate
mkAssociateCertificate pARN_ = AssociateCertificate' {arn = pARN_}

-- | The ARN of the ACM certificate that you want to associate with your MediaConvert resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acARN :: Lens.Lens' AssociateCertificate Lude.Text
acARN = Lens.lens (arn :: AssociateCertificate -> Lude.Text) (\s a -> s {arn = a} :: AssociateCertificate)
{-# DEPRECATED acARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest AssociateCertificate where
  type Rs AssociateCertificate = AssociateCertificateResponse
  request = Req.postJSON mediaConvertService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateCertificateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateCertificate where
  toJSON AssociateCertificate' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath AssociateCertificate where
  toPath = Lude.const "/2017-08-29/certificates"

instance Lude.ToQuery AssociateCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateCertificateResponse' smart constructor.
newtype AssociateCertificateResponse = AssociateCertificateResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateCertificateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateCertificateResponse
mkAssociateCertificateResponse pResponseStatus_ =
  AssociateCertificateResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrsResponseStatus :: Lens.Lens' AssociateCertificateResponse Lude.Int
acrsResponseStatus = Lens.lens (responseStatus :: AssociateCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateCertificateResponse)
{-# DEPRECATED acrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
