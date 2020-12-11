{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.AddListenerCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified SSL server certificate to the certificate list for the specified HTTPS or TLS listener.
--
-- If the certificate in already in the certificate list, the call is successful but the certificate is not added again.
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html HTTPS listeners> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html TLS listeners> in the /Network Load Balancers Guide/ .
module Network.AWS.ELBv2.AddListenerCertificates
  ( -- * Creating a request
    AddListenerCertificates (..),
    mkAddListenerCertificates,

    -- ** Request lenses
    alcListenerARN,
    alcCertificates,

    -- * Destructuring the response
    AddListenerCertificatesResponse (..),
    mkAddListenerCertificatesResponse,

    -- ** Response lenses
    alcrsCertificates,
    alcrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddListenerCertificates' smart constructor.
data AddListenerCertificates = AddListenerCertificates'
  { listenerARN ::
      Lude.Text,
    certificates :: [Certificate]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddListenerCertificates' with the minimum fields required to make a request.
--
-- * 'certificates' - The certificate to add. You can specify one certificate per call. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
-- * 'listenerARN' - The Amazon Resource Name (ARN) of the listener.
mkAddListenerCertificates ::
  -- | 'listenerARN'
  Lude.Text ->
  AddListenerCertificates
mkAddListenerCertificates pListenerARN_ =
  AddListenerCertificates'
    { listenerARN = pListenerARN_,
      certificates = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alcListenerARN :: Lens.Lens' AddListenerCertificates Lude.Text
alcListenerARN = Lens.lens (listenerARN :: AddListenerCertificates -> Lude.Text) (\s a -> s {listenerARN = a} :: AddListenerCertificates)
{-# DEPRECATED alcListenerARN "Use generic-lens or generic-optics with 'listenerARN' instead." #-}

-- | The certificate to add. You can specify one certificate per call. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alcCertificates :: Lens.Lens' AddListenerCertificates [Certificate]
alcCertificates = Lens.lens (certificates :: AddListenerCertificates -> [Certificate]) (\s a -> s {certificates = a} :: AddListenerCertificates)
{-# DEPRECATED alcCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

instance Lude.AWSRequest AddListenerCertificates where
  type Rs AddListenerCertificates = AddListenerCertificatesResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "AddListenerCertificatesResult"
      ( \s h x ->
          AddListenerCertificatesResponse'
            Lude.<$> ( x Lude..@? "Certificates" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddListenerCertificates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AddListenerCertificates where
  toPath = Lude.const "/"

instance Lude.ToQuery AddListenerCertificates where
  toQuery AddListenerCertificates' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AddListenerCertificates" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "ListenerArn" Lude.=: listenerARN,
        "Certificates" Lude.=: Lude.toQueryList "member" certificates
      ]

-- | /See:/ 'mkAddListenerCertificatesResponse' smart constructor.
data AddListenerCertificatesResponse = AddListenerCertificatesResponse'
  { certificates ::
      Lude.Maybe [Certificate],
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

-- | Creates a value of 'AddListenerCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'certificates' - Information about the certificates in the certificate list.
-- * 'responseStatus' - The response status code.
mkAddListenerCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddListenerCertificatesResponse
mkAddListenerCertificatesResponse pResponseStatus_ =
  AddListenerCertificatesResponse'
    { certificates = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the certificates in the certificate list.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alcrsCertificates :: Lens.Lens' AddListenerCertificatesResponse (Lude.Maybe [Certificate])
alcrsCertificates = Lens.lens (certificates :: AddListenerCertificatesResponse -> Lude.Maybe [Certificate]) (\s a -> s {certificates = a} :: AddListenerCertificatesResponse)
{-# DEPRECATED alcrsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alcrsResponseStatus :: Lens.Lens' AddListenerCertificatesResponse Lude.Int
alcrsResponseStatus = Lens.lens (responseStatus :: AddListenerCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddListenerCertificatesResponse)
{-# DEPRECATED alcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
