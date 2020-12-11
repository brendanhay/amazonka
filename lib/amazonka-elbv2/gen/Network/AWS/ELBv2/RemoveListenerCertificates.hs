{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.RemoveListenerCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified certificate from the certificate list for the specified HTTPS or TLS listener.
module Network.AWS.ELBv2.RemoveListenerCertificates
  ( -- * Creating a request
    RemoveListenerCertificates (..),
    mkRemoveListenerCertificates,

    -- ** Request lenses
    rlcListenerARN,
    rlcCertificates,

    -- * Destructuring the response
    RemoveListenerCertificatesResponse (..),
    mkRemoveListenerCertificatesResponse,

    -- ** Response lenses
    rlcrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveListenerCertificates' smart constructor.
data RemoveListenerCertificates = RemoveListenerCertificates'
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

-- | Creates a value of 'RemoveListenerCertificates' with the minimum fields required to make a request.
--
-- * 'certificates' - The certificate to remove. You can specify one certificate per call. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
-- * 'listenerARN' - The Amazon Resource Name (ARN) of the listener.
mkRemoveListenerCertificates ::
  -- | 'listenerARN'
  Lude.Text ->
  RemoveListenerCertificates
mkRemoveListenerCertificates pListenerARN_ =
  RemoveListenerCertificates'
    { listenerARN = pListenerARN_,
      certificates = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcListenerARN :: Lens.Lens' RemoveListenerCertificates Lude.Text
rlcListenerARN = Lens.lens (listenerARN :: RemoveListenerCertificates -> Lude.Text) (\s a -> s {listenerARN = a} :: RemoveListenerCertificates)
{-# DEPRECATED rlcListenerARN "Use generic-lens or generic-optics with 'listenerARN' instead." #-}

-- | The certificate to remove. You can specify one certificate per call. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcCertificates :: Lens.Lens' RemoveListenerCertificates [Certificate]
rlcCertificates = Lens.lens (certificates :: RemoveListenerCertificates -> [Certificate]) (\s a -> s {certificates = a} :: RemoveListenerCertificates)
{-# DEPRECATED rlcCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

instance Lude.AWSRequest RemoveListenerCertificates where
  type
    Rs RemoveListenerCertificates =
      RemoveListenerCertificatesResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "RemoveListenerCertificatesResult"
      ( \s h x ->
          RemoveListenerCertificatesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveListenerCertificates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemoveListenerCertificates where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveListenerCertificates where
  toQuery RemoveListenerCertificates' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RemoveListenerCertificates" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "ListenerArn" Lude.=: listenerARN,
        "Certificates" Lude.=: Lude.toQueryList "member" certificates
      ]

-- | /See:/ 'mkRemoveListenerCertificatesResponse' smart constructor.
newtype RemoveListenerCertificatesResponse = RemoveListenerCertificatesResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveListenerCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRemoveListenerCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveListenerCertificatesResponse
mkRemoveListenerCertificatesResponse pResponseStatus_ =
  RemoveListenerCertificatesResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcrsResponseStatus :: Lens.Lens' RemoveListenerCertificatesResponse Lude.Int
rlcrsResponseStatus = Lens.lens (responseStatus :: RemoveListenerCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveListenerCertificatesResponse)
{-# DEPRECATED rlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
