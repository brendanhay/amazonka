{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetSigningCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This method takes a user pool ID, and returns the signing certificate.
module Network.AWS.CognitoIdentityProvider.GetSigningCertificate
  ( -- * Creating a request
    GetSigningCertificate (..),
    mkGetSigningCertificate,

    -- ** Request lenses
    gscUserPoolId,

    -- * Destructuring the response
    GetSigningCertificateResponse (..),
    mkGetSigningCertificateResponse,

    -- ** Response lenses
    gscrsCertificate,
    gscrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to get a signing certificate from Cognito.
--
-- /See:/ 'mkGetSigningCertificate' smart constructor.
newtype GetSigningCertificate = GetSigningCertificate'
  { -- | The user pool ID.
    userPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSigningCertificate' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID.
mkGetSigningCertificate ::
  -- | 'userPoolId'
  Lude.Text ->
  GetSigningCertificate
mkGetSigningCertificate pUserPoolId_ =
  GetSigningCertificate' {userPoolId = pUserPoolId_}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscUserPoolId :: Lens.Lens' GetSigningCertificate Lude.Text
gscUserPoolId = Lens.lens (userPoolId :: GetSigningCertificate -> Lude.Text) (\s a -> s {userPoolId = a} :: GetSigningCertificate)
{-# DEPRECATED gscUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest GetSigningCertificate where
  type Rs GetSigningCertificate = GetSigningCertificateResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSigningCertificateResponse'
            Lude.<$> (x Lude..?> "Certificate") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSigningCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.GetSigningCertificate" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSigningCertificate where
  toJSON GetSigningCertificate' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("UserPoolId" Lude..= userPoolId)])

instance Lude.ToPath GetSigningCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSigningCertificate where
  toQuery = Lude.const Lude.mempty

-- | Response from Cognito for a signing certificate request.
--
-- /See:/ 'mkGetSigningCertificateResponse' smart constructor.
data GetSigningCertificateResponse = GetSigningCertificateResponse'
  { -- | The signing certificate.
    certificate :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSigningCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificate' - The signing certificate.
-- * 'responseStatus' - The response status code.
mkGetSigningCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSigningCertificateResponse
mkGetSigningCertificateResponse pResponseStatus_ =
  GetSigningCertificateResponse'
    { certificate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The signing certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrsCertificate :: Lens.Lens' GetSigningCertificateResponse (Lude.Maybe Lude.Text)
gscrsCertificate = Lens.lens (certificate :: GetSigningCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificate = a} :: GetSigningCertificateResponse)
{-# DEPRECATED gscrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrsResponseStatus :: Lens.Lens' GetSigningCertificateResponse Lude.Int
gscrsResponseStatus = Lens.lens (responseStatus :: GetSigningCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSigningCertificateResponse)
{-# DEPRECATED gscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
