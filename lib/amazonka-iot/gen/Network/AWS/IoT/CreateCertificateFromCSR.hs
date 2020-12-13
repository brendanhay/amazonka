{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateCertificateFromCSR
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an X.509 certificate using the specified certificate signing request.
--
-- __Note:__ The CSR must include a public key that is either an RSA key with a length of at least 2048 bits or an ECC key from NIST P-256 or NIST P-384 curves.
-- __Note:__ Reusing the same certificate signing request (CSR) results in a distinct certificate.
-- You can create multiple certificates in a batch by creating a directory, copying multiple .csr files into that directory, and then specifying that directory on the command line. The following commands show how to create a batch of certificates given a batch of CSRs.
-- Assuming a set of CSRs are located inside of the directory my-csr-directory:
-- On Linux and OS X, the command is:
-- >  ls my-csr-directory/ | xargs -I {} aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/{}
-- This command lists all of the CSRs in my-csr-directory and pipes each CSR file name to the aws iot create-certificate-from-csr AWS CLI command to create a certificate for the corresponding CSR.
-- The aws iot create-certificate-from-csr part of the command can also be run in parallel to speed up the certificate creation process:
-- >  ls my-csr-directory/ | xargs -P 10 -I {} aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/{}
-- On Windows PowerShell, the command to create certificates for all CSRs in my-csr-directory is:
-- > ls -Name my-csr-directory | %{aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/$_}
-- On a Windows command prompt, the command to create certificates for all CSRs in my-csr-directory is:
-- > forfiles /p my-csr-directory /c "cmd /c aws iot create-certificate-from-csr --certificate-signing-request file://@path"
module Network.AWS.IoT.CreateCertificateFromCSR
  ( -- * Creating a request
    CreateCertificateFromCSR (..),
    mkCreateCertificateFromCSR,

    -- ** Request lenses
    ccfcsrSetAsActive,
    ccfcsrCertificateSigningRequest,

    -- * Destructuring the response
    CreateCertificateFromCSRResponse (..),
    mkCreateCertificateFromCSRResponse,

    -- ** Response lenses
    ccfcsrrsCertificatePem,
    ccfcsrrsCertificateARN,
    ccfcsrrsCertificateId,
    ccfcsrrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the CreateCertificateFromCsr operation.
--
-- /See:/ 'mkCreateCertificateFromCSR' smart constructor.
data CreateCertificateFromCSR = CreateCertificateFromCSR'
  { -- | Specifies whether the certificate is active.
    setAsActive :: Lude.Maybe Lude.Bool,
    -- | The certificate signing request (CSR).
    certificateSigningRequest :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCertificateFromCSR' with the minimum fields required to make a request.
--
-- * 'setAsActive' - Specifies whether the certificate is active.
-- * 'certificateSigningRequest' - The certificate signing request (CSR).
mkCreateCertificateFromCSR ::
  -- | 'certificateSigningRequest'
  Lude.Text ->
  CreateCertificateFromCSR
mkCreateCertificateFromCSR pCertificateSigningRequest_ =
  CreateCertificateFromCSR'
    { setAsActive = Lude.Nothing,
      certificateSigningRequest = pCertificateSigningRequest_
    }

-- | Specifies whether the certificate is active.
--
-- /Note:/ Consider using 'setAsActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrSetAsActive :: Lens.Lens' CreateCertificateFromCSR (Lude.Maybe Lude.Bool)
ccfcsrSetAsActive = Lens.lens (setAsActive :: CreateCertificateFromCSR -> Lude.Maybe Lude.Bool) (\s a -> s {setAsActive = a} :: CreateCertificateFromCSR)
{-# DEPRECATED ccfcsrSetAsActive "Use generic-lens or generic-optics with 'setAsActive' instead." #-}

-- | The certificate signing request (CSR).
--
-- /Note:/ Consider using 'certificateSigningRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrCertificateSigningRequest :: Lens.Lens' CreateCertificateFromCSR Lude.Text
ccfcsrCertificateSigningRequest = Lens.lens (certificateSigningRequest :: CreateCertificateFromCSR -> Lude.Text) (\s a -> s {certificateSigningRequest = a} :: CreateCertificateFromCSR)
{-# DEPRECATED ccfcsrCertificateSigningRequest "Use generic-lens or generic-optics with 'certificateSigningRequest' instead." #-}

instance Lude.AWSRequest CreateCertificateFromCSR where
  type Rs CreateCertificateFromCSR = CreateCertificateFromCSRResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCertificateFromCSRResponse'
            Lude.<$> (x Lude..?> "certificatePem")
            Lude.<*> (x Lude..?> "certificateArn")
            Lude.<*> (x Lude..?> "certificateId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCertificateFromCSR where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateCertificateFromCSR where
  toJSON CreateCertificateFromCSR' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("certificateSigningRequest" Lude..= certificateSigningRequest)
          ]
      )

instance Lude.ToPath CreateCertificateFromCSR where
  toPath = Lude.const "/certificates"

instance Lude.ToQuery CreateCertificateFromCSR where
  toQuery CreateCertificateFromCSR' {..} =
    Lude.mconcat ["setAsActive" Lude.=: setAsActive]

-- | The output from the CreateCertificateFromCsr operation.
--
-- /See:/ 'mkCreateCertificateFromCSRResponse' smart constructor.
data CreateCertificateFromCSRResponse = CreateCertificateFromCSRResponse'
  { -- | The certificate data, in PEM format.
    certificatePem :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the certificate. You can use the ARN as a principal for policy operations.
    certificateARN :: Lude.Maybe Lude.Text,
    -- | The ID of the certificate. Certificate management operations only take a certificateId.
    certificateId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCertificateFromCSRResponse' with the minimum fields required to make a request.
--
-- * 'certificatePem' - The certificate data, in PEM format.
-- * 'certificateARN' - The Amazon Resource Name (ARN) of the certificate. You can use the ARN as a principal for policy operations.
-- * 'certificateId' - The ID of the certificate. Certificate management operations only take a certificateId.
-- * 'responseStatus' - The response status code.
mkCreateCertificateFromCSRResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCertificateFromCSRResponse
mkCreateCertificateFromCSRResponse pResponseStatus_ =
  CreateCertificateFromCSRResponse'
    { certificatePem = Lude.Nothing,
      certificateARN = Lude.Nothing,
      certificateId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrrsCertificatePem :: Lens.Lens' CreateCertificateFromCSRResponse (Lude.Maybe Lude.Text)
ccfcsrrsCertificatePem = Lens.lens (certificatePem :: CreateCertificateFromCSRResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificatePem = a} :: CreateCertificateFromCSRResponse)
{-# DEPRECATED ccfcsrrsCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | The Amazon Resource Name (ARN) of the certificate. You can use the ARN as a principal for policy operations.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrrsCertificateARN :: Lens.Lens' CreateCertificateFromCSRResponse (Lude.Maybe Lude.Text)
ccfcsrrsCertificateARN = Lens.lens (certificateARN :: CreateCertificateFromCSRResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CreateCertificateFromCSRResponse)
{-# DEPRECATED ccfcsrrsCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The ID of the certificate. Certificate management operations only take a certificateId.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrrsCertificateId :: Lens.Lens' CreateCertificateFromCSRResponse (Lude.Maybe Lude.Text)
ccfcsrrsCertificateId = Lens.lens (certificateId :: CreateCertificateFromCSRResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: CreateCertificateFromCSRResponse)
{-# DEPRECATED ccfcsrrsCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrrsResponseStatus :: Lens.Lens' CreateCertificateFromCSRResponse Lude.Int
ccfcsrrsResponseStatus = Lens.lens (responseStatus :: CreateCertificateFromCSRResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCertificateFromCSRResponse)
{-# DEPRECATED ccfcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
