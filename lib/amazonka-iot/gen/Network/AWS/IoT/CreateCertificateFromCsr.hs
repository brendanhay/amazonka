{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateCertificateFromCsr
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
module Network.AWS.IoT.CreateCertificateFromCsr
    (
    -- * Creating a request
      CreateCertificateFromCsr (..)
    , mkCreateCertificateFromCsr
    -- ** Request lenses
    , ccfcCertificateSigningRequest
    , ccfcSetAsActive

    -- * Destructuring the response
    , CreateCertificateFromCsrResponse (..)
    , mkCreateCertificateFromCsrResponse
    -- ** Response lenses
    , ccfcrrsCertificateArn
    , ccfcrrsCertificateId
    , ccfcrrsCertificatePem
    , ccfcrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreateCertificateFromCsr operation.
--
-- /See:/ 'mkCreateCertificateFromCsr' smart constructor.
data CreateCertificateFromCsr = CreateCertificateFromCsr'
  { certificateSigningRequest :: Types.CertificateSigningRequest
    -- ^ The certificate signing request (CSR).
  , setAsActive :: Core.Maybe Core.Bool
    -- ^ Specifies whether the certificate is active.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCertificateFromCsr' value with any optional fields omitted.
mkCreateCertificateFromCsr
    :: Types.CertificateSigningRequest -- ^ 'certificateSigningRequest'
    -> CreateCertificateFromCsr
mkCreateCertificateFromCsr certificateSigningRequest
  = CreateCertificateFromCsr'{certificateSigningRequest,
                              setAsActive = Core.Nothing}

-- | The certificate signing request (CSR).
--
-- /Note:/ Consider using 'certificateSigningRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcCertificateSigningRequest :: Lens.Lens' CreateCertificateFromCsr Types.CertificateSigningRequest
ccfcCertificateSigningRequest = Lens.field @"certificateSigningRequest"
{-# INLINEABLE ccfcCertificateSigningRequest #-}
{-# DEPRECATED certificateSigningRequest "Use generic-lens or generic-optics with 'certificateSigningRequest' instead"  #-}

-- | Specifies whether the certificate is active.
--
-- /Note:/ Consider using 'setAsActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcSetAsActive :: Lens.Lens' CreateCertificateFromCsr (Core.Maybe Core.Bool)
ccfcSetAsActive = Lens.field @"setAsActive"
{-# INLINEABLE ccfcSetAsActive #-}
{-# DEPRECATED setAsActive "Use generic-lens or generic-optics with 'setAsActive' instead"  #-}

instance Core.ToQuery CreateCertificateFromCsr where
        toQuery CreateCertificateFromCsr{..}
          = Core.maybe Core.mempty (Core.toQueryPair "setAsActive")
              setAsActive

instance Core.ToHeaders CreateCertificateFromCsr where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateCertificateFromCsr where
        toJSON CreateCertificateFromCsr{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("certificateSigningRequest" Core..= certificateSigningRequest)])

instance Core.AWSRequest CreateCertificateFromCsr where
        type Rs CreateCertificateFromCsr = CreateCertificateFromCsrResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/certificates",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCertificateFromCsrResponse' Core.<$>
                   (x Core..:? "certificateArn") Core.<*> x Core..:? "certificateId"
                     Core.<*> x Core..:? "certificatePem"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output from the CreateCertificateFromCsr operation.
--
-- /See:/ 'mkCreateCertificateFromCsrResponse' smart constructor.
data CreateCertificateFromCsrResponse = CreateCertificateFromCsrResponse'
  { certificateArn :: Core.Maybe Types.CertificateArn
    -- ^ The Amazon Resource Name (ARN) of the certificate. You can use the ARN as a principal for policy operations.
  , certificateId :: Core.Maybe Types.CertificateId
    -- ^ The ID of the certificate. Certificate management operations only take a certificateId.
  , certificatePem :: Core.Maybe Types.CertificatePem
    -- ^ The certificate data, in PEM format.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCertificateFromCsrResponse' value with any optional fields omitted.
mkCreateCertificateFromCsrResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCertificateFromCsrResponse
mkCreateCertificateFromCsrResponse responseStatus
  = CreateCertificateFromCsrResponse'{certificateArn = Core.Nothing,
                                      certificateId = Core.Nothing, certificatePem = Core.Nothing,
                                      responseStatus}

-- | The Amazon Resource Name (ARN) of the certificate. You can use the ARN as a principal for policy operations.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcrrsCertificateArn :: Lens.Lens' CreateCertificateFromCsrResponse (Core.Maybe Types.CertificateArn)
ccfcrrsCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE ccfcrrsCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The ID of the certificate. Certificate management operations only take a certificateId.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcrrsCertificateId :: Lens.Lens' CreateCertificateFromCsrResponse (Core.Maybe Types.CertificateId)
ccfcrrsCertificateId = Lens.field @"certificateId"
{-# INLINEABLE ccfcrrsCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | The certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcrrsCertificatePem :: Lens.Lens' CreateCertificateFromCsrResponse (Core.Maybe Types.CertificatePem)
ccfcrrsCertificatePem = Lens.field @"certificatePem"
{-# INLINEABLE ccfcrrsCertificatePem #-}
{-# DEPRECATED certificatePem "Use generic-lens or generic-optics with 'certificatePem' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcrrsResponseStatus :: Lens.Lens' CreateCertificateFromCsrResponse Core.Int
ccfcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccfcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
