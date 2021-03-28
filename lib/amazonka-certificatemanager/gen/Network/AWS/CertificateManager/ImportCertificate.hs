{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.ImportCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a certificate into AWS Certificate Manager (ACM) to use with services that are integrated with ACM. Note that <https://docs.aws.amazon.com/acm/latest/userguide/acm-services.html integrated services> allow only certificate types and keys they support to be associated with their resources. Further, their support differs depending on whether the certificate is imported into IAM or into ACM. For more information, see the documentation for each service. For more information about importing certificates into ACM, see <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates> in the /AWS Certificate Manager User Guide/ . 
--
-- Note the following guidelines when importing third party certificates:
--
--     * You must enter the private key that matches the certificate you are importing.
--
--
--     * The private key must be unencrypted. You cannot import a private key that is protected by a password or a passphrase.
--
--
--     * If the certificate you are importing is not self-signed, you must enter its certificate chain.
--
--
--     * If a certificate chain is included, the issuer must be the subject of one of the certificates in the chain.
--
--
--     * The certificate, private key, and certificate chain must be PEM-encoded.
--
--
--     * The current time must be between the @Not Before@ and @Not After@ certificate fields.
--
--
--     * The @Issuer@ field must not be empty.
--
--
--     * The OCSP authority URL, if present, must not exceed 1000 characters.
--
--
--     * To import a new certificate, omit the @CertificateArn@ argument. Include this argument only when you want to replace a previously imported certifica
--
--
--     * When you import a certificate by using the CLI, you must specify the certificate, the certificate chain, and the private key by their file names preceded by @file://@ . For example, you can specify a certificate saved in the @C:\temp@ folder as @file://C:\temp\certificate_to_import.pem@ . If you are making an HTTP or HTTPS Query request, include these arguments as BLOBs. 
--
--
--     * When you import a certificate by using an SDK, you must specify the certificate, the certificate chain, and the private key files in the manner required by the programming language you're using. 
--
--
--     * The cryptographic algorithm of an imported certificate must match the algorithm of the signing CA. For example, if the signing CA key type is RSA, then the certificate key type must also be RSA.
--
--
-- This operation returns the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of the imported certificate.
module Network.AWS.CertificateManager.ImportCertificate
    (
    -- * Creating a request
      ImportCertificate (..)
    , mkImportCertificate
    -- ** Request lenses
    , icCertificate
    , icPrivateKey
    , icCertificateArn
    , icCertificateChain
    , icTags

    -- * Destructuring the response
    , ImportCertificateResponse (..)
    , mkImportCertificateResponse
    -- ** Response lenses
    , icrrsCertificateArn
    , icrrsResponseStatus
    ) where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportCertificate' smart constructor.
data ImportCertificate = ImportCertificate'
  { certificate :: Core.Base64
    -- ^ The certificate to import.
  , privateKey :: Core.Sensitive Core.Base64
    -- ^ The private key that matches the public key in the certificate.
  , certificateArn :: Core.Maybe Types.Arn
    -- ^ The <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an imported certificate to replace. To import a new certificate, omit this field. 
  , certificateChain :: Core.Maybe Core.Base64
    -- ^ The PEM encoded certificate chain.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ One or more resource tags to associate with the imported certificate. 
--
-- Note: You cannot apply tags when reimporting a certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportCertificate' value with any optional fields omitted.
mkImportCertificate
    :: Core.Base64 -- ^ 'certificate'
    -> Core.Sensitive Core.Base64 -- ^ 'privateKey'
    -> ImportCertificate
mkImportCertificate certificate privateKey
  = ImportCertificate'{certificate, privateKey,
                       certificateArn = Core.Nothing, certificateChain = Core.Nothing,
                       tags = Core.Nothing}

-- | The certificate to import.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificate :: Lens.Lens' ImportCertificate Core.Base64
icCertificate = Lens.field @"certificate"
{-# INLINEABLE icCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | The private key that matches the public key in the certificate.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icPrivateKey :: Lens.Lens' ImportCertificate (Core.Sensitive Core.Base64)
icPrivateKey = Lens.field @"privateKey"
{-# INLINEABLE icPrivateKey #-}
{-# DEPRECATED privateKey "Use generic-lens or generic-optics with 'privateKey' instead"  #-}

-- | The <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an imported certificate to replace. To import a new certificate, omit this field. 
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificateArn :: Lens.Lens' ImportCertificate (Core.Maybe Types.Arn)
icCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE icCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The PEM encoded certificate chain.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificateChain :: Lens.Lens' ImportCertificate (Core.Maybe Core.Base64)
icCertificateChain = Lens.field @"certificateChain"
{-# INLINEABLE icCertificateChain #-}
{-# DEPRECATED certificateChain "Use generic-lens or generic-optics with 'certificateChain' instead"  #-}

-- | One or more resource tags to associate with the imported certificate. 
--
-- Note: You cannot apply tags when reimporting a certificate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTags :: Lens.Lens' ImportCertificate (Core.Maybe (Core.NonEmpty Types.Tag))
icTags = Lens.field @"tags"
{-# INLINEABLE icTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery ImportCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ImportCertificate where
        toHeaders ImportCertificate{..}
          = Core.pure
              ("X-Amz-Target", "CertificateManager.ImportCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ImportCertificate where
        toJSON ImportCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Certificate" Core..= certificate),
                  Core.Just ("PrivateKey" Core..= privateKey),
                  ("CertificateArn" Core..=) Core.<$> certificateArn,
                  ("CertificateChain" Core..=) Core.<$> certificateChain,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest ImportCertificate where
        type Rs ImportCertificate = ImportCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ImportCertificateResponse' Core.<$>
                   (x Core..:? "CertificateArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportCertificateResponse' smart constructor.
data ImportCertificateResponse = ImportCertificateResponse'
  { certificateArn :: Core.Maybe Types.Arn
    -- ^ The <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of the imported certificate.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportCertificateResponse' value with any optional fields omitted.
mkImportCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportCertificateResponse
mkImportCertificateResponse responseStatus
  = ImportCertificateResponse'{certificateArn = Core.Nothing,
                               responseStatus}

-- | The <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of the imported certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrrsCertificateArn :: Lens.Lens' ImportCertificateResponse (Core.Maybe Types.Arn)
icrrsCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE icrrsCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrrsResponseStatus :: Lens.Lens' ImportCertificateResponse Core.Int
icrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE icrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
