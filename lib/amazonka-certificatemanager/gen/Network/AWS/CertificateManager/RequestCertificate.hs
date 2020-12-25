{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.RequestCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests an ACM certificate for use with other AWS services. To request an ACM certificate, you must specify a fully qualified domain name (FQDN) in the @DomainName@ parameter. You can also specify additional FQDNs in the @SubjectAlternativeNames@ parameter.
--
-- If you are requesting a private certificate, domain validation is not required. If you are requesting a public certificate, each domain name that you specify must be validated to verify that you own or control the domain. You can use <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html DNS validation> or <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-email.html email validation> . We recommend that you use DNS validation. ACM issues public certificates after receiving approval from the domain owner.
module Network.AWS.CertificateManager.RequestCertificate
  ( -- * Creating a request
    RequestCertificate (..),
    mkRequestCertificate,

    -- ** Request lenses
    rcDomainName,
    rcCertificateAuthorityArn,
    rcDomainValidationOptions,
    rcIdempotencyToken,
    rcOptions,
    rcSubjectAlternativeNames,
    rcTags,
    rcValidationMethod,

    -- * Destructuring the response
    RequestCertificateResponse (..),
    mkRequestCertificateResponse,

    -- ** Response lenses
    rcrrsCertificateArn,
    rcrrsResponseStatus,
  )
where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRequestCertificate' smart constructor.
data RequestCertificate = RequestCertificate'
  { -- | Fully qualified domain name (FQDN), such as www.example.com, that you want to secure with an ACM certificate. Use an asterisk (*) to create a wildcard certificate that protects several sites in the same domain. For example, *.example.com protects www.example.com, site.example.com, and images.example.com.
    --
    -- The first domain name you enter cannot exceed 64 octets, including periods. Each subsequent Subject Alternative Name (SAN), however, can be up to 253 octets in length.
    domainName :: Types.DomainName,
    -- | The Amazon Resource Name (ARN) of the private certificate authority (CA) that will be used to issue the certificate. If you do not provide an ARN and you are trying to request a private certificate, ACM will attempt to issue a public certificate. For more information about private CAs, see the <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaWelcome.html AWS Certificate Manager Private Certificate Authority (PCA)> user guide. The ARN must have the following form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@
    certificateAuthorityArn :: Core.Maybe Types.CertificateAuthorityArn,
    -- | The domain name that you want ACM to use to send you emails so that you can validate domain ownership.
    domainValidationOptions :: Core.Maybe (Core.NonEmpty Types.DomainValidationOption),
    -- | Customer chosen string that can be used to distinguish between calls to @RequestCertificate@ . Idempotency tokens time out after one hour. Therefore, if you call @RequestCertificate@ multiple times with the same idempotency token within one hour, ACM recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, ACM recognizes that you are requesting multiple certificates.
    idempotencyToken :: Core.Maybe Types.IdempotencyToken,
    -- | Currently, you can use this parameter to specify whether to add the certificate to a certificate transparency log. Certificate transparency makes it possible to detect SSL/TLS certificates that have been mistakenly or maliciously issued. Certificates that have not been logged typically produce an error message in a browser. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-bestpractices.html#best-practices-transparency Opting Out of Certificate Transparency Logging> .
    options :: Core.Maybe Types.CertificateOptions,
    -- | Additional FQDNs to be included in the Subject Alternative Name extension of the ACM certificate. For example, add the name www.example.net to a certificate for which the @DomainName@ field is www.example.com if users can reach your site by using either name. The maximum number of domain names that you can add to an ACM certificate is 100. However, the initial quota is 10 domain names. If you need more than 10 names, you must request a quota increase. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Quotas> .
    --
    -- The maximum length of a SAN DNS name is 253 octets. The name is made up of multiple labels separated by periods. No label can be longer than 63 octets. Consider the following examples:
    --
    --     * @(63 octets).(63 octets).(63 octets).(61 octets)@ is legal because the total length is 253 octets (63+1+63+1+63+1+61) and no label exceeds 63 octets.
    --
    --
    --     * @(64 octets).(63 octets).(63 octets).(61 octets)@ is not legal because the total length exceeds 253 octets (64+1+63+1+63+1+61) and the first label exceeds 63 octets.
    --
    --
    --     * @(63 octets).(63 octets).(63 octets).(62 octets)@ is not legal because the total length of the DNS name (63+1+63+1+63+1+62) exceeds 253 octets.
    subjectAlternativeNames :: Core.Maybe (Core.NonEmpty Types.DomainNameString),
    -- | One or more resource tags to associate with the certificate.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag),
    -- | The method you want to use if you are requesting a public certificate to validate that you own or control domain. You can <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html validate with DNS> or <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-email.html validate with email> . We recommend that you use DNS validation.
    validationMethod :: Core.Maybe Types.ValidationMethod
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequestCertificate' value with any optional fields omitted.
mkRequestCertificate ::
  -- | 'domainName'
  Types.DomainName ->
  RequestCertificate
mkRequestCertificate domainName =
  RequestCertificate'
    { domainName,
      certificateAuthorityArn = Core.Nothing,
      domainValidationOptions = Core.Nothing,
      idempotencyToken = Core.Nothing,
      options = Core.Nothing,
      subjectAlternativeNames = Core.Nothing,
      tags = Core.Nothing,
      validationMethod = Core.Nothing
    }

-- | Fully qualified domain name (FQDN), such as www.example.com, that you want to secure with an ACM certificate. Use an asterisk (*) to create a wildcard certificate that protects several sites in the same domain. For example, *.example.com protects www.example.com, site.example.com, and images.example.com.
--
-- The first domain name you enter cannot exceed 64 octets, including periods. Each subsequent Subject Alternative Name (SAN), however, can be up to 253 octets in length.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcDomainName :: Lens.Lens' RequestCertificate Types.DomainName
rcDomainName = Lens.field @"domainName"
{-# DEPRECATED rcDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The Amazon Resource Name (ARN) of the private certificate authority (CA) that will be used to issue the certificate. If you do not provide an ARN and you are trying to request a private certificate, ACM will attempt to issue a public certificate. For more information about private CAs, see the <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaWelcome.html AWS Certificate Manager Private Certificate Authority (PCA)> user guide. The ARN must have the following form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCertificateAuthorityArn :: Lens.Lens' RequestCertificate (Core.Maybe Types.CertificateAuthorityArn)
rcCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# DEPRECATED rcCertificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead." #-}

-- | The domain name that you want ACM to use to send you emails so that you can validate domain ownership.
--
-- /Note:/ Consider using 'domainValidationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcDomainValidationOptions :: Lens.Lens' RequestCertificate (Core.Maybe (Core.NonEmpty Types.DomainValidationOption))
rcDomainValidationOptions = Lens.field @"domainValidationOptions"
{-# DEPRECATED rcDomainValidationOptions "Use generic-lens or generic-optics with 'domainValidationOptions' instead." #-}

-- | Customer chosen string that can be used to distinguish between calls to @RequestCertificate@ . Idempotency tokens time out after one hour. Therefore, if you call @RequestCertificate@ multiple times with the same idempotency token within one hour, ACM recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, ACM recognizes that you are requesting multiple certificates.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcIdempotencyToken :: Lens.Lens' RequestCertificate (Core.Maybe Types.IdempotencyToken)
rcIdempotencyToken = Lens.field @"idempotencyToken"
{-# DEPRECATED rcIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | Currently, you can use this parameter to specify whether to add the certificate to a certificate transparency log. Certificate transparency makes it possible to detect SSL/TLS certificates that have been mistakenly or maliciously issued. Certificates that have not been logged typically produce an error message in a browser. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-bestpractices.html#best-practices-transparency Opting Out of Certificate Transparency Logging> .
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcOptions :: Lens.Lens' RequestCertificate (Core.Maybe Types.CertificateOptions)
rcOptions = Lens.field @"options"
{-# DEPRECATED rcOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Additional FQDNs to be included in the Subject Alternative Name extension of the ACM certificate. For example, add the name www.example.net to a certificate for which the @DomainName@ field is www.example.com if users can reach your site by using either name. The maximum number of domain names that you can add to an ACM certificate is 100. However, the initial quota is 10 domain names. If you need more than 10 names, you must request a quota increase. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Quotas> .
--
-- The maximum length of a SAN DNS name is 253 octets. The name is made up of multiple labels separated by periods. No label can be longer than 63 octets. Consider the following examples:
--
--     * @(63 octets).(63 octets).(63 octets).(61 octets)@ is legal because the total length is 253 octets (63+1+63+1+63+1+61) and no label exceeds 63 octets.
--
--
--     * @(64 octets).(63 octets).(63 octets).(61 octets)@ is not legal because the total length exceeds 253 octets (64+1+63+1+63+1+61) and the first label exceeds 63 octets.
--
--
--     * @(63 octets).(63 octets).(63 octets).(62 octets)@ is not legal because the total length of the DNS name (63+1+63+1+63+1+62) exceeds 253 octets.
--
--
--
-- /Note:/ Consider using 'subjectAlternativeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcSubjectAlternativeNames :: Lens.Lens' RequestCertificate (Core.Maybe (Core.NonEmpty Types.DomainNameString))
rcSubjectAlternativeNames = Lens.field @"subjectAlternativeNames"
{-# DEPRECATED rcSubjectAlternativeNames "Use generic-lens or generic-optics with 'subjectAlternativeNames' instead." #-}

-- | One or more resource tags to associate with the certificate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcTags :: Lens.Lens' RequestCertificate (Core.Maybe (Core.NonEmpty Types.Tag))
rcTags = Lens.field @"tags"
{-# DEPRECATED rcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The method you want to use if you are requesting a public certificate to validate that you own or control domain. You can <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html validate with DNS> or <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-email.html validate with email> . We recommend that you use DNS validation.
--
-- /Note:/ Consider using 'validationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcValidationMethod :: Lens.Lens' RequestCertificate (Core.Maybe Types.ValidationMethod)
rcValidationMethod = Lens.field @"validationMethod"
{-# DEPRECATED rcValidationMethod "Use generic-lens or generic-optics with 'validationMethod' instead." #-}

instance Core.FromJSON RequestCertificate where
  toJSON RequestCertificate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainName" Core..= domainName),
            ("CertificateAuthorityArn" Core..=)
              Core.<$> certificateAuthorityArn,
            ("DomainValidationOptions" Core..=)
              Core.<$> domainValidationOptions,
            ("IdempotencyToken" Core..=) Core.<$> idempotencyToken,
            ("Options" Core..=) Core.<$> options,
            ("SubjectAlternativeNames" Core..=)
              Core.<$> subjectAlternativeNames,
            ("Tags" Core..=) Core.<$> tags,
            ("ValidationMethod" Core..=) Core.<$> validationMethod
          ]
      )

instance Core.AWSRequest RequestCertificate where
  type Rs RequestCertificate = RequestCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CertificateManager.RequestCertificate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RequestCertificateResponse'
            Core.<$> (x Core..:? "CertificateArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRequestCertificateResponse' smart constructor.
data RequestCertificateResponse = RequestCertificateResponse'
  { -- | String that contains the ARN of the issued certificate. This must be of the form:
    --
    -- @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
    certificateArn :: Core.Maybe Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequestCertificateResponse' value with any optional fields omitted.
mkRequestCertificateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RequestCertificateResponse
mkRequestCertificateResponse responseStatus =
  RequestCertificateResponse'
    { certificateArn = Core.Nothing,
      responseStatus
    }

-- | String that contains the ARN of the issued certificate. This must be of the form:
--
-- @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsCertificateArn :: Lens.Lens' RequestCertificateResponse (Core.Maybe Types.Arn)
rcrrsCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED rcrrsCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsResponseStatus :: Lens.Lens' RequestCertificateResponse Core.Int
rcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
