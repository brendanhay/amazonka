{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rcIdempotencyToken,
    rcValidationMethod,
    rcSubjectAlternativeNames,
    rcOptions,
    rcDomainValidationOptions,
    rcCertificateAuthorityARN,
    rcTags,
    rcDomainName,

    -- * Destructuring the response
    RequestCertificateResponse (..),
    mkRequestCertificateResponse,

    -- ** Response lenses
    rcrsCertificateARN,
    rcrsResponseStatus,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRequestCertificate' smart constructor.
data RequestCertificate = RequestCertificate'
  { idempotencyToken ::
      Lude.Maybe Lude.Text,
    validationMethod :: Lude.Maybe ValidationMethod,
    subjectAlternativeNames ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    options :: Lude.Maybe CertificateOptions,
    domainValidationOptions ::
      Lude.Maybe (Lude.NonEmpty DomainValidationOption),
    certificateAuthorityARN :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
    domainName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RequestCertificate' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) of the private certificate authority (CA) that will be used to issue the certificate. If you do not provide an ARN and you are trying to request a private certificate, ACM will attempt to issue a public certificate. For more information about private CAs, see the <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaWelcome.html AWS Certificate Manager Private Certificate Authority (PCA)> user guide. The ARN must have the following form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@
-- * 'domainName' - Fully qualified domain name (FQDN), such as www.example.com, that you want to secure with an ACM certificate. Use an asterisk (*) to create a wildcard certificate that protects several sites in the same domain. For example, *.example.com protects www.example.com, site.example.com, and images.example.com.
--
-- The first domain name you enter cannot exceed 64 octets, including periods. Each subsequent Subject Alternative Name (SAN), however, can be up to 253 octets in length.
-- * 'domainValidationOptions' - The domain name that you want ACM to use to send you emails so that you can validate domain ownership.
-- * 'idempotencyToken' - Customer chosen string that can be used to distinguish between calls to @RequestCertificate@ . Idempotency tokens time out after one hour. Therefore, if you call @RequestCertificate@ multiple times with the same idempotency token within one hour, ACM recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, ACM recognizes that you are requesting multiple certificates.
-- * 'options' - Currently, you can use this parameter to specify whether to add the certificate to a certificate transparency log. Certificate transparency makes it possible to detect SSL/TLS certificates that have been mistakenly or maliciously issued. Certificates that have not been logged typically produce an error message in a browser. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-bestpractices.html#best-practices-transparency Opting Out of Certificate Transparency Logging> .
-- * 'subjectAlternativeNames' - Additional FQDNs to be included in the Subject Alternative Name extension of the ACM certificate. For example, add the name www.example.net to a certificate for which the @DomainName@ field is www.example.com if users can reach your site by using either name. The maximum number of domain names that you can add to an ACM certificate is 100. However, the initial quota is 10 domain names. If you need more than 10 names, you must request a quota increase. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Quotas> .
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
-- * 'tags' - One or more resource tags to associate with the certificate.
-- * 'validationMethod' - The method you want to use if you are requesting a public certificate to validate that you own or control domain. You can <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html validate with DNS> or <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-email.html validate with email> . We recommend that you use DNS validation.
mkRequestCertificate ::
  -- | 'domainName'
  Lude.Text ->
  RequestCertificate
mkRequestCertificate pDomainName_ =
  RequestCertificate'
    { idempotencyToken = Lude.Nothing,
      validationMethod = Lude.Nothing,
      subjectAlternativeNames = Lude.Nothing,
      options = Lude.Nothing,
      domainValidationOptions = Lude.Nothing,
      certificateAuthorityARN = Lude.Nothing,
      tags = Lude.Nothing,
      domainName = pDomainName_
    }

-- | Customer chosen string that can be used to distinguish between calls to @RequestCertificate@ . Idempotency tokens time out after one hour. Therefore, if you call @RequestCertificate@ multiple times with the same idempotency token within one hour, ACM recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, ACM recognizes that you are requesting multiple certificates.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcIdempotencyToken :: Lens.Lens' RequestCertificate (Lude.Maybe Lude.Text)
rcIdempotencyToken = Lens.lens (idempotencyToken :: RequestCertificate -> Lude.Maybe Lude.Text) (\s a -> s {idempotencyToken = a} :: RequestCertificate)
{-# DEPRECATED rcIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | The method you want to use if you are requesting a public certificate to validate that you own or control domain. You can <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html validate with DNS> or <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-email.html validate with email> . We recommend that you use DNS validation.
--
-- /Note:/ Consider using 'validationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcValidationMethod :: Lens.Lens' RequestCertificate (Lude.Maybe ValidationMethod)
rcValidationMethod = Lens.lens (validationMethod :: RequestCertificate -> Lude.Maybe ValidationMethod) (\s a -> s {validationMethod = a} :: RequestCertificate)
{-# DEPRECATED rcValidationMethod "Use generic-lens or generic-optics with 'validationMethod' instead." #-}

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
rcSubjectAlternativeNames :: Lens.Lens' RequestCertificate (Lude.Maybe (Lude.NonEmpty Lude.Text))
rcSubjectAlternativeNames = Lens.lens (subjectAlternativeNames :: RequestCertificate -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {subjectAlternativeNames = a} :: RequestCertificate)
{-# DEPRECATED rcSubjectAlternativeNames "Use generic-lens or generic-optics with 'subjectAlternativeNames' instead." #-}

-- | Currently, you can use this parameter to specify whether to add the certificate to a certificate transparency log. Certificate transparency makes it possible to detect SSL/TLS certificates that have been mistakenly or maliciously issued. Certificates that have not been logged typically produce an error message in a browser. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-bestpractices.html#best-practices-transparency Opting Out of Certificate Transparency Logging> .
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcOptions :: Lens.Lens' RequestCertificate (Lude.Maybe CertificateOptions)
rcOptions = Lens.lens (options :: RequestCertificate -> Lude.Maybe CertificateOptions) (\s a -> s {options = a} :: RequestCertificate)
{-# DEPRECATED rcOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The domain name that you want ACM to use to send you emails so that you can validate domain ownership.
--
-- /Note:/ Consider using 'domainValidationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcDomainValidationOptions :: Lens.Lens' RequestCertificate (Lude.Maybe (Lude.NonEmpty DomainValidationOption))
rcDomainValidationOptions = Lens.lens (domainValidationOptions :: RequestCertificate -> Lude.Maybe (Lude.NonEmpty DomainValidationOption)) (\s a -> s {domainValidationOptions = a} :: RequestCertificate)
{-# DEPRECATED rcDomainValidationOptions "Use generic-lens or generic-optics with 'domainValidationOptions' instead." #-}

-- | The Amazon Resource Name (ARN) of the private certificate authority (CA) that will be used to issue the certificate. If you do not provide an ARN and you are trying to request a private certificate, ACM will attempt to issue a public certificate. For more information about private CAs, see the <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaWelcome.html AWS Certificate Manager Private Certificate Authority (PCA)> user guide. The ARN must have the following form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCertificateAuthorityARN :: Lens.Lens' RequestCertificate (Lude.Maybe Lude.Text)
rcCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: RequestCertificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: RequestCertificate)
{-# DEPRECATED rcCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

-- | One or more resource tags to associate with the certificate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcTags :: Lens.Lens' RequestCertificate (Lude.Maybe (Lude.NonEmpty Tag))
rcTags = Lens.lens (tags :: RequestCertificate -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: RequestCertificate)
{-# DEPRECATED rcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Fully qualified domain name (FQDN), such as www.example.com, that you want to secure with an ACM certificate. Use an asterisk (*) to create a wildcard certificate that protects several sites in the same domain. For example, *.example.com protects www.example.com, site.example.com, and images.example.com.
--
-- The first domain name you enter cannot exceed 64 octets, including periods. Each subsequent Subject Alternative Name (SAN), however, can be up to 253 octets in length.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcDomainName :: Lens.Lens' RequestCertificate Lude.Text
rcDomainName = Lens.lens (domainName :: RequestCertificate -> Lude.Text) (\s a -> s {domainName = a} :: RequestCertificate)
{-# DEPRECATED rcDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest RequestCertificate where
  type Rs RequestCertificate = RequestCertificateResponse
  request = Req.postJSON certificateManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          RequestCertificateResponse'
            Lude.<$> (x Lude..?> "CertificateArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RequestCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CertificateManager.RequestCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RequestCertificate where
  toJSON RequestCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IdempotencyToken" Lude..=) Lude.<$> idempotencyToken,
            ("ValidationMethod" Lude..=) Lude.<$> validationMethod,
            ("SubjectAlternativeNames" Lude..=)
              Lude.<$> subjectAlternativeNames,
            ("Options" Lude..=) Lude.<$> options,
            ("DomainValidationOptions" Lude..=)
              Lude.<$> domainValidationOptions,
            ("CertificateAuthorityArn" Lude..=)
              Lude.<$> certificateAuthorityARN,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("DomainName" Lude..= domainName)
          ]
      )

instance Lude.ToPath RequestCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery RequestCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRequestCertificateResponse' smart constructor.
data RequestCertificateResponse = RequestCertificateResponse'
  { certificateARN ::
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

-- | Creates a value of 'RequestCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificateARN' - String that contains the ARN of the issued certificate. This must be of the form:
--
-- @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- * 'responseStatus' - The response status code.
mkRequestCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RequestCertificateResponse
mkRequestCertificateResponse pResponseStatus_ =
  RequestCertificateResponse'
    { certificateARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | String that contains the ARN of the issued certificate. This must be of the form:
--
-- @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsCertificateARN :: Lens.Lens' RequestCertificateResponse (Lude.Maybe Lude.Text)
rcrsCertificateARN = Lens.lens (certificateARN :: RequestCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: RequestCertificateResponse)
{-# DEPRECATED rcrsCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsResponseStatus :: Lens.Lens' RequestCertificateResponse Lude.Int
rcrsResponseStatus = Lens.lens (responseStatus :: RequestCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RequestCertificateResponse)
{-# DEPRECATED rcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
