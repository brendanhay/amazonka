{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.ResendValidationEmail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resends the email that requests domain ownership validation. The domain owner or an authorized representative must approve the ACM certificate before it can be issued. The certificate can be approved by clicking a link in the mail to navigate to the Amazon certificate approval website and then clicking __I Approve__ . However, the validation email can be blocked by spam filters. Therefore, if you do not receive the original mail, you can request that the mail be resent within 72 hours of requesting the ACM certificate. If more than 72 hours have elapsed since your original request or since your last attempt to resend validation mail, you must request a new certificate. For more information about setting up your contact email addresses, see <https://docs.aws.amazon.com/acm/latest/userguide/setup-email.html Configure Email for your Domain> .
module Network.AWS.CertificateManager.ResendValidationEmail
  ( -- * Creating a request
    ResendValidationEmail (..),
    mkResendValidationEmail,

    -- ** Request lenses
    rveCertificateARN,
    rveDomain,
    rveValidationDomain,

    -- * Destructuring the response
    ResendValidationEmailResponse (..),
    mkResendValidationEmailResponse,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResendValidationEmail' smart constructor.
data ResendValidationEmail = ResendValidationEmail'
  { -- | String that contains the ARN of the requested certificate. The certificate ARN is generated and returned by the 'RequestCertificate' action as soon as the request is made. By default, using this parameter causes email to be sent to all top-level domains you specified in the certificate request. The ARN must be of the form:
    --
    -- @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
    certificateARN :: Lude.Text,
    -- | The fully qualified domain name (FQDN) of the certificate that needs to be validated.
    domain :: Lude.Text,
    -- | The base validation domain that will act as the suffix of the email addresses that are used to send the emails. This must be the same as the @Domain@ value or a superdomain of the @Domain@ value. For example, if you requested a certificate for @site.subdomain.example.com@ and specify a __ValidationDomain__ of @subdomain.example.com@ , ACM sends email to the domain registrant, technical contact, and administrative contact in WHOIS and the following five addresses:
    --
    --
    --     * admin@subdomain.example.com
    --
    --
    --     * administrator@subdomain.example.com
    --
    --
    --     * hostmaster@subdomain.example.com
    --
    --
    --     * postmaster@subdomain.example.com
    --
    --
    --     * webmaster@subdomain.example.com
    validationDomain :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResendValidationEmail' with the minimum fields required to make a request.
--
-- * 'certificateARN' - String that contains the ARN of the requested certificate. The certificate ARN is generated and returned by the 'RequestCertificate' action as soon as the request is made. By default, using this parameter causes email to be sent to all top-level domains you specified in the certificate request. The ARN must be of the form:
--
-- @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- * 'domain' - The fully qualified domain name (FQDN) of the certificate that needs to be validated.
-- * 'validationDomain' - The base validation domain that will act as the suffix of the email addresses that are used to send the emails. This must be the same as the @Domain@ value or a superdomain of the @Domain@ value. For example, if you requested a certificate for @site.subdomain.example.com@ and specify a __ValidationDomain__ of @subdomain.example.com@ , ACM sends email to the domain registrant, technical contact, and administrative contact in WHOIS and the following five addresses:
--
--
--     * admin@subdomain.example.com
--
--
--     * administrator@subdomain.example.com
--
--
--     * hostmaster@subdomain.example.com
--
--
--     * postmaster@subdomain.example.com
--
--
--     * webmaster@subdomain.example.com
mkResendValidationEmail ::
  -- | 'certificateARN'
  Lude.Text ->
  -- | 'domain'
  Lude.Text ->
  -- | 'validationDomain'
  Lude.Text ->
  ResendValidationEmail
mkResendValidationEmail
  pCertificateARN_
  pDomain_
  pValidationDomain_ =
    ResendValidationEmail'
      { certificateARN = pCertificateARN_,
        domain = pDomain_,
        validationDomain = pValidationDomain_
      }

-- | String that contains the ARN of the requested certificate. The certificate ARN is generated and returned by the 'RequestCertificate' action as soon as the request is made. By default, using this parameter causes email to be sent to all top-level domains you specified in the certificate request. The ARN must be of the form:
--
-- @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rveCertificateARN :: Lens.Lens' ResendValidationEmail Lude.Text
rveCertificateARN = Lens.lens (certificateARN :: ResendValidationEmail -> Lude.Text) (\s a -> s {certificateARN = a} :: ResendValidationEmail)
{-# DEPRECATED rveCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The fully qualified domain name (FQDN) of the certificate that needs to be validated.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rveDomain :: Lens.Lens' ResendValidationEmail Lude.Text
rveDomain = Lens.lens (domain :: ResendValidationEmail -> Lude.Text) (\s a -> s {domain = a} :: ResendValidationEmail)
{-# DEPRECATED rveDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The base validation domain that will act as the suffix of the email addresses that are used to send the emails. This must be the same as the @Domain@ value or a superdomain of the @Domain@ value. For example, if you requested a certificate for @site.subdomain.example.com@ and specify a __ValidationDomain__ of @subdomain.example.com@ , ACM sends email to the domain registrant, technical contact, and administrative contact in WHOIS and the following five addresses:
--
--
--     * admin@subdomain.example.com
--
--
--     * administrator@subdomain.example.com
--
--
--     * hostmaster@subdomain.example.com
--
--
--     * postmaster@subdomain.example.com
--
--
--     * webmaster@subdomain.example.com
--
--
--
-- /Note:/ Consider using 'validationDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rveValidationDomain :: Lens.Lens' ResendValidationEmail Lude.Text
rveValidationDomain = Lens.lens (validationDomain :: ResendValidationEmail -> Lude.Text) (\s a -> s {validationDomain = a} :: ResendValidationEmail)
{-# DEPRECATED rveValidationDomain "Use generic-lens or generic-optics with 'validationDomain' instead." #-}

instance Lude.AWSRequest ResendValidationEmail where
  type Rs ResendValidationEmail = ResendValidationEmailResponse
  request = Req.postJSON certificateManagerService
  response = Res.receiveNull ResendValidationEmailResponse'

instance Lude.ToHeaders ResendValidationEmail where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CertificateManager.ResendValidationEmail" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResendValidationEmail where
  toJSON ResendValidationEmail' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CertificateArn" Lude..= certificateARN),
            Lude.Just ("Domain" Lude..= domain),
            Lude.Just ("ValidationDomain" Lude..= validationDomain)
          ]
      )

instance Lude.ToPath ResendValidationEmail where
  toPath = Lude.const "/"

instance Lude.ToQuery ResendValidationEmail where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkResendValidationEmailResponse' smart constructor.
data ResendValidationEmailResponse = ResendValidationEmailResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResendValidationEmailResponse' with the minimum fields required to make a request.
mkResendValidationEmailResponse ::
  ResendValidationEmailResponse
mkResendValidationEmailResponse = ResendValidationEmailResponse'
