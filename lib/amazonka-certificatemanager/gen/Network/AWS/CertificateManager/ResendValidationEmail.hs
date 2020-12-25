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
    rveCertificateArn,
    rveDomain,
    rveValidationDomain,

    -- * Destructuring the response
    ResendValidationEmailResponse (..),
    mkResendValidationEmailResponse,
  )
where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResendValidationEmail' smart constructor.
data ResendValidationEmail = ResendValidationEmail'
  { -- | String that contains the ARN of the requested certificate. The certificate ARN is generated and returned by the 'RequestCertificate' action as soon as the request is made. By default, using this parameter causes email to be sent to all top-level domains you specified in the certificate request. The ARN must be of the form:
    --
    -- @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
    certificateArn :: Types.Arn,
    -- | The fully qualified domain name (FQDN) of the certificate that needs to be validated.
    domain :: Types.DomainNameString,
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
    validationDomain :: Types.DomainNameString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResendValidationEmail' value with any optional fields omitted.
mkResendValidationEmail ::
  -- | 'certificateArn'
  Types.Arn ->
  -- | 'domain'
  Types.DomainNameString ->
  -- | 'validationDomain'
  Types.DomainNameString ->
  ResendValidationEmail
mkResendValidationEmail certificateArn domain validationDomain =
  ResendValidationEmail' {certificateArn, domain, validationDomain}

-- | String that contains the ARN of the requested certificate. The certificate ARN is generated and returned by the 'RequestCertificate' action as soon as the request is made. By default, using this parameter causes email to be sent to all top-level domains you specified in the certificate request. The ARN must be of the form:
--
-- @arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rveCertificateArn :: Lens.Lens' ResendValidationEmail Types.Arn
rveCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED rveCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | The fully qualified domain name (FQDN) of the certificate that needs to be validated.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rveDomain :: Lens.Lens' ResendValidationEmail Types.DomainNameString
rveDomain = Lens.field @"domain"
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
rveValidationDomain :: Lens.Lens' ResendValidationEmail Types.DomainNameString
rveValidationDomain = Lens.field @"validationDomain"
{-# DEPRECATED rveValidationDomain "Use generic-lens or generic-optics with 'validationDomain' instead." #-}

instance Core.FromJSON ResendValidationEmail where
  toJSON ResendValidationEmail {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CertificateArn" Core..= certificateArn),
            Core.Just ("Domain" Core..= domain),
            Core.Just ("ValidationDomain" Core..= validationDomain)
          ]
      )

instance Core.AWSRequest ResendValidationEmail where
  type Rs ResendValidationEmail = ResendValidationEmailResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CertificateManager.ResendValidationEmail")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull ResendValidationEmailResponse'

-- | /See:/ 'mkResendValidationEmailResponse' smart constructor.
data ResendValidationEmailResponse = ResendValidationEmailResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResendValidationEmailResponse' value with any optional fields omitted.
mkResendValidationEmailResponse ::
  ResendValidationEmailResponse
mkResendValidationEmailResponse = ResendValidationEmailResponse'
