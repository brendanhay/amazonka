{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.VerifyDomainIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a domain to the list of identities for your Amazon SES account in the current AWS Region and attempts to verify it. For more information about verifying domains, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Verifying Email Addresses and Domains> in the /Amazon SES Developer Guide./
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.VerifyDomainIdentity
  ( -- * Creating a request
    VerifyDomainIdentity (..),
    mkVerifyDomainIdentity,

    -- ** Request lenses
    vdiDomain,

    -- * Destructuring the response
    VerifyDomainIdentityResponse (..),
    mkVerifyDomainIdentityResponse,

    -- ** Response lenses
    vdirrsVerificationToken,
    vdirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to begin Amazon SES domain verification and to generate the TXT records that you must publish to the DNS server of your domain to complete the verification. For information about domain verification, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-domains.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkVerifyDomainIdentity' smart constructor.
newtype VerifyDomainIdentity = VerifyDomainIdentity'
  { -- | The domain to be verified.
    domain :: Types.Domain
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'VerifyDomainIdentity' value with any optional fields omitted.
mkVerifyDomainIdentity ::
  -- | 'domain'
  Types.Domain ->
  VerifyDomainIdentity
mkVerifyDomainIdentity domain = VerifyDomainIdentity' {domain}

-- | The domain to be verified.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdiDomain :: Lens.Lens' VerifyDomainIdentity Types.Domain
vdiDomain = Lens.field @"domain"
{-# DEPRECATED vdiDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

instance Core.AWSRequest VerifyDomainIdentity where
  type Rs VerifyDomainIdentity = VerifyDomainIdentityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "VerifyDomainIdentity")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "Domain" domain)
            )
      }
  response =
    Response.receiveXMLWrapper
      "VerifyDomainIdentityResult"
      ( \s h x ->
          VerifyDomainIdentityResponse'
            Core.<$> (x Core..@ "VerificationToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Returns a TXT record that you must publish to the DNS server of your domain to complete domain verification with Amazon SES.
--
-- /See:/ 'mkVerifyDomainIdentityResponse' smart constructor.
data VerifyDomainIdentityResponse = VerifyDomainIdentityResponse'
  { -- | A TXT record that you must place in the DNS settings of the domain to complete domain verification with Amazon SES.
    --
    -- As Amazon SES searches for the TXT record, the domain's verification status is "Pending". When Amazon SES detects the record, the domain's verification status changes to "Success". If Amazon SES is unable to detect the record within 72 hours, the domain's verification status changes to "Failed." In that case, if you still want to verify the domain, you must restart the verification process from the beginning.
    verificationToken :: Types.VerificationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VerifyDomainIdentityResponse' value with any optional fields omitted.
mkVerifyDomainIdentityResponse ::
  -- | 'verificationToken'
  Types.VerificationToken ->
  -- | 'responseStatus'
  Core.Int ->
  VerifyDomainIdentityResponse
mkVerifyDomainIdentityResponse verificationToken responseStatus =
  VerifyDomainIdentityResponse' {verificationToken, responseStatus}

-- | A TXT record that you must place in the DNS settings of the domain to complete domain verification with Amazon SES.
--
-- As Amazon SES searches for the TXT record, the domain's verification status is "Pending". When Amazon SES detects the record, the domain's verification status changes to "Success". If Amazon SES is unable to detect the record within 72 hours, the domain's verification status changes to "Failed." In that case, if you still want to verify the domain, you must restart the verification process from the beginning.
--
-- /Note:/ Consider using 'verificationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdirrsVerificationToken :: Lens.Lens' VerifyDomainIdentityResponse Types.VerificationToken
vdirrsVerificationToken = Lens.field @"verificationToken"
{-# DEPRECATED vdirrsVerificationToken "Use generic-lens or generic-optics with 'verificationToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdirrsResponseStatus :: Lens.Lens' VerifyDomainIdentityResponse Core.Int
vdirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED vdirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
