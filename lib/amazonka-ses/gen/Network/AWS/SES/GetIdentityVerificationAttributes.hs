{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetIdentityVerificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a list of identities (email addresses and/or domains), returns the verification status and (for domain identities) the verification token for each identity.
--
-- The verification status of an email address is "Pending" until the email address owner clicks the link within the verification email that Amazon SES sent to that address. If the email address owner clicks the link within 24 hours, the verification status of the email address changes to "Success". If the link is not clicked within 24 hours, the verification status changes to "Failed." In that case, if you still want to verify the email address, you must restart the verification process from the beginning.
-- For domain identities, the domain's verification status is "Pending" as Amazon SES searches for the required TXT record in the DNS settings of the domain. When Amazon SES detects the record, the domain's verification status changes to "Success". If Amazon SES is unable to detect the record within 72 hours, the domain's verification status changes to "Failed." In that case, if you still want to verify the domain, you must restart the verification process from the beginning.
-- This operation is throttled at one request per second and can only get verification attributes for up to 100 identities at a time.
module Network.AWS.SES.GetIdentityVerificationAttributes
  ( -- * Creating a request
    GetIdentityVerificationAttributes (..),
    mkGetIdentityVerificationAttributes,

    -- ** Request lenses
    givaIdentities,

    -- * Destructuring the response
    GetIdentityVerificationAttributesResponse (..),
    mkGetIdentityVerificationAttributesResponse,

    -- ** Response lenses
    givarrsVerificationAttributes,
    givarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to return the Amazon SES verification status of a list of identities. For domain identities, this request also returns the verification token. For information about verifying identities with Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkGetIdentityVerificationAttributes' smart constructor.
newtype GetIdentityVerificationAttributes = GetIdentityVerificationAttributes'
  { -- | A list of identities.
    identities :: [Types.Identity]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdentityVerificationAttributes' value with any optional fields omitted.
mkGetIdentityVerificationAttributes ::
  GetIdentityVerificationAttributes
mkGetIdentityVerificationAttributes =
  GetIdentityVerificationAttributes' {identities = Core.mempty}

-- | A list of identities.
--
-- /Note:/ Consider using 'identities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givaIdentities :: Lens.Lens' GetIdentityVerificationAttributes [Types.Identity]
givaIdentities = Lens.field @"identities"
{-# DEPRECATED givaIdentities "Use generic-lens or generic-optics with 'identities' instead." #-}

instance Core.AWSRequest GetIdentityVerificationAttributes where
  type
    Rs GetIdentityVerificationAttributes =
      GetIdentityVerificationAttributesResponse
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
            ( Core.pure ("Action", "GetIdentityVerificationAttributes")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> ( Core.toQueryValue
                            "Identities"
                            (Core.toQueryList "member" identities)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetIdentityVerificationAttributesResult"
      ( \s h x ->
          GetIdentityVerificationAttributesResponse'
            Core.<$> ( x Core..@? "VerificationAttributes" Core..@! Core.mempty
                         Core..<@> Core.parseXMLMap "entry" "key" "value"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The Amazon SES verification status of a list of identities. For domain identities, this response also contains the verification token.
--
-- /See:/ 'mkGetIdentityVerificationAttributesResponse' smart constructor.
data GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse'
  { -- | A map of Identities to IdentityVerificationAttributes objects.
    verificationAttributes :: Core.HashMap Types.Identity Types.IdentityVerificationAttributes,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdentityVerificationAttributesResponse' value with any optional fields omitted.
mkGetIdentityVerificationAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetIdentityVerificationAttributesResponse
mkGetIdentityVerificationAttributesResponse responseStatus =
  GetIdentityVerificationAttributesResponse'
    { verificationAttributes =
        Core.mempty,
      responseStatus
    }

-- | A map of Identities to IdentityVerificationAttributes objects.
--
-- /Note:/ Consider using 'verificationAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givarrsVerificationAttributes :: Lens.Lens' GetIdentityVerificationAttributesResponse (Core.HashMap Types.Identity Types.IdentityVerificationAttributes)
givarrsVerificationAttributes = Lens.field @"verificationAttributes"
{-# DEPRECATED givarrsVerificationAttributes "Use generic-lens or generic-optics with 'verificationAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givarrsResponseStatus :: Lens.Lens' GetIdentityVerificationAttributesResponse Core.Int
givarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED givarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
