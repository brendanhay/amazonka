{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetIdentityDkimAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of Easy DKIM signing for an entity. For domain name identities, this operation also returns the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES has successfully verified that these tokens have been published.
--
-- This operation takes a list of identities as input and returns the following information for each:
--
--     * Whether Easy DKIM signing is enabled or disabled.
--
--
--     * A set of DKIM tokens that represent the identity. If the identity is an email address, the tokens represent the domain of that address.
--
--
--     * Whether Amazon SES has successfully verified the DKIM tokens published in the domain's DNS. This information is only returned for domain name identities, not for email addresses.
--
--
-- This operation is throttled at one request per second and can only get DKIM attributes for up to 100 identities at a time.
-- For more information about creating DNS records using DKIM tokens, go to the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html Amazon SES Developer Guide> .
module Network.AWS.SES.GetIdentityDkimAttributes
  ( -- * Creating a request
    GetIdentityDkimAttributes (..),
    mkGetIdentityDkimAttributes,

    -- ** Request lenses
    gidaIdentities,

    -- * Destructuring the response
    GetIdentityDkimAttributesResponse (..),
    mkGetIdentityDkimAttributesResponse,

    -- ** Response lenses
    gidarrsDkimAttributes,
    gidarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request for the status of Amazon SES Easy DKIM signing for an identity. For domain identities, this request also returns the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES successfully verified that these tokens were published. For more information about Easy DKIM, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkGetIdentityDkimAttributes' smart constructor.
newtype GetIdentityDkimAttributes = GetIdentityDkimAttributes'
  { -- | A list of one or more verified identities - email addresses, domains, or both.
    identities :: [Types.Identity]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdentityDkimAttributes' value with any optional fields omitted.
mkGetIdentityDkimAttributes ::
  GetIdentityDkimAttributes
mkGetIdentityDkimAttributes =
  GetIdentityDkimAttributes' {identities = Core.mempty}

-- | A list of one or more verified identities - email addresses, domains, or both.
--
-- /Note:/ Consider using 'identities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gidaIdentities :: Lens.Lens' GetIdentityDkimAttributes [Types.Identity]
gidaIdentities = Lens.field @"identities"
{-# DEPRECATED gidaIdentities "Use generic-lens or generic-optics with 'identities' instead." #-}

instance Core.AWSRequest GetIdentityDkimAttributes where
  type
    Rs GetIdentityDkimAttributes =
      GetIdentityDkimAttributesResponse
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
            ( Core.pure ("Action", "GetIdentityDkimAttributes")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> ( Core.toQueryValue
                            "Identities"
                            (Core.toQueryList "member" identities)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetIdentityDkimAttributesResult"
      ( \s h x ->
          GetIdentityDkimAttributesResponse'
            Core.<$> ( x Core..@? "DkimAttributes" Core..@! Core.mempty
                         Core..<@> Core.parseXMLMap "entry" "key" "value"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the status of Amazon SES Easy DKIM signing for an identity. For domain identities, this response also contains the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES successfully verified that these tokens were published.
--
-- /See:/ 'mkGetIdentityDkimAttributesResponse' smart constructor.
data GetIdentityDkimAttributesResponse = GetIdentityDkimAttributesResponse'
  { -- | The DKIM attributes for an email address or a domain.
    dkimAttributes :: Core.HashMap Types.Identity Types.IdentityDkimAttributes,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdentityDkimAttributesResponse' value with any optional fields omitted.
mkGetIdentityDkimAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetIdentityDkimAttributesResponse
mkGetIdentityDkimAttributesResponse responseStatus =
  GetIdentityDkimAttributesResponse'
    { dkimAttributes = Core.mempty,
      responseStatus
    }

-- | The DKIM attributes for an email address or a domain.
--
-- /Note:/ Consider using 'dkimAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gidarrsDkimAttributes :: Lens.Lens' GetIdentityDkimAttributesResponse (Core.HashMap Types.Identity Types.IdentityDkimAttributes)
gidarrsDkimAttributes = Lens.field @"dkimAttributes"
{-# DEPRECATED gidarrsDkimAttributes "Use generic-lens or generic-optics with 'dkimAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gidarrsResponseStatus :: Lens.Lens' GetIdentityDkimAttributesResponse Core.Int
gidarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gidarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
