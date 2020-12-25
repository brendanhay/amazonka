{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListVerifiedEmailAddresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated. Use the @ListIdentities@ operation to list the email addresses and domains associated with your account.
module Network.AWS.SES.ListVerifiedEmailAddresses
  ( -- * Creating a request
    ListVerifiedEmailAddresses (..),
    mkListVerifiedEmailAddresses,

    -- * Destructuring the response
    ListVerifiedEmailAddressesResponse (..),
    mkListVerifiedEmailAddressesResponse,

    -- ** Response lenses
    lvearrsVerifiedEmailAddresses,
    lvearrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | /See:/ 'mkListVerifiedEmailAddresses' smart constructor.
data ListVerifiedEmailAddresses = ListVerifiedEmailAddresses'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVerifiedEmailAddresses' value with any optional fields omitted.
mkListVerifiedEmailAddresses ::
  ListVerifiedEmailAddresses
mkListVerifiedEmailAddresses = ListVerifiedEmailAddresses'

instance Core.AWSRequest ListVerifiedEmailAddresses where
  type
    Rs ListVerifiedEmailAddresses =
      ListVerifiedEmailAddressesResponse
  request x@_ =
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
            ( Core.pure ("Action", "ListVerifiedEmailAddresses")
                Core.<> (Core.pure ("Version", "2010-12-01"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListVerifiedEmailAddressesResult"
      ( \s h x ->
          ListVerifiedEmailAddressesResponse'
            Core.<$> ( x Core..@? "VerifiedEmailAddresses"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A list of email addresses that you have verified with Amazon SES under your AWS account.
--
-- /See:/ 'mkListVerifiedEmailAddressesResponse' smart constructor.
data ListVerifiedEmailAddressesResponse = ListVerifiedEmailAddressesResponse'
  { -- | A list of email addresses that have been verified.
    verifiedEmailAddresses :: Core.Maybe [Types.Address],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVerifiedEmailAddressesResponse' value with any optional fields omitted.
mkListVerifiedEmailAddressesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListVerifiedEmailAddressesResponse
mkListVerifiedEmailAddressesResponse responseStatus =
  ListVerifiedEmailAddressesResponse'
    { verifiedEmailAddresses =
        Core.Nothing,
      responseStatus
    }

-- | A list of email addresses that have been verified.
--
-- /Note:/ Consider using 'verifiedEmailAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvearrsVerifiedEmailAddresses :: Lens.Lens' ListVerifiedEmailAddressesResponse (Core.Maybe [Types.Address])
lvearrsVerifiedEmailAddresses = Lens.field @"verifiedEmailAddresses"
{-# DEPRECATED lvearrsVerifiedEmailAddresses "Use generic-lens or generic-optics with 'verifiedEmailAddresses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvearrsResponseStatus :: Lens.Lens' ListVerifiedEmailAddressesResponse Core.Int
lvearrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lvearrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
