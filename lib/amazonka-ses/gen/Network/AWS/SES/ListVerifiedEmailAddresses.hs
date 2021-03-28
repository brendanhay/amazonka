{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListVerifiedEmailAddresses (..)
    , mkListVerifiedEmailAddresses

    -- * Destructuring the response
    , ListVerifiedEmailAddressesResponse (..)
    , mkListVerifiedEmailAddressesResponse
    -- ** Response lenses
    , lvearrsVerifiedEmailAddresses
    , lvearrsResponseStatus
    ) where

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
mkListVerifiedEmailAddresses
    :: ListVerifiedEmailAddresses
mkListVerifiedEmailAddresses = ListVerifiedEmailAddresses'

instance Core.ToQuery ListVerifiedEmailAddresses where
        toQuery ListVerifiedEmailAddresses{..}
          = Core.toQueryPair "Action"
              ("ListVerifiedEmailAddresses" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)

instance Core.ToHeaders ListVerifiedEmailAddresses where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListVerifiedEmailAddresses where
        type Rs ListVerifiedEmailAddresses =
             ListVerifiedEmailAddressesResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ListVerifiedEmailAddressesResult"
              (\ s h x ->
                 ListVerifiedEmailAddressesResponse' Core.<$>
                   (x Core..@? "VerifiedEmailAddresses" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A list of email addresses that you have verified with Amazon SES under your AWS account.
--
-- /See:/ 'mkListVerifiedEmailAddressesResponse' smart constructor.
data ListVerifiedEmailAddressesResponse = ListVerifiedEmailAddressesResponse'
  { verifiedEmailAddresses :: Core.Maybe [Types.Address]
    -- ^ A list of email addresses that have been verified.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVerifiedEmailAddressesResponse' value with any optional fields omitted.
mkListVerifiedEmailAddressesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListVerifiedEmailAddressesResponse
mkListVerifiedEmailAddressesResponse responseStatus
  = ListVerifiedEmailAddressesResponse'{verifiedEmailAddresses =
                                          Core.Nothing,
                                        responseStatus}

-- | A list of email addresses that have been verified.
--
-- /Note:/ Consider using 'verifiedEmailAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvearrsVerifiedEmailAddresses :: Lens.Lens' ListVerifiedEmailAddressesResponse (Core.Maybe [Types.Address])
lvearrsVerifiedEmailAddresses = Lens.field @"verifiedEmailAddresses"
{-# INLINEABLE lvearrsVerifiedEmailAddresses #-}
{-# DEPRECATED verifiedEmailAddresses "Use generic-lens or generic-optics with 'verifiedEmailAddresses' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvearrsResponseStatus :: Lens.Lens' ListVerifiedEmailAddressesResponse Core.Int
lvearrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lvearrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
