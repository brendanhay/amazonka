{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListIdentities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list containing all of the identities (email addresses and domains) for your AWS account in the current AWS Region, regardless of verification status.
--
-- You can execute this operation no more than once per second.
--
-- This operation returns paginated results.
module Network.AWS.SES.ListIdentities
    (
    -- * Creating a request
      ListIdentities (..)
    , mkListIdentities
    -- ** Request lenses
    , liIdentityType
    , liMaxItems
    , liNextToken

    -- * Destructuring the response
    , ListIdentitiesResponse (..)
    , mkListIdentitiesResponse
    -- ** Response lenses
    , lirrsIdentities
    , lirrsNextToken
    , lirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to return a list of all identities (email addresses and domains) that you have attempted to verify under your AWS account, regardless of verification status.
--
-- /See:/ 'mkListIdentities' smart constructor.
data ListIdentities = ListIdentities'
  { identityType :: Core.Maybe Types.IdentityType
    -- ^ The type of the identities to list. Possible values are "EmailAddress" and "Domain". If this parameter is omitted, then all identities will be listed.
  , maxItems :: Core.Maybe Core.Int
    -- ^ The maximum number of identities per page. Possible values are 1-1000 inclusive.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use for pagination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIdentities' value with any optional fields omitted.
mkListIdentities
    :: ListIdentities
mkListIdentities
  = ListIdentities'{identityType = Core.Nothing,
                    maxItems = Core.Nothing, nextToken = Core.Nothing}

-- | The type of the identities to list. Possible values are "EmailAddress" and "Domain". If this parameter is omitted, then all identities will be listed.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liIdentityType :: Lens.Lens' ListIdentities (Core.Maybe Types.IdentityType)
liIdentityType = Lens.field @"identityType"
{-# INLINEABLE liIdentityType #-}
{-# DEPRECATED identityType "Use generic-lens or generic-optics with 'identityType' instead"  #-}

-- | The maximum number of identities per page. Possible values are 1-1000 inclusive.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxItems :: Lens.Lens' ListIdentities (Core.Maybe Core.Int)
liMaxItems = Lens.field @"maxItems"
{-# INLINEABLE liMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | The token to use for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListIdentities (Core.Maybe Types.NextToken)
liNextToken = Lens.field @"nextToken"
{-# INLINEABLE liNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListIdentities where
        toQuery ListIdentities{..}
          = Core.toQueryPair "Action" ("ListIdentities" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IdentityType")
                identityType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListIdentities where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListIdentities where
        type Rs ListIdentities = ListIdentitiesResponse
        toRequest x@Core.Request{..}
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
          = Response.receiveXMLWrapper "ListIdentitiesResult"
              (\ s h x ->
                 ListIdentitiesResponse' Core.<$>
                   (x Core..@ "Identities" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> x Core..@? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListIdentities where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"identities") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | A list of all identities that you have attempted to verify under your AWS account, regardless of verification status.
--
-- /See:/ 'mkListIdentitiesResponse' smart constructor.
data ListIdentitiesResponse = ListIdentitiesResponse'
  { identities :: [Types.Identity]
    -- ^ A list of identities.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token used for pagination.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIdentitiesResponse' value with any optional fields omitted.
mkListIdentitiesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListIdentitiesResponse
mkListIdentitiesResponse responseStatus
  = ListIdentitiesResponse'{identities = Core.mempty,
                            nextToken = Core.Nothing, responseStatus}

-- | A list of identities.
--
-- /Note:/ Consider using 'identities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsIdentities :: Lens.Lens' ListIdentitiesResponse [Types.Identity]
lirrsIdentities = Lens.field @"identities"
{-# INLINEABLE lirrsIdentities #-}
{-# DEPRECATED identities "Use generic-lens or generic-optics with 'identities' instead"  #-}

-- | The token used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsNextToken :: Lens.Lens' ListIdentitiesResponse (Core.Maybe Types.NextToken)
lirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsResponseStatus :: Lens.Lens' ListIdentitiesResponse Core.Int
lirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
