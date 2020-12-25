{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.ListDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListDomains@ operation lists all domains associated with the Access Key ID. It returns domain names up to the limit set by <#MaxNumberOfDomains MaxNumberOfDomains> . A <#NextToken NextToken> is returned if there are more than @MaxNumberOfDomains@ domains. Calling @ListDomains@ successive times with the @NextToken@ provided by the operation returns up to @MaxNumberOfDomains@ more domain names with each successive operation call.
--
-- This operation returns paginated results.
module Network.AWS.SDB.ListDomains
  ( -- * Creating a request
    ListDomains (..),
    mkListDomains,

    -- ** Request lenses
    ldMaxNumberOfDomains,
    ldNextToken,

    -- * Destructuring the response
    ListDomainsResponse (..),
    mkListDomainsResponse,

    -- ** Response lenses
    ldrrsDomainNames,
    ldrrsNextToken,
    ldrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SDB.Types as Types

-- | /See:/ 'mkListDomains' smart constructor.
data ListDomains = ListDomains'
  { -- | The maximum number of domain names you want returned. The range is 1 to 100. The default setting is 100.
    maxNumberOfDomains :: Core.Maybe Core.Int,
    -- | A string informing Amazon SimpleDB where to start the next list of domain names.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomains' value with any optional fields omitted.
mkListDomains ::
  ListDomains
mkListDomains =
  ListDomains'
    { maxNumberOfDomains = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of domain names you want returned. The range is 1 to 100. The default setting is 100.
--
-- /Note:/ Consider using 'maxNumberOfDomains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxNumberOfDomains :: Lens.Lens' ListDomains (Core.Maybe Core.Int)
ldMaxNumberOfDomains = Lens.field @"maxNumberOfDomains"
{-# DEPRECATED ldMaxNumberOfDomains "Use generic-lens or generic-optics with 'maxNumberOfDomains' instead." #-}

-- | A string informing Amazon SimpleDB where to start the next list of domain names.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDomains (Core.Maybe Types.String)
ldNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListDomains where
  type Rs ListDomains = ListDomainsResponse
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
            ( Core.pure ("Action", "ListDomains")
                Core.<> (Core.pure ("Version", "2009-04-15"))
                Core.<> ( Core.toQueryValue "MaxNumberOfDomains"
                            Core.<$> maxNumberOfDomains
                        )
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListDomainsResult"
      ( \s h x ->
          ListDomainsResponse'
            Core.<$> (x Core..@? "DomainName")
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDomains where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"domainNames" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { -- | A list of domain names that match the expression.
    domainNames :: Core.Maybe [Types.String],
    -- | @MaxNumberOfDomains@
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomainsResponse' value with any optional fields omitted.
mkListDomainsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDomainsResponse
mkListDomainsResponse responseStatus =
  ListDomainsResponse'
    { domainNames = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of domain names that match the expression.
--
-- /Note:/ Consider using 'domainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDomainNames :: Lens.Lens' ListDomainsResponse (Core.Maybe [Types.String])
ldrrsDomainNames = Lens.field @"domainNames"
{-# DEPRECATED ldrrsDomainNames "Use generic-lens or generic-optics with 'domainNames' instead." #-}

-- | @MaxNumberOfDomains@
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsNextToken :: Lens.Lens' ListDomainsResponse (Core.Maybe Types.String)
ldrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDomainsResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
