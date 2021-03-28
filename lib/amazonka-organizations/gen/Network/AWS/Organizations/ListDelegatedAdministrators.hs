{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListDelegatedAdministrators
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AWS accounts that are designated as delegated administrators in this organization.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListDelegatedAdministrators
    (
    -- * Creating a request
      ListDelegatedAdministrators (..)
    , mkListDelegatedAdministrators
    -- ** Request lenses
    , ldaMaxResults
    , ldaNextToken
    , ldaServicePrincipal

    -- * Destructuring the response
    , ListDelegatedAdministratorsResponse (..)
    , mkListDelegatedAdministratorsResponse
    -- ** Response lenses
    , ldarrsDelegatedAdministrators
    , ldarrsNextToken
    , ldarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDelegatedAdministrators' smart constructor.
data ListDelegatedAdministrators = ListDelegatedAdministrators'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
  , servicePrincipal :: Core.Maybe Types.ServicePrincipal
    -- ^ Specifies a service principal name. If specified, then the operation lists the delegated administrators only for the specified service.
--
-- If you don't specify a service principal, the operation lists all delegated administrators for all services in your organization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDelegatedAdministrators' value with any optional fields omitted.
mkListDelegatedAdministrators
    :: ListDelegatedAdministrators
mkListDelegatedAdministrators
  = ListDelegatedAdministrators'{maxResults = Core.Nothing,
                                 nextToken = Core.Nothing, servicePrincipal = Core.Nothing}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldaMaxResults :: Lens.Lens' ListDelegatedAdministrators (Core.Maybe Core.Natural)
ldaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ldaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldaNextToken :: Lens.Lens' ListDelegatedAdministrators (Core.Maybe Types.NextToken)
ldaNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Specifies a service principal name. If specified, then the operation lists the delegated administrators only for the specified service.
--
-- If you don't specify a service principal, the operation lists all delegated administrators for all services in your organization.
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldaServicePrincipal :: Lens.Lens' ListDelegatedAdministrators (Core.Maybe Types.ServicePrincipal)
ldaServicePrincipal = Lens.field @"servicePrincipal"
{-# INLINEABLE ldaServicePrincipal #-}
{-# DEPRECATED servicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead"  #-}

instance Core.ToQuery ListDelegatedAdministrators where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListDelegatedAdministrators where
        toHeaders ListDelegatedAdministrators{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSOrganizationsV20161128.ListDelegatedAdministrators")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListDelegatedAdministrators where
        toJSON ListDelegatedAdministrators{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("ServicePrincipal" Core..=) Core.<$> servicePrincipal])

instance Core.AWSRequest ListDelegatedAdministrators where
        type Rs ListDelegatedAdministrators =
             ListDelegatedAdministratorsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDelegatedAdministratorsResponse' Core.<$>
                   (x Core..:? "DelegatedAdministrators") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDelegatedAdministrators where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"delegatedAdministrators" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListDelegatedAdministratorsResponse' smart constructor.
data ListDelegatedAdministratorsResponse = ListDelegatedAdministratorsResponse'
  { delegatedAdministrators :: Core.Maybe [Types.DelegatedAdministrator]
    -- ^ The list of delegated administrators in your organization.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDelegatedAdministratorsResponse' value with any optional fields omitted.
mkListDelegatedAdministratorsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDelegatedAdministratorsResponse
mkListDelegatedAdministratorsResponse responseStatus
  = ListDelegatedAdministratorsResponse'{delegatedAdministrators =
                                           Core.Nothing,
                                         nextToken = Core.Nothing, responseStatus}

-- | The list of delegated administrators in your organization.
--
-- /Note:/ Consider using 'delegatedAdministrators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldarrsDelegatedAdministrators :: Lens.Lens' ListDelegatedAdministratorsResponse (Core.Maybe [Types.DelegatedAdministrator])
ldarrsDelegatedAdministrators = Lens.field @"delegatedAdministrators"
{-# INLINEABLE ldarrsDelegatedAdministrators #-}
{-# DEPRECATED delegatedAdministrators "Use generic-lens or generic-optics with 'delegatedAdministrators' instead"  #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldarrsNextToken :: Lens.Lens' ListDelegatedAdministratorsResponse (Core.Maybe Types.NextToken)
ldarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldarrsResponseStatus :: Lens.Lens' ListDelegatedAdministratorsResponse Core.Int
ldarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
