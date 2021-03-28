{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about types that have been registered with CloudFormation.
module Network.AWS.CloudFormation.ListTypes
    (
    -- * Creating a request
      ListTypes (..)
    , mkListTypes
    -- ** Request lenses
    , ltDeprecatedStatus
    , ltMaxResults
    , ltNextToken
    , ltProvisioningType
    , ltType
    , ltVisibility

    -- * Destructuring the response
    , ListTypesResponse (..)
    , mkListTypesResponse
    -- ** Response lenses
    , ltrrsNextToken
    , ltrrsTypeSummaries
    , ltrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTypes' smart constructor.
data ListTypes = ListTypes'
  { deprecatedStatus :: Core.Maybe Types.DeprecatedStatus
    -- ^ The deprecation status of the types that you want to get summary information about.
--
-- Valid values include:
--
--     * @LIVE@ : The type is registered for use in CloudFormation operations.
--
--
--     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations. 
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
  , provisioningType :: Core.Maybe Types.ProvisioningType
    -- ^ The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted.
--
-- Valid values include:
--
--     * @FULLY_MUTABLE@ : The type includes an update handler to process updates to the type during stack update operations.
--
--
--     * @IMMUTABLE@ : The type does not include an update handler, so the type cannot be updated and must instead be replaced during stack update operations.
--
--
--     * @NON_PROVISIONABLE@ : The type does not include create, read, and delete handlers, and therefore cannot actually be provisioned.
--
--
  , type' :: Core.Maybe Types.RegistryType
    -- ^ The type of extension.
  , visibility :: Core.Maybe Types.Visibility
    -- ^ The scope at which the type is visible and usable in CloudFormation operations.
--
-- Valid values include:
--
--     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you create as @PRIVATE@ .
--
--
--     * @PUBLIC@ : The type is publically visible and usable within any Amazon account.
--
--
-- The default is @PRIVATE@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTypes' value with any optional fields omitted.
mkListTypes
    :: ListTypes
mkListTypes
  = ListTypes'{deprecatedStatus = Core.Nothing,
               maxResults = Core.Nothing, nextToken = Core.Nothing,
               provisioningType = Core.Nothing, type' = Core.Nothing,
               visibility = Core.Nothing}

-- | The deprecation status of the types that you want to get summary information about.
--
-- Valid values include:
--
--     * @LIVE@ : The type is registered for use in CloudFormation operations.
--
--
--     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations. 
--
--
--
-- /Note:/ Consider using 'deprecatedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltDeprecatedStatus :: Lens.Lens' ListTypes (Core.Maybe Types.DeprecatedStatus)
ltDeprecatedStatus = Lens.field @"deprecatedStatus"
{-# INLINEABLE ltDeprecatedStatus #-}
{-# DEPRECATED deprecatedStatus "Use generic-lens or generic-optics with 'deprecatedStatus' instead"  #-}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTypes (Core.Maybe Core.Natural)
ltMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTypes (Core.Maybe Types.NextToken)
ltNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted.
--
-- Valid values include:
--
--     * @FULLY_MUTABLE@ : The type includes an update handler to process updates to the type during stack update operations.
--
--
--     * @IMMUTABLE@ : The type does not include an update handler, so the type cannot be updated and must instead be replaced during stack update operations.
--
--
--     * @NON_PROVISIONABLE@ : The type does not include create, read, and delete handlers, and therefore cannot actually be provisioned.
--
--
--
-- /Note:/ Consider using 'provisioningType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltProvisioningType :: Lens.Lens' ListTypes (Core.Maybe Types.ProvisioningType)
ltProvisioningType = Lens.field @"provisioningType"
{-# INLINEABLE ltProvisioningType #-}
{-# DEPRECATED provisioningType "Use generic-lens or generic-optics with 'provisioningType' instead"  #-}

-- | The type of extension.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltType :: Lens.Lens' ListTypes (Core.Maybe Types.RegistryType)
ltType = Lens.field @"type'"
{-# INLINEABLE ltType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The scope at which the type is visible and usable in CloudFormation operations.
--
-- Valid values include:
--
--     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you create as @PRIVATE@ .
--
--
--     * @PUBLIC@ : The type is publically visible and usable within any Amazon account.
--
--
-- The default is @PRIVATE@ .
--
-- /Note:/ Consider using 'visibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltVisibility :: Lens.Lens' ListTypes (Core.Maybe Types.Visibility)
ltVisibility = Lens.field @"visibility"
{-# INLINEABLE ltVisibility #-}
{-# DEPRECATED visibility "Use generic-lens or generic-optics with 'visibility' instead"  #-}

instance Core.ToQuery ListTypes where
        toQuery ListTypes{..}
          = Core.toQueryPair "Action" ("ListTypes" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeprecatedStatus")
                deprecatedStatus
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ProvisioningType")
                provisioningType
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Type") type'
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Visibility") visibility

instance Core.ToHeaders ListTypes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListTypes where
        type Rs ListTypes = ListTypesResponse
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
          = Response.receiveXMLWrapper "ListTypesResult"
              (\ s h x ->
                 ListTypesResponse' Core.<$>
                   (x Core..@? "NextToken") Core.<*>
                     x Core..@? "TypeSummaries" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTypesResponse' smart constructor.
data ListTypesResponse = ListTypesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
  , typeSummaries :: Core.Maybe [Types.TypeSummary]
    -- ^ A list of @TypeSummary@ structures that contain information about the specified types.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListTypesResponse' value with any optional fields omitted.
mkListTypesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTypesResponse
mkListTypesResponse responseStatus
  = ListTypesResponse'{nextToken = Core.Nothing,
                       typeSummaries = Core.Nothing, responseStatus}

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTypesResponse (Core.Maybe Types.NextToken)
ltrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of @TypeSummary@ structures that contain information about the specified types.
--
-- /Note:/ Consider using 'typeSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTypeSummaries :: Lens.Lens' ListTypesResponse (Core.Maybe [Types.TypeSummary])
ltrrsTypeSummaries = Lens.field @"typeSummaries"
{-# INLINEABLE ltrrsTypeSummaries #-}
{-# DEPRECATED typeSummaries "Use generic-lens or generic-optics with 'typeSummaries' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTypesResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
