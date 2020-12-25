{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListTypeVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about the versions of a type.
module Network.AWS.CloudFormation.ListTypeVersions
  ( -- * Creating a request
    ListTypeVersions (..),
    mkListTypeVersions,

    -- ** Request lenses
    ltvArn,
    ltvDeprecatedStatus,
    ltvMaxResults,
    ltvNextToken,
    ltvType,
    ltvTypeName,

    -- * Destructuring the response
    ListTypeVersionsResponse (..),
    mkListTypeVersionsResponse,

    -- ** Response lenses
    ltvrrsNextToken,
    ltvrrsTypeVersionSummaries,
    ltvrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTypeVersions' smart constructor.
data ListTypeVersions = ListTypeVersions'
  { -- | The Amazon Resource Name (ARN) of the type for which you want version summary information.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    arn :: Core.Maybe Types.PrivateTypeArn,
    -- | The deprecation status of the type versions that you want to get summary information about.
    --
    -- Valid values include:
    --
    --     * @LIVE@ : The type version is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.
    --
    --
    --     * @DEPRECATED@ : The type version has been deregistered and can no longer be used in CloudFormation operations.
    --
    --
    -- The default is @LIVE@ .
    deprecatedStatus :: Core.Maybe Types.DeprecatedStatus,
    -- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The kind of the type.
    --
    -- Currently the only valid value is @RESOURCE@ .
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    type' :: Core.Maybe Types.RegistryType,
    -- | The name of the type for which you want version summary information.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    typeName :: Core.Maybe Types.TypeName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTypeVersions' value with any optional fields omitted.
mkListTypeVersions ::
  ListTypeVersions
mkListTypeVersions =
  ListTypeVersions'
    { arn = Core.Nothing,
      deprecatedStatus = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      type' = Core.Nothing,
      typeName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the type for which you want version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvArn :: Lens.Lens' ListTypeVersions (Core.Maybe Types.PrivateTypeArn)
ltvArn = Lens.field @"arn"
{-# DEPRECATED ltvArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The deprecation status of the type versions that you want to get summary information about.
--
-- Valid values include:
--
--     * @LIVE@ : The type version is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.
--
--
--     * @DEPRECATED@ : The type version has been deregistered and can no longer be used in CloudFormation operations.
--
--
-- The default is @LIVE@ .
--
-- /Note:/ Consider using 'deprecatedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvDeprecatedStatus :: Lens.Lens' ListTypeVersions (Core.Maybe Types.DeprecatedStatus)
ltvDeprecatedStatus = Lens.field @"deprecatedStatus"
{-# DEPRECATED ltvDeprecatedStatus "Use generic-lens or generic-optics with 'deprecatedStatus' instead." #-}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvMaxResults :: Lens.Lens' ListTypeVersions (Core.Maybe Core.Natural)
ltvMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvNextToken :: Lens.Lens' ListTypeVersions (Core.Maybe Types.NextToken)
ltvNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The kind of the type.
--
-- Currently the only valid value is @RESOURCE@ .
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvType :: Lens.Lens' ListTypeVersions (Core.Maybe Types.RegistryType)
ltvType = Lens.field @"type'"
{-# DEPRECATED ltvType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The name of the type for which you want version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvTypeName :: Lens.Lens' ListTypeVersions (Core.Maybe Types.TypeName)
ltvTypeName = Lens.field @"typeName"
{-# DEPRECATED ltvTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

instance Core.AWSRequest ListTypeVersions where
  type Rs ListTypeVersions = ListTypeVersionsResponse
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
            ( Core.pure ("Action", "ListTypeVersions")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "Arn" Core.<$> arn)
                Core.<> (Core.toQueryValue "DeprecatedStatus" Core.<$> deprecatedStatus)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryValue "Type" Core.<$> type')
                Core.<> (Core.toQueryValue "TypeName" Core.<$> typeName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListTypeVersionsResult"
      ( \s h x ->
          ListTypeVersionsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "TypeVersionSummaries"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListTypeVersionsResponse' smart constructor.
data ListTypeVersionsResponse = ListTypeVersionsResponse'
  { -- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of @TypeVersionSummary@ structures that contain information about the specified type's versions.
    typeVersionSummaries :: Core.Maybe [Types.TypeVersionSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTypeVersionsResponse' value with any optional fields omitted.
mkListTypeVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTypeVersionsResponse
mkListTypeVersionsResponse responseStatus =
  ListTypeVersionsResponse'
    { nextToken = Core.Nothing,
      typeVersionSummaries = Core.Nothing,
      responseStatus
    }

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvrrsNextToken :: Lens.Lens' ListTypeVersionsResponse (Core.Maybe Types.NextToken)
ltvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @TypeVersionSummary@ structures that contain information about the specified type's versions.
--
-- /Note:/ Consider using 'typeVersionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvrrsTypeVersionSummaries :: Lens.Lens' ListTypeVersionsResponse (Core.Maybe [Types.TypeVersionSummary])
ltvrrsTypeVersionSummaries = Lens.field @"typeVersionSummaries"
{-# DEPRECATED ltvrrsTypeVersionSummaries "Use generic-lens or generic-optics with 'typeVersionSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvrrsResponseStatus :: Lens.Lens' ListTypeVersionsResponse Core.Int
ltvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
