{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListTypeRegistrations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of registration tokens for the specified type(s).
module Network.AWS.CloudFormation.ListTypeRegistrations
  ( -- * Creating a request
    ListTypeRegistrations (..),
    mkListTypeRegistrations,

    -- ** Request lenses
    ltrMaxResults,
    ltrNextToken,
    ltrRegistrationStatusFilter,
    ltrType,
    ltrTypeArn,
    ltrTypeName,

    -- * Destructuring the response
    ListTypeRegistrationsResponse (..),
    mkListTypeRegistrationsResponse,

    -- ** Response lenses
    ltrrrsNextToken,
    ltrrrsRegistrationTokenList,
    ltrrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTypeRegistrations' smart constructor.
data ListTypeRegistrations = ListTypeRegistrations'
  { -- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The current status of the type registration request.
    --
    -- The default is @IN_PROGRESS@ .
    registrationStatusFilter :: Core.Maybe Types.RegistrationStatus,
    -- | The kind of type.
    --
    -- Currently the only valid value is @RESOURCE@ .
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    type' :: Core.Maybe Types.RegistryType,
    -- | The Amazon Resource Name (ARN) of the type.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    typeArn :: Core.Maybe Types.TypeArn,
    -- | The name of the type.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    typeName :: Core.Maybe Types.TypeName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTypeRegistrations' value with any optional fields omitted.
mkListTypeRegistrations ::
  ListTypeRegistrations
mkListTypeRegistrations =
  ListTypeRegistrations'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      registrationStatusFilter = Core.Nothing,
      type' = Core.Nothing,
      typeArn = Core.Nothing,
      typeName = Core.Nothing
    }

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrMaxResults :: Lens.Lens' ListTypeRegistrations (Core.Maybe Core.Natural)
ltrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrNextToken :: Lens.Lens' ListTypeRegistrations (Core.Maybe Types.NextToken)
ltrNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The current status of the type registration request.
--
-- The default is @IN_PROGRESS@ .
--
-- /Note:/ Consider using 'registrationStatusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrRegistrationStatusFilter :: Lens.Lens' ListTypeRegistrations (Core.Maybe Types.RegistrationStatus)
ltrRegistrationStatusFilter = Lens.field @"registrationStatusFilter"
{-# DEPRECATED ltrRegistrationStatusFilter "Use generic-lens or generic-optics with 'registrationStatusFilter' instead." #-}

-- | The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrType :: Lens.Lens' ListTypeRegistrations (Core.Maybe Types.RegistryType)
ltrType = Lens.field @"type'"
{-# DEPRECATED ltrType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The Amazon Resource Name (ARN) of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'typeArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrTypeArn :: Lens.Lens' ListTypeRegistrations (Core.Maybe Types.TypeArn)
ltrTypeArn = Lens.field @"typeArn"
{-# DEPRECATED ltrTypeArn "Use generic-lens or generic-optics with 'typeArn' instead." #-}

-- | The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrTypeName :: Lens.Lens' ListTypeRegistrations (Core.Maybe Types.TypeName)
ltrTypeName = Lens.field @"typeName"
{-# DEPRECATED ltrTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

instance Core.AWSRequest ListTypeRegistrations where
  type Rs ListTypeRegistrations = ListTypeRegistrationsResponse
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
            ( Core.pure ("Action", "ListTypeRegistrations")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryValue "RegistrationStatusFilter"
                            Core.<$> registrationStatusFilter
                        )
                Core.<> (Core.toQueryValue "Type" Core.<$> type')
                Core.<> (Core.toQueryValue "TypeArn" Core.<$> typeArn)
                Core.<> (Core.toQueryValue "TypeName" Core.<$> typeName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListTypeRegistrationsResult"
      ( \s h x ->
          ListTypeRegistrationsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "RegistrationTokenList"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListTypeRegistrationsResponse' smart constructor.
data ListTypeRegistrationsResponse = ListTypeRegistrationsResponse'
  { -- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of type registration tokens.
    --
    -- Use @'DescribeTypeRegistration' @ to return detailed information about a type registration request.
    registrationTokenList :: Core.Maybe [Types.RegistrationToken],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTypeRegistrationsResponse' value with any optional fields omitted.
mkListTypeRegistrationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTypeRegistrationsResponse
mkListTypeRegistrationsResponse responseStatus =
  ListTypeRegistrationsResponse'
    { nextToken = Core.Nothing,
      registrationTokenList = Core.Nothing,
      responseStatus
    }

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrrsNextToken :: Lens.Lens' ListTypeRegistrationsResponse (Core.Maybe Types.NextToken)
ltrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of type registration tokens.
--
-- Use @'DescribeTypeRegistration' @ to return detailed information about a type registration request.
--
-- /Note:/ Consider using 'registrationTokenList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrrsRegistrationTokenList :: Lens.Lens' ListTypeRegistrationsResponse (Core.Maybe [Types.RegistrationToken])
ltrrrsRegistrationTokenList = Lens.field @"registrationTokenList"
{-# DEPRECATED ltrrrsRegistrationTokenList "Use generic-lens or generic-optics with 'registrationTokenList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrrsResponseStatus :: Lens.Lens' ListTypeRegistrationsResponse Core.Int
ltrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
