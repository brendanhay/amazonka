{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.ListWorkflowTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about workflow types in the specified domain. The results may be split into multiple pages that can be retrieved by making the call repeatedly.
--
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.SWF.ListWorkflowTypes
  ( -- * Creating a request
    ListWorkflowTypes (..),
    mkListWorkflowTypes,

    -- ** Request lenses
    lwtDomain,
    lwtRegistrationStatus,
    lwtMaximumPageSize,
    lwtName,
    lwtNextPageToken,
    lwtReverseOrder,

    -- * Destructuring the response
    ListWorkflowTypesResponse (..),
    mkListWorkflowTypesResponse,

    -- ** Response lenses
    lwtrrsTypeInfos,
    lwtrrsNextPageToken,
    lwtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkListWorkflowTypes' smart constructor.
data ListWorkflowTypes = ListWorkflowTypes'
  { -- | The name of the domain in which the workflow types have been registered.
    domain :: Types.Domain,
    -- | Specifies the registration status of the workflow types to list.
    registrationStatus :: Types.RegistrationStatus,
    -- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
    maximumPageSize :: Core.Maybe Core.Natural,
    -- | If specified, lists the workflow type with this name.
    name :: Core.Maybe Types.Name,
    -- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
    --
    -- The configured @maximumPageSize@ determines how many results can be returned in a single call.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | When set to @true@ , returns the results in reverse order. By default the results are returned in ascending alphabetical order of the @name@ of the workflow types.
    reverseOrder :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListWorkflowTypes' value with any optional fields omitted.
mkListWorkflowTypes ::
  -- | 'domain'
  Types.Domain ->
  -- | 'registrationStatus'
  Types.RegistrationStatus ->
  ListWorkflowTypes
mkListWorkflowTypes domain registrationStatus =
  ListWorkflowTypes'
    { domain,
      registrationStatus,
      maximumPageSize = Core.Nothing,
      name = Core.Nothing,
      nextPageToken = Core.Nothing,
      reverseOrder = Core.Nothing
    }

-- | The name of the domain in which the workflow types have been registered.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtDomain :: Lens.Lens' ListWorkflowTypes Types.Domain
lwtDomain = Lens.field @"domain"
{-# DEPRECATED lwtDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Specifies the registration status of the workflow types to list.
--
-- /Note:/ Consider using 'registrationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtRegistrationStatus :: Lens.Lens' ListWorkflowTypes Types.RegistrationStatus
lwtRegistrationStatus = Lens.field @"registrationStatus"
{-# DEPRECATED lwtRegistrationStatus "Use generic-lens or generic-optics with 'registrationStatus' instead." #-}

-- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
--
-- /Note:/ Consider using 'maximumPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtMaximumPageSize :: Lens.Lens' ListWorkflowTypes (Core.Maybe Core.Natural)
lwtMaximumPageSize = Lens.field @"maximumPageSize"
{-# DEPRECATED lwtMaximumPageSize "Use generic-lens or generic-optics with 'maximumPageSize' instead." #-}

-- | If specified, lists the workflow type with this name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtName :: Lens.Lens' ListWorkflowTypes (Core.Maybe Types.Name)
lwtName = Lens.field @"name"
{-# DEPRECATED lwtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtNextPageToken :: Lens.Lens' ListWorkflowTypes (Core.Maybe Types.NextPageToken)
lwtNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED lwtNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | When set to @true@ , returns the results in reverse order. By default the results are returned in ascending alphabetical order of the @name@ of the workflow types.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtReverseOrder :: Lens.Lens' ListWorkflowTypes (Core.Maybe Core.Bool)
lwtReverseOrder = Lens.field @"reverseOrder"
{-# DEPRECATED lwtReverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead." #-}

instance Core.FromJSON ListWorkflowTypes where
  toJSON ListWorkflowTypes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            Core.Just ("registrationStatus" Core..= registrationStatus),
            ("maximumPageSize" Core..=) Core.<$> maximumPageSize,
            ("name" Core..=) Core.<$> name,
            ("nextPageToken" Core..=) Core.<$> nextPageToken,
            ("reverseOrder" Core..=) Core.<$> reverseOrder
          ]
      )

instance Core.AWSRequest ListWorkflowTypes where
  type Rs ListWorkflowTypes = ListWorkflowTypesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "SimpleWorkflowService.ListWorkflowTypes")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkflowTypesResponse'
            Core.<$> (x Core..:? "typeInfos" Core..!= Core.mempty)
            Core.<*> (x Core..:? "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListWorkflowTypes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"typeInfos") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextPageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | Contains a paginated list of information structures about workflow types.
--
-- /See:/ 'mkListWorkflowTypesResponse' smart constructor.
data ListWorkflowTypesResponse = ListWorkflowTypesResponse'
  { -- | The list of workflow type information.
    typeInfos :: [Types.WorkflowTypeInfo],
    -- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
    --
    -- The configured @maximumPageSize@ determines how many results can be returned in a single call.
    nextPageToken :: Core.Maybe Types.PageToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListWorkflowTypesResponse' value with any optional fields omitted.
mkListWorkflowTypesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListWorkflowTypesResponse
mkListWorkflowTypesResponse responseStatus =
  ListWorkflowTypesResponse'
    { typeInfos = Core.mempty,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | The list of workflow type information.
--
-- /Note:/ Consider using 'typeInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtrrsTypeInfos :: Lens.Lens' ListWorkflowTypesResponse [Types.WorkflowTypeInfo]
lwtrrsTypeInfos = Lens.field @"typeInfos"
{-# DEPRECATED lwtrrsTypeInfos "Use generic-lens or generic-optics with 'typeInfos' instead." #-}

-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtrrsNextPageToken :: Lens.Lens' ListWorkflowTypesResponse (Core.Maybe Types.PageToken)
lwtrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED lwtrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwtrrsResponseStatus :: Lens.Lens' ListWorkflowTypesResponse Core.Int
lwtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lwtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
